#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#define MAX_ITEMS 256

typedef struct {
    int fd;
    int valid;
} AutoCloseFd;

typedef struct {
    int fd;
    void *buffer;  // IoBuffer pointer
} FdMonitorItem;

typedef struct {
    FdMonitorItem items[MAX_ITEMS];
    int item_count;
    int running;
    int terminate;
    pthread_mutex_t mutex;
} SharedData;

typedef struct {
    SharedData *data;
    int change_signal_fd;
} BackgroundFdMonitor;

typedef struct {
    pthread_mutex_t mutex;
} IoBuffer;

// Global shared data
static SharedData g_shared_data = {0};
static pthread_t g_background_thread;
static int g_thread_started = 0;

void perror_custom(const char *s) {
    int e = errno;
    fprintf(stderr, "%s: %s\n", s, strerror(e));
}

int make_fd_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) return -1;

    int nonblocking = (flags & O_NONBLOCK) == O_NONBLOCK;
    if (!nonblocking) {
        if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
            return -1;
        }
    }
    return 0;
}

int make_autoclose_pipes(int *read_fd, int *write_fd) {
    int fds[2];
    if (pipe(fds) == -1) {
        return -1;
    }
    *read_fd = fds[0];
    *write_fd = fds[1];
    return 0;
}

IoBuffer *io_buffer_new() {
    IoBuffer *buf = malloc(sizeof(IoBuffer));
    if (!buf) return NULL;

    pthread_mutex_init(&buf->mutex, NULL);
    return buf;
}

void io_buffer_free(IoBuffer *buf) {
    if (buf) {
        pthread_mutex_destroy(&buf->mutex);
        free(buf);
    }
}

ssize_t io_buffer_read_once(int fd) {
    assert(fd >= 0);
    char bytes[4096];

    // We want to swallow EINTR only; in particular EAGAIN needs to be returned back to the caller.
    ssize_t amt;
    do {
        amt = read(fd, bytes, sizeof(bytes));
    } while (amt < 0 && errno == EINTR);

    return amt;
}

void auto_close_fd_close(AutoCloseFd *afd) {
    if (afd->fd >= 0) {
        close(afd->fd);
        afd->fd = -1;
        afd->valid = 0;
    }
}

void complete_and_take_buffer(IoBuffer *buffer, AutoCloseFd *fd) {
    pthread_mutex_lock(&buffer->mutex);
    while (fd->valid && io_buffer_read_once(fd->fd) > 0) {
        // pass
    }
    pthread_mutex_unlock(&buffer->mutex);
}

void *background_fd_monitor_run(void *arg) {
    BackgroundFdMonitor *monitor = (BackgroundFdMonitor *)arg;
    struct pollfd pollfds[MAX_ITEMS + 1];
    int item_fds[MAX_ITEMS];

    while (1) {
        // Clear poll array
        memset(pollfds, 0, sizeof(pollfds));
        int nfds = 0;

        // Add change signal fd
        pollfds[nfds].fd = monitor->change_signal_fd;
        pollfds[nfds].events = POLLIN;
        nfds++;

        // Lock and snapshot items
        pthread_mutex_lock(&monitor->data->mutex);
        int item_count = 0;
        for (int i = 0; i < monitor->data->item_count; i++) {
            int fd = monitor->data->items[i].fd;
            if (fd >= 0) {
                pollfds[nfds].fd = fd;
                pollfds[nfds].events = POLLIN;
                item_fds[item_count] = i;  // Map back to item index
                nfds++;
                item_count++;
            }
        }

        int is_wait_lap = (item_count == 0);
        int timeout_ms = is_wait_lap ? 256 : -1;  // -1 means wait forever
        pthread_mutex_unlock(&monitor->data->mutex);

        // Call poll() - this is where the Cygwin bug occurs
        int ret = poll(pollfds, nfds, timeout_ms);
        if (ret < 0 && errno != EINTR && errno != EBADF) {
            // This is where we expect to see the bug on Cygwin
            perror_custom("select");  // Using "select" to match original
            abort();
        }

        // Re-acquire lock and service items
        pthread_mutex_lock(&monitor->data->mutex);

        // Check if any monitored fds are ready
        for (int i = 1; i < nfds; i++) {  // Skip change signal fd at index 0
            if (pollfds[i].revents & POLLIN) {
                int item_idx = item_fds[i - 1];
                if (item_idx < monitor->data->item_count) {
                    FdMonitorItem *item = &monitor->data->items[item_idx];

                    // Simulate the callback - read from fd and close it
                    assert(item->fd >= 0);
                    IoBuffer *buf = (IoBuffer *)item->buffer;
                    ssize_t read_ret = io_buffer_read_once(item->fd);
                    if (read_ret == 0 ||
                        (read_ret < 0 && errno != EAGAIN && errno != EWOULDBLOCK)) {
                        close(item->fd);
                        item->fd = -1;
                    }
                }
            }
        }

        // Check for termination
        if (monitor->data->terminate || (is_wait_lap && monitor->data->item_count == 0)) {
            monitor->data->running = 0;
            pthread_mutex_unlock(&monitor->data->mutex);
            break;
        }

        pthread_mutex_unlock(&monitor->data->mutex);
    }

    return NULL;
}

int fd_monitor_add(int fd, IoBuffer *buffer) {
    pthread_mutex_lock(&g_shared_data.mutex);

    if (g_shared_data.item_count >= MAX_ITEMS) {
        pthread_mutex_unlock(&g_shared_data.mutex);
        return -1;
    }

    int item_id = g_shared_data.item_count;
    g_shared_data.items[item_id].fd = fd;
    g_shared_data.items[item_id].buffer = buffer;
    g_shared_data.item_count++;

    // Start background thread if not started
    if (!g_shared_data.running) {
        g_shared_data.running = 1;
        if (!g_thread_started) {
            BackgroundFdMonitor *monitor = malloc(sizeof(BackgroundFdMonitor));
            monitor->data = &g_shared_data;

            // Create dummy change signal fd (not used but needed for poll array)
            int dummy_pipes[2];
            if (make_autoclose_pipes(&dummy_pipes[0], &dummy_pipes[1]) == 0) {
                monitor->change_signal_fd = dummy_pipes[0];
            } else {
                monitor->change_signal_fd = -1;
            }

            pthread_create(&g_background_thread, NULL, background_fd_monitor_run, monitor);
            g_thread_started = 1;
        }
    }

    pthread_mutex_unlock(&g_shared_data.mutex);
    return item_id;
}

int fd_monitor_remove_item(int item_id) {
    pthread_mutex_lock(&g_shared_data.mutex);

    if (item_id < 0 || item_id >= g_shared_data.item_count) {
        pthread_mutex_unlock(&g_shared_data.mutex);
        return -1;
    }

    int fd = g_shared_data.items[item_id].fd;
    g_shared_data.items[item_id].fd = -1;
    g_shared_data.items[item_id].buffer = NULL;

    pthread_mutex_unlock(&g_shared_data.mutex);
    return fd;
}

int begin_filling(IoBuffer *buffer, int fd) { return fd_monitor_add(fd, buffer); }

void test_foo() {
    // Initialize
    pthread_mutex_init(&g_shared_data.mutex, NULL);

    for (int i = 0; i < 100; i++) {
        int read_fd, write_fd;
        if (make_autoclose_pipes(&read_fd, &write_fd) != 0) {
            perror_custom("pipe");
            continue;
        }

        if (make_fd_nonblocking(read_fd) != 0) {
            perror_custom("make_fd_nonblocking");
            close(read_fd);
            close(write_fd);
            continue;
        }

        IoBuffer *buffer = io_buffer_new();
        if (!buffer) {
            close(read_fd);
            close(write_fd);
            continue;
        }

        int item_id = begin_filling(buffer, read_fd);
        if (item_id < 0) {
            io_buffer_free(buffer);
            close(read_fd);
            close(write_fd);
            continue;
        }

        int removed_fd = fd_monitor_remove_item(item_id);
        if (removed_fd >= 0) {
            AutoCloseFd afd = {removed_fd, 1};
            complete_and_take_buffer(buffer, &afd);
            auto_close_fd_close(&afd);
        }

        io_buffer_free(buffer);
        close(write_fd);
    }

    // Clean up
    pthread_mutex_lock(&g_shared_data.mutex);
    g_shared_data.terminate = 1;
    pthread_mutex_unlock(&g_shared_data.mutex);

    if (g_thread_started) {
        pthread_join(g_background_thread, NULL);
    }

    pthread_mutex_destroy(&g_shared_data.mutex);
}

int main() {
    printf("Running test_foo C version...\n");
    test_foo();
    printf("Test completed.\n");
    return 0;
}
