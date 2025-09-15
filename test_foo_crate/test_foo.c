#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#define NUM_ITEMS 500

typedef struct {
    int fd;
} FdMonitorItem;

typedef struct {
    FdMonitorItem items[NUM_ITEMS];
    int item_count;
    pthread_mutex_t mutex;
} SharedData;

// Global shared data
static SharedData g_shared_data;
static pthread_t g_background_thread;
static int g_thread_started = 0;

static void io_buffer_read_once(int fd) {
    assert(fd >= 0);
    char data[1];
    ssize_t amt = read(fd, data, 1);
    assert(amt >= 0);  // no error
    assert(amt == 0);  // no write
}

static void *background_fd_monitor_run(void *_arg) {
    (void)_arg;
    SharedData *shared_data = &g_shared_data;
    struct pollfd pollfds[NUM_ITEMS];
    int item_fds[NUM_ITEMS];

    while (1) {
        memset(pollfds, 0, sizeof(pollfds));
        int nfds = 0;

        pthread_mutex_lock(&shared_data->mutex);
        int item_count = 0;
        for (int i = 0; i < shared_data->item_count; i++) {
            int fd = shared_data->items[i].fd;
            if (fd == -1) continue;
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
        pthread_mutex_unlock(&shared_data->mutex);

        // Call poll() - this is where the Cygwin bug occurs
        int ret = poll(pollfds, nfds, timeout_ms);
        if (ret == -1 && errno != EINTR && errno != EBADF) {
            perror("select");  // Using "select" to match original
            if (errno == 0) {
                // Possible Cygwin bug.
                _exit(1);
            }
        }

        // Re-acquire lock and service items
        pthread_mutex_lock(&shared_data->mutex);

        // Check if any monitored fds are ready
        for (int i = 0; i < nfds; i++) {
            if (pollfds[i].revents & POLLIN) {
                int item_idx = item_fds[i - 1];
                if (item_idx < shared_data->item_count) {
                    FdMonitorItem *item = &shared_data->items[item_idx];

                    // Simulate the callback - read from fd and close it
                    if (item->fd == -1) continue;  // TODO
                    assert(item->fd >= 0);
                    io_buffer_read_once(item->fd);
                    close(item->fd);
                    item->fd = -1;
                }
            }
        }

        // Check for termination
        if ((is_wait_lap && shared_data->item_count == 0)) {
            pthread_mutex_unlock(&shared_data->mutex);
            break;
        }

        pthread_mutex_unlock(&shared_data->mutex);
    }

    return NULL;
}

int fd_monitor_add(int fd) {
    pthread_mutex_lock(&g_shared_data.mutex);
    assert(g_shared_data.item_count < NUM_ITEMS);
    int item_id = g_shared_data.item_count;
    g_shared_data.items[item_id].fd = fd;
    g_shared_data.item_count++;
    pthread_mutex_unlock(&g_shared_data.mutex);

    // Start background thread if not started
    if (!g_thread_started) {
        pthread_create(&g_background_thread, NULL, background_fd_monitor_run, NULL);
        g_thread_started = 1;
    }

    return item_id;
}

int fd_monitor_remove_item(int item_id) {
    pthread_mutex_lock(&g_shared_data.mutex);

    if (item_id == -1 || item_id >= g_shared_data.item_count) {
        pthread_mutex_unlock(&g_shared_data.mutex);
        return -1;
    }

    int fd = g_shared_data.items[item_id].fd;
    g_shared_data.items[item_id].fd = -1;

    pthread_mutex_unlock(&g_shared_data.mutex);
    return fd;
}

int main() {
    pthread_mutex_init(&g_shared_data.mutex, NULL);

    int item_ids[NUM_ITEMS];
    for (int i = 0; i < NUM_ITEMS; i++) {
        int fds[2];
        int ok = pipe(fds) != -1;
        assert(ok);
        int read_fd = fds[0];
        close(fds[1]);
        int flags = fcntl(read_fd, F_GETFL, 0);
        assert(flags != -1);
        ok = fcntl(read_fd, F_SETFL, flags | O_NONBLOCK) != -1;
        assert(ok);
        item_ids[i] = fd_monitor_add(read_fd);
    }

    for (int i = 0; i < NUM_ITEMS; i++) {
        int item_id = item_ids[i];
        int removed_fd = fd_monitor_remove_item(item_id);
        if (removed_fd >= 0) {
            io_buffer_read_once(removed_fd);
            close(removed_fd);
        }
    }

    // Did not reproduce.
    _exit(0);
}
