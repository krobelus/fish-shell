#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#define MAX_ITEMS 500

typedef struct {
    int fd;
} FdMonitorItem;

typedef struct {
    FdMonitorItem items[MAX_ITEMS];
    int item_count;
    int running;
    pthread_mutex_t mutex;
} SharedData;

// Global shared data
static SharedData g_shared_data = {0};
static pthread_t g_background_thread;
static int g_thread_started = 0;

ssize_t io_buffer_read_once(int fd) {
    assert(fd >= 0);
    char bytes[4096];
    ssize_t amt = read(fd, bytes, sizeof(bytes));
    assert(amt >= 0);  // no error
    return amt;
}

void *background_fd_monitor_run(void *) {
    SharedData *shared_data = &g_shared_data;
    struct pollfd pollfds[MAX_ITEMS];
    int item_fds[MAX_ITEMS];

    while (1) {
        memset(pollfds, 0, sizeof(pollfds));
        int nfds = 0;

        pthread_mutex_lock(&shared_data->mutex);
        int item_count = 0;
        for (int i = 0; i < shared_data->item_count; i++) {
            int fd = shared_data->items[i].fd;
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
        if (ret < 0 && errno != EINTR && errno != EBADF) {
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
                    if (item->fd < 0) continue;  // TODO
                    assert(item->fd >= 0);
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
        if ((is_wait_lap && shared_data->item_count == 0)) {
            shared_data->running = 0;
            pthread_mutex_unlock(&shared_data->mutex);
            break;
        }

        pthread_mutex_unlock(&shared_data->mutex);
    }

    return NULL;
}

int fd_monitor_add(int fd) {
    pthread_mutex_lock(&g_shared_data.mutex);

    assert(g_shared_data.item_count < MAX_ITEMS);

    int item_id = g_shared_data.item_count;
    g_shared_data.items[item_id].fd = fd;
    g_shared_data.item_count++;

    // Start background thread if not started
    if (!g_shared_data.running) {
        g_shared_data.running = 1;
        if (!g_thread_started) {
            pthread_create(&g_background_thread, NULL, background_fd_monitor_run, NULL);
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

    pthread_mutex_unlock(&g_shared_data.mutex);
    return fd;
}

int main() {
    pthread_mutex_init(&g_shared_data.mutex, NULL);

    int item_ids[MAX_ITEMS];
    for (int i = 0; i < MAX_ITEMS; i++) {
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

    for (int i = 0; i < MAX_ITEMS; i++) {
        int item_id = item_ids[i];
        int removed_fd = fd_monitor_remove_item(item_id);
        if (removed_fd >= 0) {
            while (io_buffer_read_once(removed_fd) > 0);
            close(removed_fd);
        }
    }

    // Did not reproduce.
    _exit(0);
}
