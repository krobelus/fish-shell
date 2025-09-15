// Minimal dependencies for test_foo() function
use std::collections::HashMap;
use std::os::unix::io::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;
use std::ffi::CStr;
use std::io::Write;

// External dependencies
extern crate errno;
extern crate libc;

// === threads module ===
static MAIN_THREAD_ID: OnceLock<usize> = OnceLock::new();

fn thread_id() -> usize {
    unsafe { libc::pthread_self() as usize }
}

pub fn init() {
    MAIN_THREAD_ID
        .set(thread_id())
        .expect("threads::init() must only be called once (at startup)!");
}

// === topic_monitor module ===
static mut s_principal: *const TopicMonitor = std::ptr::null();

#[derive(Default)]
pub struct TopicMonitor {
    status: AtomicU64,
}

unsafe impl Send for TopicMonitor {}
unsafe impl Sync for TopicMonitor {}

impl TopicMonitor {
    pub fn initialize() -> &'static Self {
        unsafe {
            if s_principal.is_null() {
                s_principal = Box::into_raw(Box::default());
            }
            &*s_principal
        }
    }
}

pub fn topic_monitor_init() {
    TopicMonitor::initialize();
}

// === wutil module (perror function) ===
pub fn perror(s: &str) {
    let e = errno::errno().0;
    let mut stderr = std::io::stderr().lock();
    if !s.is_empty() {
        let _ = write!(stderr, "{s}: ");
    }
    let slice = unsafe {
        let msg = libc::strerror(e);
        CStr::from_ptr(msg).to_bytes()
    };
    let _ = stderr.write_all(slice);
    let _ = stderr.write_all(b"\n");
}

// === parser module ===
#[derive(Default, Debug, Clone, Copy)]
pub enum CancelBehavior {
    #[default]
    Return,
    Clear,
}

pub struct Parser {
    pub variables: EnvStack,
    cancel_behavior: CancelBehavior,
}

impl Parser {
    pub fn new(variables: EnvStack, cancel_behavior: CancelBehavior) -> Parser {
        Self {
            variables,
            cancel_behavior,
        }
    }
}

// === env module ===
pub struct EnvStack {
    inner: Arc<Mutex<()>>,
}

impl EnvStack {
    pub fn new() -> EnvStack {
        EnvStack {
            inner: Arc::new(Mutex::new(())),
        }
    }
}

// === fd_readable_set module ===
pub enum Timeout {
    Duration(Duration),
    Forever,
}

impl Timeout {
    pub const ZERO: Timeout = Timeout::Duration(Duration::ZERO);
    
    #[allow(unused)]
    fn as_poll_msecs(&self) -> libc::c_int {
        match self {
            Timeout::Forever => -1 as libc::c_int,
            Timeout::Duration(duration) => {
                assert!(
                    duration.as_millis() < libc::c_int::MAX as _,
                    "Timeout too long but not forever!"
                );
                duration.as_millis() as libc::c_int
            }
        }
    }
}

pub struct FdReadableSet {
    pollfds_: Vec<libc::pollfd>,
}

impl FdReadableSet {
    pub fn new() -> FdReadableSet {
        FdReadableSet {
            pollfds_: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.pollfds_.clear();
    }

    #[inline]
    fn pollfd_get_fd(pollfd: &libc::pollfd) -> RawFd {
        pollfd.fd
    }

    pub fn add(&mut self, fd: RawFd) {
        if fd < 0 {
            return;
        }
        let pos = match self.pollfds_.binary_search_by_key(&fd, Self::pollfd_get_fd) {
            Ok(_) => return,
            Err(pos) => pos,
        };

        self.pollfds_.insert(
            pos,
            libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            },
        );
    }

    pub fn test(&self, fd: RawFd) -> bool {
        if let Ok(pos) = self.pollfds_.binary_search_by_key(&fd, Self::pollfd_get_fd) {
            let pollfd = &self.pollfds_[pos];
            debug_assert_eq!(pollfd.fd, fd);
            return pollfd.revents & (libc::POLLIN | libc::POLLHUP) != 0;
        }
        false
    }

    fn do_poll(fds: &mut [libc::pollfd], timeout: Timeout) -> libc::c_int {
        let count = fds.len();
        assert!(count <= libc::nfds_t::MAX as usize, "count too big");
        unsafe {
            libc::poll(
                fds.as_mut_ptr(),
                count as libc::nfds_t,
                timeout.as_poll_msecs(),
            )
        }
    }

    pub fn check_readable(&mut self, timeout: Timeout) -> libc::c_int {
        if self.pollfds_.is_empty() {
            return 0;
        }
        Self::do_poll(&mut self.pollfds_, timeout)
    }

    pub fn is_fd_readable(fd: RawFd, timeout: Timeout) -> bool {
        if fd < 0 {
            return false;
        }
        let mut pfd = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
        let ret = Self::do_poll(std::slice::from_mut(&mut pfd), timeout);
        ret > 0 && (pfd.revents & libc::POLLIN) != 0
    }

    pub fn poll_fd_readable(fd: RawFd) -> bool {
        Self::is_fd_readable(fd, Timeout::ZERO)
    }
}

// === io module ===
pub struct SeparatedBuffer {
    buffer_limit: usize,
    contents_size: usize,
    elements: Vec<Vec<u8>>,
    discard: bool,
}

impl SeparatedBuffer {
    pub fn new(limit: usize) -> Self {
        SeparatedBuffer {
            buffer_limit: limit,
            contents_size: 0,
            elements: vec![],
            discard: false,
        }
    }

    pub fn limit(&self) -> usize {
        self.buffer_limit
    }

    pub fn clear(&mut self) {
        self.contents_size = 0;
        self.elements.clear();
        self.discard = false;
    }
}

#[derive(Clone)]
pub struct IoBuffer(pub Arc<Mutex<SeparatedBuffer>>);

impl IoBuffer {
    pub fn new(buffer_limit: usize) -> Self {
        IoBuffer(Arc::new(Mutex::new(SeparatedBuffer::new(buffer_limit))))
    }

    pub fn read_once(fd: RawFd, buffer: &mut std::sync::MutexGuard<'_, SeparatedBuffer>) -> isize {
        assert!(fd >= 0, "Invalid fd");
        let mut bytes = [b'\0'; 4096 * 4];

        // We want to swallow EINTR only; in particular EAGAIN needs to be returned back to the caller.
        let amt = loop {
            let amt = unsafe {
                libc::read(
                    fd,
                    std::ptr::addr_of_mut!(bytes).cast(),
                    std::mem::size_of_val(&bytes),
                )
            };
            if amt < 0 && errno::errno().0 == libc::EINTR {
                continue;
            }
            break amt;
        };

        if amt > 0 {
            buffer.elements.push(bytes[..amt as usize].to_vec());
            buffer.contents_size += amt as usize;
        }
        amt
    }

    pub fn complete_and_take_buffer(&self, fd: AutoCloseFd) -> SeparatedBuffer {
        let mut locked_buff = self.0.lock().unwrap();
        while fd.is_valid() && IoBuffer::read_once(fd.as_raw_fd(), &mut locked_buff) > 0 {
            // pass
        }

        let mut result = SeparatedBuffer::new(locked_buff.limit());
        std::mem::swap(&mut result, &mut locked_buff);
        locked_buff.clear();
        result
    }
}

use std::sync::LazyLock;

static FD_MONITOR: LazyLock<FdMonitor> = LazyLock::new(|| FdMonitor::new());

pub fn fd_monitor() -> &'static FdMonitor {
    &FD_MONITOR
}

// === fds module ===
pub struct AutoCloseFd {
    fd_: RawFd,
}

impl AutoCloseFd {
    pub fn new(fd: RawFd) -> Self {
        AutoCloseFd { fd_: fd }
    }

    pub fn close(&mut self) {
        if self.fd_ != -1 {
            unsafe { libc::close(self.fd_) };
            self.fd_ = -1;
        }
    }

    pub fn fd(&self) -> RawFd {
        self.fd_
    }

    pub fn is_valid(&self) -> bool {
        self.fd_ >= 0
    }
}

impl AsRawFd for AutoCloseFd {
    fn as_raw_fd(&self) -> RawFd {
        self.fd()
    }
}

impl Drop for AutoCloseFd {
    fn drop(&mut self) {
        self.close()
    }
}

pub struct AutoClosePipes {
    pub read: OwnedFd,
    pub write: OwnedFd,
}

pub fn make_autoclose_pipes() -> Result<AutoClosePipes, std::io::Error> {
    let mut fds = [0i32; 2];
    let result = unsafe { libc::pipe(fds.as_mut_ptr()) };
    if result == -1 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(AutoClosePipes {
        read: unsafe { OwnedFd::from_raw_fd(fds[0]) },
        write: unsafe { OwnedFd::from_raw_fd(fds[1]) },
    })
}

pub fn make_fd_nonblocking(fd: RawFd) -> Result<(), std::io::Error> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL, 0) };
    let nonblocking = (flags & libc::O_NONBLOCK) == libc::O_NONBLOCK;
    if !nonblocking {
        match unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) } {
            -1 => return Err(std::io::Error::last_os_error()),
            _ => return Ok(()),
        };
    }
    Ok(())
}

// === fd_monitor module ===
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FdMonitorItemId(u64);

pub type Callback = Box<dyn Fn(&mut AutoCloseFd) + Send + Sync>;

pub struct FdMonitorItem {
    fd: AutoCloseFd,
    callback: Callback,
}

struct FdMonitorData {
    items: HashMap<FdMonitorItemId, FdMonitorItem>,
}

// FdEventSignaller for waking up background thread
pub struct FdEventSignaller {
    fd: OwnedFd,
    write: OwnedFd,
}

impl FdEventSignaller {
    pub fn new() -> Self {
        let pipes = make_autoclose_pipes().expect("Failed to create pipes");
        make_fd_nonblocking(pipes.read.as_raw_fd()).unwrap();
        make_fd_nonblocking(pipes.write.as_raw_fd()).unwrap();
        Self {
            fd: pipes.read,
            write: pipes.write,
        }
    }

    pub fn read_fd(&self) -> RawFd {
        self.fd.as_raw_fd()
    }

    pub fn try_consume(&self) -> bool {
        let mut buff = [0_u8; 1024];
        let mut ret;
        loop {
            ret = unsafe {
                libc::read(
                    self.read_fd(),
                    buff.as_mut_ptr() as *mut libc::c_void,
                    std::mem::size_of_val(&buff),
                )
            };
            if ret >= 0 || errno::errno().0 != libc::EINTR {
                break;
            }
        }
        if ret < 0 && ![libc::EAGAIN, libc::EWOULDBLOCK].contains(&errno::errno().0) {
            perror("read");
        }
        ret > 0
    }

    pub fn post(&self) {
        let write_fd = self.write_fd();
        if write_fd < 0 {
            return; // Invalid fd, don't try to write
        }
        
        let c = 1_u8;
        let ret = unsafe {
            libc::write(
                write_fd,
                &c as *const u8 as *const libc::c_void,
                1,
            )
        };
        if ret < 0 && ![libc::EAGAIN, libc::EWOULDBLOCK, libc::EBADF].contains(&errno::errno().0) {
            perror("write");
        }
    }

    fn write_fd(&self) -> RawFd {
        self.write.as_raw_fd()
    }
}

// Data shared between the `FdMonitor` instance and its associated `BackgroundFdMonitor`.
struct SharedData {
    items: HashMap<FdMonitorItemId, FdMonitorItem>,
    running: bool,
    terminate: bool,
}

pub struct FdMonitor {
    change_signaller: Arc<FdEventSignaller>,
    data: Arc<Mutex<SharedData>>,
    last_id: AtomicU64,
}

// The background half of the fd monitor, running on its own thread.
struct BackgroundFdMonitor {
    change_signaller: Arc<FdEventSignaller>,
    data: Arc<Mutex<SharedData>>,
}

impl FdMonitorItem {
    fn service(&mut self) {
        (self.callback)(&mut self.fd)
    }
}

impl BackgroundFdMonitor {
    fn run(self) {
        let mut fds = FdReadableSet::new();
        let mut item_ids: Vec<FdMonitorItemId> = Vec::new();

        loop {
            fds.clear();
            let change_signal_fd = self.change_signaller.read_fd();
            fds.add(change_signal_fd);

            let mut data = self.data.lock().expect("Mutex poisoned!");
            item_ids.clear();
            item_ids.reserve(data.items.len());
            for (item_id, item) in &data.items {
                let fd = item.fd.as_raw_fd();
                if fd >= 0 {
                    fds.add(fd);
                    item_ids.push(*item_id);
                }
            }

            item_ids.sort_unstable();

            let is_wait_lap = item_ids.is_empty();
            let timeout = if is_wait_lap {
                Some(Duration::from_millis(256))
            } else {
                None
            };

            drop(data);
            let ret = fds.check_readable(timeout.map(Timeout::Duration).unwrap_or(Timeout::Forever));
            if ret < 0 && !matches!(errno::errno().0, libc::EINTR | libc::EBADF) {
                perror("select");
                panic!();
            }

            data = self.data.lock().expect("Mutex poisoned!");

            for item_id in &item_ids {
                let Some(item) = data.items.get_mut(item_id) else {
                    continue;
                };
                if fds.test(item.fd.as_raw_fd()) {
                    item.service();
                }
            }

            let change_signalled = fds.test(change_signal_fd);
            if change_signalled || is_wait_lap {
                self.change_signaller.try_consume();

                if data.terminate || (is_wait_lap && data.items.is_empty() && !change_signalled) {
                    data.running = false;
                    break;
                }
            }
        }
    }
}

// Simple thread spawning function
fn spawn_thread<F>(f: F)
where
    F: FnOnce() + Send + 'static,
{
    std::thread::spawn(f);
}

impl FdMonitor {
    fn new() -> Self {
        Self {
            data: Arc::new(Mutex::new(SharedData {
                items: HashMap::new(),
                running: false,
                terminate: false,
            })),
            change_signaller: Arc::new(FdEventSignaller::new()),
            last_id: AtomicU64::new(0),
        }
    }

    pub fn add(&self, fd: AutoCloseFd, callback: Callback) -> FdMonitorItemId {
        assert!(fd.is_valid());

        let item_id = self.last_id.fetch_add(1, Ordering::Relaxed) + 1;
        let item_id = FdMonitorItemId(item_id);
        let item: FdMonitorItem = FdMonitorItem { fd, callback };
        let start_thread = {
            let mut data = self.data.lock().expect("Mutex poisoned!");
            let old_value = data.items.insert(item_id, item);
            assert!(old_value.is_none(), "Item ID {} already exists!", item_id.0);
            let already_started = data.running;
            data.running = true;
            !already_started
        };

        if start_thread {
            let background_monitor = BackgroundFdMonitor {
                data: Arc::clone(&self.data),
                change_signaller: Arc::clone(&self.change_signaller),
            };
            spawn_thread(move || {
                background_monitor.run();
            });
        }

        self.change_signaller.post();
        item_id
    }

    pub fn remove_item(&self, item_id: FdMonitorItemId) -> AutoCloseFd {
        assert!(item_id.0 > 0, "Invalid item id!");
        let mut data = self.data.lock().expect("Mutex poisoned!");
        let removed = data.items.remove(&item_id).expect("Item ID not found");
        drop(data);
        self.change_signaller.post();
        removed.fd
    }
}

impl Drop for FdMonitor {
    fn drop(&mut self) {
        // Set terminate flag and signal the background thread to exit
        if let Ok(mut data) = self.data.lock() {
            data.terminate = true;
        }
        self.change_signaller.post();
        
        // Wait briefly for the thread to exit
        std::thread::sleep(Duration::from_millis(50));
    }
}

// === The test_foo function ===
#[test]
fn test_foo() {
    init();
    topic_monitor_init();
    let _parser = Parser::new(EnvStack::new(), CancelBehavior::Clear);
    use std::os::fd::IntoRawFd;

    fn begin_filling(iobuffer: IoBuffer, fd: OwnedFd) -> FdMonitorItemId {
        let item_callback: Callback = Box::new(move |fd: &mut AutoCloseFd| {
            assert!(fd.as_raw_fd() >= 0, "Invalid fd");
            let mut buf = iobuffer.0.lock().unwrap();
            let ret = IoBuffer::read_once(fd.as_raw_fd(), &mut buf);
            if ret == 0
                || (ret < 0 && ![libc::EAGAIN, libc::EWOULDBLOCK].contains(&errno::errno().0))
            {
                fd.close();
            }
        });
        let fd = AutoCloseFd::new(fd.into_raw_fd());
        fd_monitor().add(fd, item_callback)
    }

    for _i in 0..100 {
        let pipes = make_autoclose_pipes().unwrap();
        make_fd_nonblocking(pipes.read.as_raw_fd()).unwrap();
        let buffer = IoBuffer::new(/*buffer_limit=*/ 2);
        let item_id = begin_filling(buffer.clone(), pipes.read);

        let fd = fd_monitor().remove_item(item_id);
        buffer.complete_and_take_buffer(fd);
    }
}
