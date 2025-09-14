// Minimal dependencies for test_foo() function
use std::collections::HashMap;
use std::os::unix::io::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FdMonitorItemId(u64);

pub type Callback = Box<dyn Fn(&mut AutoCloseFd) + Send + Sync>;

pub struct FdMonitorItem {
    fd: AutoCloseFd,
    callback: Callback,
}

pub struct FdMonitor {
    last_id: AtomicU64,
    data: Mutex<FdMonitorData>,
}

struct FdMonitorData {
    items: HashMap<FdMonitorItemId, FdMonitorItem>,
}

impl FdMonitor {
    fn new() -> Self {
        Self {
            last_id: AtomicU64::new(0),
            data: Mutex::new(FdMonitorData {
                items: HashMap::new(),
            }),
        }
    }

    pub fn add(&self, fd: AutoCloseFd, callback: Callback) -> FdMonitorItemId {
        assert!(fd.is_valid());

        let item_id = self.last_id.fetch_add(1, Ordering::Relaxed) + 1;
        let item_id = FdMonitorItemId(item_id);
        let item = FdMonitorItem { fd, callback };

        let mut data = self.data.lock().expect("Mutex poisoned!");
        data.items.insert(item_id, item);

        item_id
    }

    pub fn remove_item(&self, item_id: FdMonitorItemId) -> AutoCloseFd {
        assert!(item_id.0 > 0, "Item ID not found");
        let mut data = self.data.lock().expect("Mutex poisoned!");
        let removed = data.items.remove(&item_id).expect("Item ID not found");
        removed.fd
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
