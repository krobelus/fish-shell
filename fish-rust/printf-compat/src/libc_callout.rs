//! Support for libc callout to printf.

use super::output::WideWrite;
/// This is used to implement printf format specifiers like 'g' and 'a' that are too tricky in Rust.
use super::Flags;
use libc::{self, c_char};
use std::convert::TryInto;
use std::fmt;

// Don't inflict our large stack frame on callers.
#[inline(never)]
pub fn libc_sprintf_f64(
    w: &mut impl WideWrite,
    value: f64,
    flags: Flags,
    precision: u64,
    format_spec: char,
) -> fmt::Result {
    // The caller is responsible for applying a width, thousands seps, and localized decimal point.
    // TODO-RUST: should use snprintf_l, not snprintf!
    // This will return the "wrong" decimal separator in some cases. It's OK as long as LC_NUMERIC
    // matches what the caller expects, which fish tries to arrange.
    let prepend_plus = flags.contains(Flags::PREPEND_PLUS);
    let prepend_space = flags.contains(Flags::PREPEND_SPACE);
    let alt_mode = flags.contains(Flags::ALTERNATE_FORM);

    assert!(matches!(
        format_spec,
        'g' | 'a' | 'e' | 'f' | 'G' | 'A' | 'E' | 'F'
    ));

    // Helper to interpret the result of snprintf as a usize or error.
    let handle_ret =
        |x: libc::c_int| -> Result<usize, fmt::Error> { x.try_into().or(Err(fmt::Error)) };

    // Construct a nul-terminated string. Note this should be ASCII.
    let fmt = format!(
        "%{}{}{}.{}{}{}",
        if alt_mode { "#" } else { "" },
        if prepend_plus { "+" } else { "" },
        if prepend_space { " " } else { "" },
        precision,
        format_spec,
        '\0'
    );
    debug_assert!(fmt.is_ascii());
    let fmt_cstr = fmt.as_ptr() as *const c_char;

    // Try with a stack-allocated buffer.
    let mut buf = [0 as c_char; 128];
    let mut buf_ptr = buf.as_mut_ptr();
    let mut buff_len = buf.len();
    let ret = unsafe {
        libc::snprintf(
            buf.as_mut_ptr(),
            buff_len,
            fmt_cstr,
            value as libc::c_double,
        )
    };
    let mut ret_len = handle_ret(ret)?;

    // Maybe we need to try again with a larger buffer.
    let mut heap_buf = Vec::new();
    if ret_len >= buf.len() {
        heap_buf.resize(ret_len, 0);
        buff_len = heap_buf.len();
        let ret = unsafe { libc::snprintf(buf_ptr, buff_len, fmt_cstr, value as libc::c_double) };
        buf_ptr = heap_buf.as_mut_ptr();
        ret_len = handle_ret(ret)?;
    }
    assert!(
        ret_len < buff_len,
        "snprintf should not have returned a length >= the buffer size"
    );

    // Success.
    let s = unsafe {
        core::str::from_utf8(core::slice::from_raw_parts(buf_ptr as *const u8, ret_len))
            .expect("Returned string should be ASCII")
    };
    w.write_str(s)
}
