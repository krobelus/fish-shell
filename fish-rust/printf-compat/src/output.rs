//! Various ways to output formatting data.

use core::fmt;

use super::{ArgList, Argument, DoubleFormat, Flags, Locale, SignedInt, Specifier, UnsignedInt};
use crate::libc_callout::libc_sprintf_f64;
use crate::{wstr, WString};

/// Saturate a u64 to a usize.
/// Note this is a no-op on 64 bit.
trait SaturateUSize {
    fn to_usize_sat(self) -> usize;
}

impl SaturateUSize for u64 {
    #[inline(always)]
    fn to_usize_sat(self) -> usize {
        self.min(usize::MAX as u64) as usize
    }
}

/// Adapter for implementing `fmt::Write` for `WideWrite`, avoiding orphan rule.
pub struct WideWriteAdapt<'a, T: ?Sized>(&'a mut T);
impl<'a, T: WideWrite + ?Sized> fmt::Write for WideWriteAdapt<'a, T> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_str(s)
    }
}

/// The trait for receiving printf output.
pub trait WideWrite {
    /// Write a wstr.
    fn write_wstr(&mut self, s: &wstr) -> fmt::Result;

    /// Write a str.
    fn write_str(&mut self, s: &str) -> fmt::Result;

    /// Allows using write! macro.
    fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        let mut adapt = WideWriteAdapt(self);
        fmt::write(&mut adapt, args)
    }
}

/// Wide strings implement [`WideWrite`].
impl WideWrite for WString {
    fn write_wstr(&mut self, s: &wstr) -> fmt::Result {
        self.push_utfstr(s);
        Ok(())
    }

    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

/// A Writer which counts how many chars are written.
struct WriteCounter(usize);

impl WideWrite for WriteCounter {
    fn write_wstr(&mut self, s: &wstr) -> fmt::Result {
        self.0 += s.as_char_slice().len();
        Ok(())
    }

    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0 += s.chars().count();
        Ok(())
    }
}

fn write_str(
    w: &mut impl WideWrite,
    flags: Flags,
    width: usize,
    precision: Option<u64>,
    b: &[char],
) -> fmt::Result {
    let string: String = b.iter().collect();
    let precision = precision.unwrap_or(string.len() as u64);
    if flags.contains(Flags::LEFT_ALIGN) {
        write!(
            w,
            "{:1$.prec$}",
            string,
            width,
            prec = precision.to_usize_sat()
        )
    } else {
        write!(
            w,
            "{:>1$.prec$}",
            string,
            width,
            prec = precision.to_usize_sat()
        )
    }
}

/// Given that we have formatted a value into a string in a locale-oblivious way, apply a locale to it.
/// That means inserting thousands separators if needed, and perhaps replacing the decimal point.
fn apply_locale(input: &mut WString, locale: &Locale, flags: Flags, fixup_decimal: bool) {
    // We expect to have (optionally) a leading +/-/space.
    // Note locales are not used for hex output.

    // Replace up to one decimal point if needed.
    if fixup_decimal && locale.decimal_point != '.' {
        for c in input.as_char_slice_mut().iter_mut() {
            if *c == '.' {
                *c = locale.decimal_point;
                break;
            }
        }
    }

    // Apply thousands separators if needed.
    if flags.contains(Flags::THOUSANDS_GROUPING) && locale.thousands_sep.is_some() {
        let thousands_sep = locale.thousands_sep.unwrap();

        // We need special handling for leading zeros, which may arise if the string was formatted with a precision.
        // Example: "%'.10d" 123456 should produce "000123,456" and NOT "000,123,456".
        let chars = input.as_char_slice();

        // Find the first sequence of digits.
        let mut digits_start = 0;
        while digits_start < chars.len() && !chars[digits_start].is_ascii_digit() {
            digits_start += 1;
        }

        // Find the first nonzero digit.
        let mut nonzeros_start = digits_start;
        while nonzeros_start < chars.len() && chars[nonzeros_start] == '0' {
            nonzeros_start += 1;
        }

        // Find the one past the last digit.
        let mut digits_end = nonzeros_start;
        while digits_end < chars.len() && chars[digits_start].is_ascii_digit() {
            digits_end += 1;
        }

        // Produce digits from our nonzero region using separators.
        let digits = &chars[nonzeros_start..digits_end];
        let mut digits_with_sep = WString::with_capacity(digits.len() + digits.len() / 3);

        // Go right to left; we construct in reverse.
        let mut group_iter = locale.digit_group_iter();
        let mut remaining_in_group = group_iter.next_group();
        for digit in digits.iter().rev() {
            assert!(digit.is_ascii_digit());
            if remaining_in_group == 0 {
                digits_with_sep.push(thousands_sep);
                remaining_in_group = group_iter.next_group();
            }
            remaining_in_group -= 1;
            digits_with_sep.push(*digit);
        }

        // Reverse the string.
        digits_with_sep.as_char_slice_mut().reverse();

        // Figure out how many leading zeros will have been replaced with separators.
        // This the smaller of the number of leading zeros, and the number of separators inserted.
        let zeros_to_eat = std::cmp::min(
            digits_with_sep.len() - digits.len(),
            nonzeros_start - digits_start,
        );

        input.replace_range(
            (nonzeros_start - zeros_to_eat)..digits_end,
            &digits_with_sep,
        );
    }
}

// Insert the given character into the string the given number of times at the given index.
fn insert_chars(input: &mut WString, idx: usize, c: char, count: usize) {
    // TODO: we can avoid a temporary if we use unsafe.
    let tmp = WString::from_chars(std::iter::repeat(c).take(count).collect::<Vec<_>>());
    input.insert_utfstr(idx, &tmp);
}

fn apply_numeric_padding(input: &mut WString, width: usize, flags: Flags) {
    // Given that we have have formatted a string with a numeric formatter and applied
    // locale bits, pad the string to at least the given width, respecting the flags.
    // There are three possible padding types:
    //   1. Pad on right with spaces.
    //   2. Pad on left with spaces.
    //   3. Pad on left with zeros, inserting them before any +/-/space sign.
    let padding_req = width.saturating_sub(input.as_char_slice().len());
    if padding_req == 0 {
        return;
    }

    // Handle left-or-right alignment with spaces.
    // Left-adjust takes precedence over zero-padding.
    if flags.contains(Flags::LEFT_ALIGN) {
        // Left align, insert at end.
        insert_chars(input, input.as_char_slice().len(), ' ', padding_req);
    } else if !flags.contains(Flags::PREPEND_ZERO) {
        // Right align, insert at start.
        insert_chars(input, 0, ' ', padding_req);
    } else {
        // Zero pad. Skip a leading +/-/space, and a leading 0x.
        let mut sign_idx = 0;
        let chars = input.as_char_slice();
        if sign_idx < chars.len() && matches!(chars[sign_idx], '+' | '-' | ' ') {
            sign_idx += 1;
        }
        if sign_idx + 1 < chars.len()
            && chars[sign_idx] == '0'
            && matches!(chars[sign_idx + 1], 'x' | 'X')
        {
            sign_idx += 2;
        }
        insert_chars(input, sign_idx, '0', padding_req);
    }
}

/// Format a non-finite value.
#[allow(clippy::collapsible_else_if)]
fn write_double_non_finite(
    w: &mut impl WideWrite,
    value: f64,
    flags: Flags,
    upper: bool,
) -> fmt::Result {
    assert!(!value.is_finite());
    // Do not pad with zeros as we are not finite, since 00000IN` makes no sense.
    // Do not place a leading + or ' ' if we are NaN, since +NaN makes no sense.
    // However +inf does make sense.
    if value == f64::INFINITY {
        if flags.contains(Flags::PREPEND_PLUS) {
            if upper {
                write!(w, "+INF")
            } else {
                write!(w, "+inf")
            }
        } else {
            if upper {
                write!(w, "INF")
            } else {
                write!(w, "inf")
            }
        }
    } else if value == f64::NEG_INFINITY {
        if upper {
            write!(w, "-INF")
        } else {
            write!(w, "-inf")
        }
    } else if value.is_nan() {
        if upper {
            write!(w, "NAN")
        } else {
            write!(w, "nan")
        }
    } else {
        unreachable!()
    }
}

/// Write a float in decimal form. This supports Normal and UpperNormal which are identical.
/// width and align is ignored - this will be fixed up later (necessary for locale support).
fn write_double_decimal(
    w: &mut impl WideWrite,
    value: f64,
    flags: Flags,
    precision: u64,
) -> fmt::Result {
    assert!(value.is_finite());
    let prec = precision.to_usize_sat();
    if flags.contains(Flags::PREPEND_PLUS) {
        write!(w, "{:+.*}", prec, value)
    } else if flags.contains(Flags::PREPEND_SPACE) && !value.is_sign_negative() {
        // note leading space
        write!(w, " {:.*}", prec, value)
    } else {
        write!(w, "{:.*}", prec, value)
    }
}

/// Write an unsigned integer value. This supports Hex/UpperHex/Octal/Uint.
/// width and align is ignored - this will be fixed up later (necessary for locale support).
fn write_uint(
    w: &mut impl WideWrite,
    val: UnsignedInt,
    flags: Flags,
    precision: u64,
    mode: char,
) -> fmt::Result {
    assert!(matches!(mode, 'x' | 'X' | 'o' | 'u'));
    // Note that the precision is re-interpreted as a width.
    let width = precision.to_usize_sat();
    // PREPEND_PLUS and PREPEND_SPACE are ignored for unsigned values.
    if flags.contains(Flags::ALTERNATE_FORM) {
        match mode {
            'x' => write!(w, "{:#0width$x}", val, width = width),
            // Need temporary storage because Rust emits 0x and not 0X.
            'X' => {
                let mut tmp = WString::new();
                write!(tmp, "{:#0width$x}", val, width = width)?;
                for c in tmp.as_char_slice_mut() {
                    if *c == 'x' {
                        *c = 'X';
                        break;
                    }
                }
                w.write_wstr(&tmp)
            }
            'o' => {
                // Need to remove the 'o' that Rust outputs.
                let mut tmp = WString::new();
                write!(tmp, "{:#0width$o}", val, width = width + 1)?;
                if let Some(idx) = tmp.as_char_slice().iter().position(|c| *c == 'o') {
                    tmp.remove(idx);
                }
                w.write_wstr(&tmp)
            }
            'u' => write!(w, "{:#0width$}", val, width = width),
            _ => unreachable!(),
        }
    } else {
        match mode {
            'x' => write!(w, "{:0width$x}", val, width = width),
            'X' => write!(w, "{:0width$X}", val, width = width),
            'o' => write!(w, "{:0width$o}", val, width = width),
            'u' => write!(w, "{:0width$}", val, width = width),
            _ => unreachable!(),
        }
    }
}

/// Write a signed integer value. This supports the 'd' conversion.
fn write_sint(
    w: &mut impl WideWrite,
    val: SignedInt,
    flags: Flags,
    precision: u64,
    mode: char,
) -> fmt::Result {
    assert!(matches!(mode, 'd'));
    // Note that the precision is re-interpreted as a width.
    let width = precision.to_usize_sat();
    let width_1 = width.saturating_sub(1);
    let negative = val.is_sign_negative();
    // PREPEND_PLUS and PREPEND_SPACE are respected for signed values.
    match mode {
        'd' => {
            if flags.contains(Flags::PREPEND_PLUS) && !negative {
                write!(w, "+{:0width$}", val, width = width_1)
            } else if flags.contains(Flags::PREPEND_SPACE) && !negative {
                write!(w, " {:0width$}", val, width = width_1)
            } else {
                write!(w, "{:0width$}", val, width = width)
            }
        }
        _ => unreachable!(),
    }
}

/// Write an f64. This supports 'f', 'F', 'g', 'G', 'e', 'E', 'a', and 'A'.
fn write_double(
    w: &mut impl WideWrite,
    value: f64,
    flags: Flags,
    precision: u64,
    format: DoubleFormat,
) -> fmt::Result {
    if !value.is_finite() {
        // Our double is +/- inf, or nan. All values are formatted the same.
        return write_double_non_finite(w, value, flags, format.is_upper());
    }
    match format {
        DoubleFormat::Normal | DoubleFormat::UpperNormal => {
            write_double_decimal(w, value, flags, precision)
        }

        // The other floats we call out to the real printf, as implementing these in Rust is too difficult.
        DoubleFormat::Hex
        | DoubleFormat::UpperHex
        | DoubleFormat::Scientific
        | DoubleFormat::UpperScientific
        | DoubleFormat::Auto
        | DoubleFormat::UpperAuto => {
            libc_sprintf_f64(w, value, flags, precision, format.to_spec_char())
        }
    }
}

/// Write a single argument to the writer, respecting the given locale.
fn write_1_arg_locale(mut arg: Argument, locale: &Locale, w: &mut impl WideWrite) -> fmt::Result {
    let mut storage_obj = WString::new();
    let storage = &mut storage_obj;
    arg = munge_arg(arg);
    let Argument {
        mut flags,
        width,
        precision,
        specifier,
    } = arg;
    let width = width.to_usize_sat();
    let mut do_locale = false;
    let mut fixup_decimal = false;
    let mut respect_zero_padding = true;
    // In general we follow the following:
    //  1. Write into our temporary storage.
    //  2. Apply locale bits (thousands sep, decimal separator).
    //  3. Apply width padding.
    //  4. Write that.
    // However we can write directly into the writer, for some simple cases; note the early returns below.
    match specifier {
        Specifier::Percent => return w.write_str("%"),
        Specifier::Literals(data) => {
            return write_str(w, flags, width, precision, data);
        }
        Specifier::String(data) => {
            return write_str(w, flags, width, precision, data.as_char_slice());
        }
        Specifier::Char(data) => {
            if flags.contains(Flags::LEFT_ALIGN) {
                return write!(w, "{:width$}", data, width = width);
            } else {
                return write!(w, "{:>width$}", data, width = width);
            }
        }
        Specifier::Pointer(data) => {
            if flags.contains(Flags::LEFT_ALIGN) {
                return write!(w, "{:<width$p}", data, width = width);
            } else if flags.contains(Flags::PREPEND_ZERO) {
                return write!(w, "{:0width$p}", data, width = width);
            } else {
                return write!(w, "{:width$p}", data, width = width);
            }
        }

        // The following numeric conversions may need width padding, but do not need localization.
        Specifier::Hex(data) | Specifier::UpperHex(data) | Specifier::Octal(data) => {
            write_uint(
                storage,
                data,
                flags,
                precision.unwrap_or(0),
                specifier.to_spec_char(),
            )?;
        }

        // The remaining may need width padding and also localization.
        Specifier::Uint(data) => {
            do_locale = true;
            write_uint(storage, data, flags, precision.unwrap_or(0), 'u')?;
        }
        Specifier::Int(data) => {
            do_locale = true;
            write_sint(storage, data, flags, precision.unwrap_or(0), 'd')?;
        }
        Specifier::Double { value, format } => {
            do_locale = value.is_finite();
            fixup_decimal = format.may_have_decimal();
            // do not respect zero padding for inf/nan; "+000INF" would be silly.
            respect_zero_padding = value.is_finite();
            write_double(storage, value, flags, precision.unwrap_or(6), format)?;
        }
    };
    if do_locale {
        apply_locale(storage, locale, flags, fixup_decimal);
    }
    if !respect_zero_padding {
        flags.remove(Flags::PREPEND_ZERO);
    }
    apply_numeric_padding(storage, width, flags);
    w.write_wstr(storage)
}

/// Apply some special cases to the given argument, to avoid replicating this logic.
fn munge_arg(mut arg: Argument) -> Argument {
    // "If a precision is given with a numeric conversion (d, i, o, u, x, and X), the 0 flag is ignored."
    if arg.specifier.is_int_numeric() && arg.precision.is_some() {
        arg.flags.remove(Flags::PREPEND_ZERO);
    }
    arg
}

/// Write to a struct that implements [`WideWrite`].
pub fn wide_write<'a>(
    w: &'a mut impl WideWrite,
    locale: &'a Locale,
) -> impl FnMut(Argument) -> fmt::Result + 'a {
    move |arg| write_1_arg_locale(arg, locale, w)
}

// Adapts `fmt::Write` to `WideWrite`.
struct FmtWrite<'a, T>(&'a mut T);

impl<'a, T> WideWrite for FmtWrite<'a, T>
where
    T: fmt::Write,
{
    /// Write a wstr.
    fn write_wstr(&mut self, s: &wstr) -> fmt::Result {
        self.0.write_str(&s.to_string())
    }

    /// Write a str.
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_str(s)
    }

    /// Allows using write! macro.
    fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        self.0.write_fmt(args)
    }
}

/// Write to a struct that implements [`fmt::Write`].
pub fn fmt_write<'a>(
    w: &'a mut impl fmt::Write,
    locale: &'a Locale,
) -> impl FnMut(Argument) -> fmt::Result + 'a {
    move |arg| write_1_arg_locale(arg, locale, &mut FmtWrite(w))
}

/// Returns an object that implements [`Display`][fmt::Display] for safely
/// printing formatting data. This is slightly less performant than using
/// [`fmt_write`], but may be the only option.
///
/// This shares the same caveats as [`fmt_write`].
pub fn display<'a, 'b, 'c>(
    format: &'a wstr,
    locale: &'b Locale,
    args: ArgList<'c>,
) -> ArgListDisplay<'a, 'b, 'c> {
    ArgListDisplay {
        format,
        locale,
        args,
    }
}

/// Helper struct created by [`display`] for safely printing `printf`-style
/// formatting with [`format!`] and `{}`. This can be used with anything that
/// uses [`format_args!`], such as [`println!`] or the `log` crate.
pub struct ArgListDisplay<'a, 'b, 'c> {
    format: &'a wstr,
    locale: &'b Locale,
    args: ArgList<'c>,
}

impl<'a, 'b, 'c> fmt::Display for ArgListDisplay<'a, 'b, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        super::format(
            self.format,
            &mut self.args.clone(),
            fmt_write(f, self.locale),
        )
    }
}
