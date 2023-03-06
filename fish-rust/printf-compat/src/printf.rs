use crate::args::{Arg, ArgList};
use crate::locale::{Locale, C_LOCALE};
use crate::output::wide_write;
use crate::{wstr, WString};

/// The sprintf function entry points. Prefer to use the macros below.
pub fn sprintf_locale(fmt: &wstr, locale: &Locale, args: &[Arg]) -> WString {
    let mut s = WString::new();
    let mut arglist = ArgList::new(args);
    let res = crate::parser::format(fmt, &mut arglist, wide_write(&mut s, locale));
    if res.is_err() {
        panic!(
            "sprintf reported error \"{}\" with format string: {}",
            res.unwrap_err(),
            fmt
        );
    }
    if arglist.remaining() > 0 {
        panic!(
            "sprintf had {} unconsumed args for format string: {}",
            arglist.remaining(),
            fmt
        );
    }
    s
}

pub fn sprintf_c_locale(fmt: &wstr, args: &[Arg]) -> WString {
    sprintf_locale(fmt, &C_LOCALE, args)
}

/// The basic entry point. Accepts a format string as a &wstr, and a list of arguments.
#[macro_export]
macro_rules! sprintf {
    // Variant which allows a string literal.
    (
        $fmt:literal, // format string
        $($arg:expr),* // arguments
        $(,)? // optional trailing comma
    ) => {
        {
            use $crate::args::ToArg;
            $crate::printf::sprintf_c_locale(
                widestring::utf32str!($fmt),
                &[$($arg.to_arg()),*]
            )
        }
    };

    // Variant which allows a runtime format string, which must be of type &wstr.
    (
        $fmt:expr, // format string
        $($arg:expr),* // arguments
        $(,)? // optional trailing comma
    ) => {
        {
            use $crate::args::ToArg;
            $crate::printf::sprintf_c_locale(
                $fmt,
                &[$($arg.to_arg()),*]
            )
        }
    };
}

#[cfg(test)]
mod tests {
    use widestring::utf32str;

    // Test basic sprintf with both literals and wide strings.
    #[test]
    fn test_sprintf() {
        assert_eq!(sprintf!("Hello, %s!", "world"), "Hello, world!");
        assert_eq!(sprintf!(utf32str!("Hello, %ls!"), "world"), "Hello, world!");
        assert_eq!(
            sprintf!(utf32str!("Hello, %ls!"), utf32str!("world")),
            "Hello, world!"
        );
    }
}
