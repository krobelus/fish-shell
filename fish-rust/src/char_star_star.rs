use libc::strlen;
use std::ffi::{c_char, CStr, CString};
use std::marker::PhantomData;
use std::ops::Deref;
use std::pin::Pin;
use std::ptr;

pub trait Nullable: Copy {
    fn is_null(self) -> bool;
}

impl<T: ?Sized> Nullable for *const T {
    fn is_null(self) -> bool {
        <*const T>::is_null(self)
    }
}

impl Nullable for c_char {
    fn is_null(self) -> bool {
        self == 0
    }
}

// The trait is unsafe because `as_ptr` must guarantee
// to return a pointer to a sequence terminated with an element
// for which `Nullable::is_null` is `true`.
// The pointer must not be null even for empty sequences:
// empty sequence must consist of the terminator.
pub unsafe trait NullTerminatedSequence {
    type Item: Nullable;

    fn as_ptr(&self) -> *const Self::Item;
    fn len_without_terminator(&self) -> usize;
    fn iter(&self) -> NullTerminatedIter<Self::Item> {
        NullTerminatedIter {
            current_ptr: self.as_ptr(),
            _phantom: PhantomData,
        }
    }
}

pub struct NullTerminatedIter<'a, T: Nullable> {
    current_ptr: *const T,
    _phantom: PhantomData<&'a [T]>,
}

impl<T> Iterator for NullTerminatedIter<'_, T>
where
    T: Nullable,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: by construction. We are only constructed in `NullTerminatedSequence::iter()`
        // with `current_ptr` set to `NullTerminatedSequence::as_ptr()`, which is guaranteed to be
        // "good" by the unsafety of the `NullTerminatedSequence` trait.
        // "Good" means not a nullptr and is pointing to a sequence
        // terminated by an element for which `Nullable::is_null` is `true`.
        // And we never increment the pointer past that element.
        let current = unsafe { *self.current_ptr };
        if current.is_null() {
            None
        } else {
            // SAFETY: see above
            self.current_ptr = unsafe { self.current_ptr.add(1) };
            Some(current)
        }
    }
}

// SAFETY: CStr is NUL-Terminated and its pointer is not null
unsafe impl NullTerminatedSequence for CStr {
    type Item = c_char;

    fn as_ptr(&self) -> *const Self::Item {
        CStr::as_ptr(self)
    }

    fn len_without_terminator(&self) -> usize {
        // SAFETY: CStr is NUL-terminated
        unsafe { strlen(self.as_ptr()) }
    }
}

#[cfg(test)]
mod cstr_tests {
    use super::{c_char, CStr, CString, NullTerminatedSequence};

    #[test]
    fn cstr_as_ptr() {
        let c_str = CString::new("foo").unwrap().as_c_str();
    }
    #[test]
    fn cstr_len_without_terminator() {
        assert_eq!(<&CStr as Default>::default().len_without_terminator(), 0);

        fn test_from_bytes(bytes: &[u8], expected_len: usize) {
            let actual_len =
                unsafe { CStr::from_bytes_with_nul_unchecked(bytes) }.len_without_terminator();
            assert_eq!(expected_len, actual_len);
        }

        test_from_bytes("I'm a fish!\0".as_bytes(), 11);
        // This one is actually nasty, because neither Rust's CStr
        // nor our implementation of NullTerminatedSequence for it
        // guarantee anything about CStr's with '\0' in the middle.
        test_from_bytes("O\0ps".as_bytes(), 1);
        test_from_bytes("\0".as_bytes(), 0);
    }

    #[test]
    fn cstr_iter() {
        assert_eq!(<&CStr as Default>::default().len_without_terminator(), 0);

        fn test_from_bytes(bytes: &[u8], expected: impl AsRef<[u8]>) {
            let c_str = unsafe { CStr::from_bytes_with_nul_unchecked(bytes) };
            let actual: Vec<_> = NullTerminatedSequence::iter(c_str)
                .map(|c: c_char| c as u8)
                .collect();
            assert_eq!(expected.as_ref(), &actual);
        }

        test_from_bytes("I'm a fish!\0".as_bytes(), "I'm a fish!".as_bytes());
        // This one is actually nasty, because neither Rust's CStr
        // nor our implementation of NullTerminatedSequence for it
        // guarantee anything about CStr's with '\0' in the middle.
        test_from_bytes("O\0ps".as_bytes(), "O".as_bytes());
        test_from_bytes("\0".as_bytes(), []);
    }
}

/// This supports the null-terminated array of NUL-terminated strings consumed by exec.
/// Given a list of strings, construct a vector of pointers to those strings contents.
/// This is used for building null-terminated arrays of null-terminated strings.
/// *Important*: the vector stores pointers into the interior of the input strings, which may be
/// subject to the small-string optimization. This means that pointers will be left dangling if any
/// input string is deallocated *or moved*. This class should only be used in transient calls.
pub struct CharStarStar<'p, Char> {
    pointers: Vec<*const Char>,
    _phantom: PhantomData<&'p [&'p [Char]]>,
}

// SAFETY: we make sure to append nullptr in the new()
unsafe impl<'p, Char: Nullable> NullTerminatedSequence for CharStarStar<'p, Char> {
    type Item = *const Char;
    /// Return the list of pointers, appropriate for envp or argv.
    /// Note this returns a mutable array of const strings. The caller may rearrange the strings but
    /// not modify their contents.
    /// We freely give out mutable pointers even though we are not mut; this is because most of the uses
    /// expect the array to be mutable even though fish does not mutate it, so it's either this or cast
    /// away the const at the call site.
    fn as_ptr(&self) -> *const Self::Item {
        debug_assert!(
            self.pointers.last().map(|p| p.is_null()).unwrap_or(false),
            "Should have null terminator"
        );
        self.pointers.as_ptr()
    }

    fn len_without_terminator(&self) -> usize {
        self.pointers.len() - 1
    }
}

impl<'p, Char: Nullable> CharStarStar<'p, Char> {
    /// Construct from a list of "strings".
    /// This holds pointers into the strings.
    pub fn new<T, Str>(strs: &'p [T]) -> Self
    where
        T: Deref<Target = Str>,
        Str: NullTerminatedSequence<Item = Char> + ?Sized,
    {
        let mut pointers = Vec::with_capacity(1 + strs.len());
        for s in strs {
            let p = s.deref().as_ptr();
            debug_assert!(!p.is_null());
            pointers.push(p);
        }
        pointers.push(ptr::null());

        Self {
            pointers,
            _phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod char_star_star_tests {
    use super::{CStr, CString, CharStarStar, NullTerminatedSequence};
    #[test]
    fn char_star_star_as_ptr() {
        let c_strings = &[CString::new("foo").unwrap(), CString::new("bar").unwrap()];
        let c_strs: Vec<_> = c_strings.iter().map(|s| s.as_c_str()).collect();
        let char_star_star = CharStarStar::new(&c_strs);
        let ptr = char_star_star.as_ptr();
        unsafe {
            assert_eq!(CStr::from_ptr(*ptr).to_str().unwrap(), "foo");
            assert_eq!(CStr::from_ptr(*ptr.offset(1)).to_str().unwrap(), "bar");
            assert_eq!(*ptr.offset(2), std::ptr::null());
        }
    }
    #[test]
    fn char_star_star_iter() {
        let c_strings = &[CString::new("foo").unwrap(), CString::new("bar").unwrap()];
        let c_strs: Vec<_> = c_strings.iter().map(|s| s.as_c_str()).collect();
        let char_star_star = CharStarStar::new(&c_strs);
        let v1: Vec<_> = char_star_star.iter().collect();
        let v2: Vec<_> = c_strings.iter().map(|s| s.as_ptr()).collect();
        assert_eq!(v1, v2);
    }
}

/// A container which exposes a null-terminated array of pointers to strings that it owns.
/// This is useful for persisted null-terminated arrays, e.g. the exported environment variable
/// list. This assumes u8, since we don't need this for wide chars.
pub struct OwningCharStarStar {
    strings: Pin<Box<[CString]>>,
    // Note that inner holds pointers into our boxed strings.
    // The 'static is a lie.
    inner: CharStarStar<'static, c_char>,
}

impl OwningCharStarStar {
    /// Construct, taking ownership of a list of strings.
    pub fn new(strings: Vec<CString>) -> Self {
        let strings = strings.into_boxed_slice();
        // Safety: we're pinning the strings, so they won't move.
        let string_slice: &'static [CString] = unsafe { std::mem::transmute(&*strings) };
        Self {
            strings: Pin::from(strings),
            inner: CharStarStar::new(string_slice),
        }
    }
}

impl std::ops::Deref for OwningCharStarStar {
    type Target = CharStarStar<'static, c_char>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(test)]
mod owning_char_star_star_tests {
    use super::{CStr, CString, CharStarStar, NullTerminatedSequence, OwningCharStarStar};

    #[test]
    fn owning_char_star_star_ptr() {
        let c_strings = vec![CString::new("foo").unwrap(), CString::new("bar").unwrap()];
        let arr = OwningCharStarStar::new(c_strings);
        let ptr = arr.as_ptr();
        unsafe {
            assert_eq!(CStr::from_ptr(*ptr).to_str().unwrap(), "foo");
            assert_eq!(CStr::from_ptr(*ptr.offset(1)).to_str().unwrap(), "bar");
            assert_eq!(*ptr.offset(2), std::ptr::null());
        }
    }

    #[test]
    fn owning_char_star_star_iter() {
        // TODO
    }
}
