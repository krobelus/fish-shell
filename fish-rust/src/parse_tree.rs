//! Programmatic representation of fish code.

use std::pin::Pin;
use std::rc::Rc;

use crate::ast::Ast;
use crate::parse_constants::{
    token_type_user_presentable_description, ParseErrorCode, ParseErrorList, ParseKeyword,
    ParseTokenType, ParseTreeFlags, SourceOffset, SourceRange, PARSE_FLAG_CONTINUE_AFTER_ERROR,
    SOURCE_OFFSET_INVALID,
};
use crate::tokenizer::TokenizerError;
use crate::wchar::{wstr, WString, L};
use crate::wchar_ffi::{WCharFromFFI, WCharToFFI};
use crate::wutil::sprintf;
use cxx::{CxxWString, UniquePtr};

/// A struct representing the token type that we use internally.
struct ParseToken {
    /// The type of the token as represented by the parser
    typ: ParseTokenType,
    /// Any keyword represented by this token
    keyword: ParseKeyword,
    /// Hackish: whether the source contains a dash prefix
    has_dash_prefix: bool,
    /// Hackish: whether the source looks like '-h' or '--help'
    is_help_argument: bool,
    /// Hackish: if TOK_END, whether the source is a newline.
    is_newline: bool,
    // Hackish: whether this token is a string like FOO=bar
    may_be_variable_assignment: bool,
    /// If this is a tokenizer error, that error.
    error: TokenizerError,
    source_start: SourceOffset,
    source_length: SourceOffset,
}

impl ParseToken {
    pub fn new(typ: ParseTokenType) -> Self {
        ParseToken {
            typ,
            keyword: ParseKeyword::none,
            has_dash_prefix: false,
            is_help_argument: false,
            is_newline: false,
            may_be_variable_assignment: false,
            error: TokenizerError::none,
            source_start: SOURCE_OFFSET_INVALID,
            source_length: 0,
        }
    }
    /// \return the source range.
    /// Note the start may be invalid.
    pub fn range(&self) -> SourceRange {
        SourceRange::new(self.source_start, self.source_length)
    }
    /// \return whether we are a string with the dash prefix set.
    pub fn is_dash_prefix_string(&self) -> bool {
        self.typ == ParseTokenType::string && self.has_dash_prefix
    }
    /// Returns a string description of the given parse token.
    pub fn describe(&self) -> WString {
        let mut result = Into::<&'static wstr>::into(self.typ).to_owned();
        if self.keyword != ParseKeyword::none {
            result += &sprintf!(L!(" <%ls>"), Into::<&'static wstr>::into(self.keyword))[..]
        }
        result
    }
    pub fn user_presentable_description(&self) -> WString {
        token_type_user_presentable_description(self.typ, self.keyword)
    }
}

pub fn parse_error_from_tokenizer_error(err: TokenizerError) -> ParseErrorCode {
    match err {
        TokenizerError::none => ParseErrorCode::none,
        TokenizerError::unterminated_quote => ParseErrorCode::tokenizer_unterminated_quote,
        TokenizerError::unterminated_subshell => ParseErrorCode::tokenizer_unterminated_subshell,
        TokenizerError::unterminated_slice => ParseErrorCode::tokenizer_unterminated_slice,
        TokenizerError::unterminated_escape => ParseErrorCode::tokenizer_unterminated_escape,
        _ => ParseErrorCode::tokenizer_other,
    }
}

/// A type wrapping up a parse tree and the original source behind it.
/// TODO model nonmovable
struct ParsedSource {
    src: WString,
    ast: Ast,
}

impl ParsedSource {
    fn new(src: WString, ast: Ast) -> Self {
        ParsedSource { src, ast }
    }
}

type ParsedSourceRef = Option<Rc<ParsedSource>>;

/// Return a shared pointer to ParsedSource, or null on failure.
/// If parse_flag_continue_after_error is not set, this will return null on any error.
fn parse_source(
    src: WString,
    flags: ParseTreeFlags,
    errors: &mut ParseErrorList,
) -> ParsedSourceRef {
    let ast = Ast::parse(&src, flags, errors);
    if ast.errored() && !(flags & PARSE_FLAG_CONTINUE_AFTER_ERROR) {
        None
    } else {
        Some(Rc::new(ParsedSource::new(src, ast)))
    }
}

struct ParsedSourceRefFFI(pub ParsedSourceRef);

#[cxx::bridge]
mod parse_tree_ffi {
    extern "C++" {
        include!("ast.h");
        pub type Ast = crate::ast::Ast;
    }
    extern "Rust" {
        type ParsedSourceRefFFI;
        fn empty_parsed_source_ref() -> Box<ParsedSourceRefFFI>;
        fn new_parsed_source_ref(src: &CxxWString, ast: Pin<&mut Ast>) -> Box<ParsedSourceRefFFI>;
        fn clone(self: &ParsedSourceRefFFI) -> Box<ParsedSourceRefFFI>;
        fn src(self: &ParsedSourceRefFFI) -> UniquePtr<CxxWString>;
    }
}

fn empty_parsed_source_ref() -> Box<ParsedSourceRefFFI> {
    Box::new(ParsedSourceRefFFI(None))
}
fn new_parsed_source_ref(src: &CxxWString, ast: Pin<&mut Ast>) -> Box<ParsedSourceRefFFI> {
    let mut tmp = Ast::default();
    std::mem::swap(&mut tmp, ast.get_mut());
    Box::new(ParsedSourceRefFFI(Some(Rc::new(ParsedSource::new(
        src.from_ffi(),
        tmp,
    )))))
}
impl ParsedSourceRefFFI {
    fn clone(self: &ParsedSourceRefFFI) -> Box<ParsedSourceRefFFI> {
        Box::new(ParsedSourceRefFFI(self.0.clone()))
    }
    fn src(&self) -> UniquePtr<CxxWString> {
        self.0.as_ref().unwrap().src.to_ffi()
    }
}
