use core::fmt::{self, Debug, Display, Write};
use std::sync::Arc;

use crate::lex::{Span, TokenKind};
use crate::util::num_digits;
use crate::Str;

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

pub(crate) struct ErrorCtx<'src> {
    src: &'src str,
    src_rc: Option<Arc<str>>,
    errors: Vec<Error>,
}

impl<'src> ErrorCtx<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            src_rc: None,
            errors: Vec::new(),
        }
    }

    pub fn push(&mut self, e: Error) {
        self.errors.push(e);
    }

    pub fn num_errors(&self) -> usize {
        self.errors.len()
    }

    pub fn finish(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    pub fn finish_result(&mut self) -> Result<(), Vec<Error>> {
        let errors = self.finish();
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn src(&mut self) -> Arc<str> {
        match &self.src_rc {
            Some(v) => Arc::clone(v),
            None => {
                let rc = <Arc<str>>::from(self.src);
                self.src_rc = Some(Arc::clone(&rc));
                rc
            }
        }
    }
}

pub(crate) trait FoldError: Sized + private::Sealed {
    fn fold_error(self, e: &mut ErrorCtx);
}

impl private::Sealed for Result<()> {}
impl FoldError for Result<()> {
    fn fold_error(self, ecx: &mut ErrorCtx) {
        match self {
            Ok(_) => (),
            Err(e) => ecx.push(e),
        }
    }
}

mod private {
    pub trait Sealed {}
}

#[derive(Clone)]
pub struct Error {
    kind: ErrorKind,
}

#[derive(Clone)]
enum ErrorKind {
    Simple(String),
    Single(Arc<str>, Message),
    Multi(Arc<str>, Vec<Message>),
}

impl Error {
    #[inline]
    pub fn simple(message: impl ToString) -> SimpleError {
        SimpleError(message.to_string())
    }

    #[inline]
    pub fn spanned(
        message: impl Into<Str<'static>>,
        span: impl Into<Span>,
        src: impl Into<Arc<str>>,
    ) -> SpannedError {
        SpannedError(SpannedErrorInner::Single(
            src.into(),
            Message::spanned(message, span),
        ))
    }
}

#[derive(Clone)]
pub struct SimpleError(String);

impl From<SimpleError> for Error {
    #[inline]
    fn from(value: SimpleError) -> Self {
        Self {
            kind: ErrorKind::Simple(value.0),
        }
    }
}

#[derive(Clone)]
pub struct SpannedError(SpannedErrorInner);

impl SpannedError {
    #[inline]
    pub fn append(self, message: impl Into<Str<'static>>) -> Self {
        self.append_message(Message::text(message))
    }

    #[inline]
    pub fn append_spanned(self, message: impl Into<Str<'static>>, span: impl Into<Span>) -> Self {
        self.append_message(Message::spanned(message, span))
    }

    #[inline]
    fn append_message(self, message: Message) -> Self {
        Self(match self.0 {
            SpannedErrorInner::Single(src, fst) => {
                SpannedErrorInner::Multi(src, vec![fst, message])
            }
            SpannedErrorInner::Multi(src, mut messages) => {
                messages.push(message);
                SpannedErrorInner::Multi(src, messages)
            }
        })
    }
}

#[derive(Clone)]
enum SpannedErrorInner {
    Single(Arc<str>, Message),
    Multi(Arc<str>, Vec<Message>),
}

impl From<SpannedError> for Error {
    #[inline]
    fn from(value: SpannedError) -> Self {
        Self {
            kind: match value.0 {
                SpannedErrorInner::Single(src, message) => ErrorKind::Single(src, message),
                SpannedErrorInner::Multi(src, messages) => ErrorKind::Multi(src, messages),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum Message {
    Text(Str<'static>),
    Spanned(Str<'static>, Span),
}

impl Message {
    #[inline]
    fn text(v: impl Into<Str<'static>>) -> Self {
        Self::Text(v.into())
    }

    #[inline]
    fn spanned(v: impl Into<Str<'static>>, s: impl Into<Span>) -> Self {
        Self::Spanned(v.into(), s.into())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    line_num: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
}

impl Location {
    fn from_source_span(source: &str, span: &Span) -> Self {
        let line_start = source[..span.start()]
            .rfind('\n')
            .map(|v| v + 1)
            .unwrap_or(0);
        let line_num = 1 + source[..line_start].lines().count();
        let column = span.start() - line_start;
        let line_end = source[span.start()..]
            .find('\n')
            .map(|v| v + span.start())
            .unwrap_or(source.len());

        Self {
            line_num,
            column,
            line_start,
            line_end,
        }
    }

    #[inline]
    pub fn line(&self) -> usize {
        self.line_num
    }

    #[inline]
    pub fn column(&self) -> usize {
        self.column
    }
}

impl std::error::Error for Error {}

/* impl Display for ErrorKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ErrorKind::UnexpectedToken => f.write_str("unexpected token"),
      ErrorKind::ExpectedToken(kind) => write!(f, "expected token `{kind}`"),
      ErrorKind::InvalidFloat => f.write_str("invalid float literal"),
      ErrorKind::InvalidInt => f.write_str("invalid integer literal"),
      ErrorKind::InvalidEscape => f.write_str("invalid string escape"),
      ErrorKind::InvalidPlace => f.write_str("invalid assignment target"),
      ErrorKind::NoNestedFunctions => f.write_str("functions may not be nested"),

      ErrorKind::NoSuchTypeInScope => f.write_str("this type is not in the current scope"),
      ErrorKind::VarAlreadyDefined => {
        f.write_str("this variable is already defined in the current scope")
      }
      ErrorKind::WrongParamCount => f.write_str("mismatched number of params"),
      ErrorKind::UndefinedVar => f.write_str("undefined variable"),
      ErrorKind::NotIndexable => f.write_str("not an indexable collection"),
      ErrorKind::TypeError => f.write_str("type error"),
    }
  }
} */

impl Debug for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&self.kind, f)
    }
}

impl Debug for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple(m) => f.debug_tuple("SimpleError").field(m).finish(),
            Self::Single(_, m) => f.debug_tuple("SpannedError").field(m).finish(),
            Self::Multi(_, m) => f.debug_tuple("SpannedError").field(m).finish(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ErrorKind::Simple(message) => report_simple(f, message),
            ErrorKind::Single(src, message) => report_single(f, src, message),
            ErrorKind::Multi(src, messages) => report_multi(f, src, messages),
        }
    }
}

fn report_simple(out: &mut impl Write, message: &str) -> fmt::Result {
    write!(out, "error: {message}")
}

fn report_single(out: &mut impl Write, src: &str, message: &Message) -> fmt::Result {
    match message {
        Message::Text(text) => writeln!(out, "error: {text}"),
        Message::Spanned(text, span) => {
            let loc = Location::from_source_span(src, span);
            let ln = loc.line_num;
            let lw = num_digits(loc.line_num);
            let pos = span.start() - loc.line_start;
            let len = if span.end() > loc.line_end {
                loc.line_end - span.start()
            } else {
                span.end() - span.start()
            };
            let line = &src[loc.line_start..loc.line_end];

            writeln!(out, "error: {text}")?;
            writeln!(out, "{ln} |  {line}")?;
            writeln!(out, "{:lw$} |  {:pos$}{:^<len$}", "", "", "^")
        }
    }
}

fn report_multi(out: &mut impl Write, src: &str, messages: &[Message]) -> fmt::Result {
    for message in messages {
        report_single(out, src, message)?;
    }
    Ok(())
}

impl<'src> ErrorCtx<'src> {
    #[inline]
    pub fn unexpected_token(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("unexpected token", span, self.src()).into()
    }

    #[inline]
    pub fn expected_token(&mut self, kind: TokenKind, span: impl Into<Span>) -> Error {
        Error::spanned(format!("expected `{}`", kind.name()), span, self.src()).into()
    }

    #[inline]
    pub fn no_nested_functions(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("functions may not be nested", span, self.src()).into()
    }

    #[inline]
    pub fn invalid_assign_target(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("invalid assignment target", span, self.src()).into()
    }

    #[inline]
    pub fn invalid_int(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("invalid integer literal", span, self.src()).into()
    }

    #[inline]
    pub fn invalid_float(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("invalid float literal", span, self.src()).into()
    }

    #[inline]
    pub fn param_mismatch(&mut self, span: impl Into<Span>, expected: usize, got: usize) -> Error {
        Error::spanned(
            format!("expected {expected} params, got {got}"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn type_mismatch(
        &mut self,
        span: impl Into<Span>,
        lhs: impl Display,
        rhs: impl Display,
    ) -> Error {
        Error::spanned(
            format!("type mismatch between `{lhs}` and `{rhs}`"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn infinite_type(&mut self, span: impl Into<Span>, ty: impl Display) -> Error {
        Error::spanned(
            format!("the type `{ty}` has an infinite size"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn invalid_label(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("invalid argument label", span, self.src()).into()
    }

    #[inline]
    pub fn undefined_var(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("undefined variable", span, self.src()).into()
    }

    #[inline]
    pub fn unknown_type(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("type must be known at this point", span, self.src()).into()
    }

    #[inline]
    pub fn unknown_method(
        &mut self,
        span: impl Into<Span>,
        receiver: impl Display,
        name: &str,
    ) -> Error {
        Error::spanned(
            format!("the type `{receiver}` has no method named `{name}`"),
            span,
            self.src(),
        )
        .into()
    }

    /* #[inline]
    pub fn invalid_string_escape(&mut self, span: impl Into<Span>) -> Error {
      Error::spanned("invalid string escape", span, self.src()).into()
    } */
}
