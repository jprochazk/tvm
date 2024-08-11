use core::fmt::{self, Debug, Display, Write};
use std::sync::Arc;

use beef::lean::Cow;

use crate::lex::Span;
use crate::util::num_digits;
use crate::Str;

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

#[derive(Default, Debug, Clone)]
pub struct Report(pub Vec<Error>);

impl Report {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Error> {
        self.0.iter()
    }
}

impl IntoIterator for Report {
    type Item = Error;

    type IntoIter = std::vec::IntoIter<Error>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Display for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for e in self.0.iter() {
            writeln!(f, "{e}")?;
        }
        Ok(())
    }
}

impl std::error::Error for Report {}

pub struct ErrorCtx<'src> {
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

    pub fn finish(&mut self) -> Result<(), Report> {
        let errors = std::mem::take(&mut self.errors);
        if errors.is_empty() {
            Ok(())
        } else {
            Err(Report(errors))
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

#[derive(Clone)]
pub struct Error {
    kind: Box<ErrorKind>,
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
            kind: Box::new(ErrorKind::Simple(value.0)),
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
                SpannedErrorInner::Single(src, message) => {
                    Box::new(ErrorKind::Single(src, message))
                }
                SpannedErrorInner::Multi(src, messages) => {
                    Box::new(ErrorKind::Multi(src, messages))
                }
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
    pub(crate) fn from_source_span(source: &str, span: &Span) -> Self {
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

    #[inline]
    pub fn line_span(&self) -> Span {
        Span {
            start: self.line_start as u32,
            end: self.line_end as u32,
        }
    }
}

impl std::error::Error for Error {}

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
        match &*self.kind {
            ErrorKind::Simple(message) => report_simple(f, message),
            ErrorKind::Single(src, message) => report_single(f, src, message),
            ErrorKind::Multi(src, messages) => report_multi(f, src, messages),
        }
    }
}

fn report_simple(out: &mut impl Write, message: &str) -> fmt::Result {
    write!(out, "{message}")
}

fn report_single(out: &mut impl Write, src: &str, message: &Message) -> fmt::Result {
    match message {
        Message::Text(text) => writeln!(out, "{text}"),
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

            writeln!(out, "{text}")?;
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

// error constructors
impl<'src> ErrorCtx<'src> {
    #[inline]
    pub fn unexpected_token(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: unexpected token", span, self.src()).into()
    }

    #[inline]
    pub fn emit_unexpected_token(&mut self, span: impl Into<Span>) {
        let e = self.unexpected_token(span);
        self.push(e);
    }

    #[inline]
    pub fn expected_token(&mut self, kind: &str, span: impl Into<Span>) -> Error {
        Error::spanned(format!("error: expected {kind:?}"), span, self.src()).into()
    }

    #[inline]
    pub fn emit_expected_token(&mut self, kind: &str, span: impl Into<Span>) {
        let e = self.expected_token(kind, span);
        self.push(e);
    }

    #[inline]
    pub fn no_nested_functions(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: functions may not be nested", span, self.src()).into()
    }

    #[inline]
    pub fn emit_no_nested_functions(&mut self, span: impl Into<Span>) {
        let e = self.no_nested_functions(span);
        self.push(e);
    }

    #[inline]
    pub fn invalid_assign_target(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: invalid assignment target", span, self.src()).into()
    }

    #[inline]
    pub fn emit_invalid_assign_target(&mut self, span: impl Into<Span>) {
        let e = self.invalid_assign_target(span);
        self.push(e);
    }

    #[inline]
    pub fn invalid_int(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: invalid integer literal", span, self.src()).into()
    }

    #[inline]
    pub fn emit_invalid_int(&mut self, span: impl Into<Span>) {
        let e = self.invalid_int(span);
        self.push(e);
    }

    #[inline]
    pub fn invalid_float(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: invalid float literal", span, self.src()).into()
    }

    #[inline]
    pub fn emit_invalid_float(&mut self, span: impl Into<Span>) {
        let e = self.invalid_float(span);
        self.push(e);
    }

    #[inline]
    pub fn invalid_arg_key(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: invalid argument key", span, self.src()).into()
    }

    #[inline]
    pub fn emit_invalid_arg_key(&mut self, span: impl Into<Span>) {
        let e = self.invalid_arg_key(span);
        self.push(e);
    }

    #[inline]
    pub fn param_mismatch(&mut self, span: impl Into<Span>, expected: usize, got: usize) -> Error {
        let plural = if expected > 1 { "s" } else { "" };
        Error::spanned(
            format!("error: expected {expected} param{plural}, got {got}"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_param_mismatch(&mut self, span: impl Into<Span>, expected: usize, got: usize) {
        let e = self.param_mismatch(span, expected, got);
        self.push(e);
    }

    #[inline]
    pub fn type_mismatch(
        &mut self,
        span: impl Into<Span>,
        lhs: impl Display,
        rhs: impl Display,
    ) -> Error {
        Error::spanned(
            format!("error: type mismatch between \"{lhs}\" and \"{rhs}\""),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_type_mismatch(
        &mut self,
        span: impl Into<Span>,
        lhs: impl Display,
        rhs: impl Display,
    ) {
        let e = self.type_mismatch(span, lhs, rhs);
        self.push(e);
    }

    #[inline]
    pub fn undefined_var(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: undefined variable", span, self.src()).into()
    }

    #[inline]
    pub fn emit_undefined_var(&mut self, span: impl Into<Span>) {
        let e = self.undefined_var(span);
        self.push(e);
    }

    #[inline]
    pub fn unknown_type(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: type must be known at this point", span, self.src()).into()
    }

    #[inline]
    pub fn emit_unknown_type(&mut self, span: impl Into<Span>) {
        let e = self.unknown_type(span);
        self.push(e);
    }

    #[inline]
    pub fn extern_fn_body(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: an extern function may not have a body",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_extern_fn_body(&mut self, span: impl Into<Span>) {
        let e = self.extern_fn_body(span);
        self.push(e);
    }

    #[inline]
    pub fn any_outside_extern_fn(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: `any` used outside of extern function signature",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_any_outside_extern_fn(&mut self, span: impl Into<Span>) {
        let e = self.any_outside_extern_fn(span);
        self.push(e);
    }

    #[inline]
    pub fn extern_type_fields(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: an extern type may not have fields",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_extern_type_fields(&mut self, span: impl Into<Span>) {
        let e = self.extern_type_fields(span);
        self.push(e);
    }

    #[inline]
    pub fn undefined_decl(&mut self, span: impl Into<Span>, name: &str) -> Error {
        Error::spanned(
            format!("error: {name:?} is not declared in this scope"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_undefined_decl(&mut self, span: impl Into<Span>, name: &str) {
        let e = self.undefined_decl(span, name);
        self.push(e);
    }

    #[inline]
    pub fn invalid_shadowing(
        &mut self,
        span: impl Into<Span>,
        name: &str,
        kind: impl Display,
        existing: impl Display,
        existing_span: impl Into<Span>,
    ) -> Error {
        Error::spanned(
            format!("error: {kind} {name:?} shadows existing {existing}"),
            span,
            self.src(),
        )
        .append_spanned(
            format!("note: existing {existing} declared here"),
            existing_span,
        )
        .into()
    }

    #[inline]
    pub fn emit_invalid_shadowing(
        &mut self,
        span: impl Into<Span>,
        name: &str,
        kind: impl Display,
        existing: impl Display,
        existing_span: impl Into<Span>,
    ) {
        let e = self.invalid_shadowing(span, name, kind, existing, existing_span);
        self.push(e);
    }

    #[inline]
    pub fn bad_return_type<'a>(&'a mut self) -> BadReturnType<'a, 'src> {
        BadReturnType {
            ecx: self,
            name_span: Span::empty(),
            fn_ret: Cow::borrowed("()"),
            fn_ret_span: None,
            ret_val: Cow::borrowed("()"),
            ret_val_span: None,
        }
    }

    #[inline]
    pub fn not_callable(&mut self, span: impl Into<Span>, ty: impl Display) -> Error {
        Error::spanned(
            format!("error: type \"{ty}\" is not callable"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_not_callable(&mut self, span: impl Into<Span>, ty: impl Display) {
        let e = self.not_callable(span, ty);
        self.push(e);
    }

    #[inline]
    pub fn extern_cons_not_callable(&mut self, span: impl Into<Span>, name: &str) -> Error {
        Error::spanned(
            format!(
                "error: {name:?} may not be constructed directly, because it is an external type"
            ),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_extern_cons_not_callable(&mut self, span: impl Into<Span>, name: &str) {
        let e = self.extern_cons_not_callable(span, name);
        self.push(e);
    }

    #[inline]
    pub fn shadowed_def(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: a type with this name already exists",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_shadowed_def(&mut self, span: impl Into<Span>) {
        let e = self.shadowed_def(span);
        self.push(e);
    }

    #[inline]
    pub fn unsupported_op(
        &mut self,
        span: impl Into<Span>,
        ty: impl Display,
        op: impl Display,
    ) -> Error {
        Error::spanned(
            format!("error: the type \"{ty}\" does not support the \"{op}\" operation"),
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_unsupported_op(
        &mut self,
        span: impl Into<Span>,
        ty: impl Display,
        op: impl Display,
    ) {
        let e = self.unsupported_op(span, ty, op);
        self.push(e);
    }

    #[inline]
    pub fn too_many_registers(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: compiler ran out of registers", span, self.src())
            .append("note: the maximum number of registers is 255")
            .append("help: reduce register usage by splitting code into multiple functions")
            .into()
    }

    #[inline]
    pub fn too_many_literals(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: compiler ran out of literal pool slots",
            span,
            self.src(),
        )
        .append("note: the maximum number of literal pool slots is 65535")
        .append("help: reduce literal pool usage by splitting code into multiple functions")
        .into()
    }

    #[inline]
    pub fn invalid_escape_sequence(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: invalid escape sequence", span, self.src()).into()
    }

    #[inline]
    pub fn continue_outside_loop(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: cannot use `continue` outside of loops",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn break_outside_loop(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: cannot use `break` outside of loops",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn missing_if_tail(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned(
            "error: `if` used as an expression must have an `else`",
            span,
            self.src(),
        )
        .into()
    }

    #[inline]
    pub fn emit_missing_if_tail(&mut self, span: impl Into<Span>) {
        let e = self.missing_if_tail(span);
        self.push(e);
    }

    #[inline]
    pub fn return_outside_fn(&mut self, span: impl Into<Span>) -> Error {
        Error::spanned("error: `return` used outside of function", span, self.src()).into()
    }

    #[inline]
    pub fn emit_return_outside_fn(&mut self, span: impl Into<Span>) {
        let e = self.return_outside_fn(span);
        self.push(e);
    }
}

pub struct BadReturnType<'a, 'src> {
    ecx: &'a mut ErrorCtx<'src>,
    name_span: Span,
    fn_ret: Cow<'static, str>,
    fn_ret_span: Option<Span>,
    ret_val: Cow<'static, str>,
    ret_val_span: Option<Span>,
}

impl<'a, 'src> BadReturnType<'a, 'src> {
    pub fn fn_name(mut self, span: impl Into<Span>) -> Self {
        self.name_span = span.into();
        self
    }

    pub fn fn_ret(mut self, ty: impl Display, span: Option<impl Into<Span>>) -> Self {
        self.fn_ret = Cow::owned(ty.to_string());
        self.fn_ret_span = span.map(Into::into);
        self
    }

    pub fn ret_val(mut self, ty: impl Display, span: Option<impl Into<Span>>) -> Self {
        self.ret_val = Cow::owned(ty.to_string());
        self.ret_val_span = span.map(Into::into);
        self
    }

    pub fn finish(&mut self) -> Error {
        let mut err = Error::spanned(
            "error: mismatched return type",
            self.name_span,
            self.ecx.src(),
        );

        let msg = format!("note: the expected return type is \"{}\"", self.fn_ret);
        match self.fn_ret_span {
            Some(span) => err = err.append_spanned(msg, span),
            None => err = err.append(msg),
        }

        let msg = format!("note: the actual return type is \"{}\"", self.ret_val);
        match self.ret_val_span {
            Some(span) => err = err.append_spanned(msg, span),
            None => err = err.append(msg),
        }

        err.into()
    }

    pub fn emit(mut self) {
        let e = self.finish();
        self.ecx.push(e);
    }
}
