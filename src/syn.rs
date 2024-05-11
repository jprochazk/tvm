use crate::ast::*;
use crate::error::{Error, ErrorCtx, Report, Result};
use crate::lex::{Lexer, Span, Token, TokenKind, EOF};
use crate::value::f64n;
use crate::Str;

/// Performs a resilient parse, in which the returned `Ast`
/// may be incomplete if the returned list of errors is
/// not empty.
pub fn parse(s: &str) -> (Ast<'_>, Report) {
    Parser::new(s).parse()
}

/// Performs a full parse.
pub fn try_parse(s: &str) -> Result<Ast<'_>, Report> {
    Parser::new(s).try_parse()
}

// TODO: yield may only appear in `gen` fn

struct Parser<'src> {
    decls: Vec<Decl<'src>>,
    lex: Lexer<'src>,
    prev: Token,
    curr: Token,
    ecx: ErrorCtx<'src>,
}

impl<'src> Parser<'src> {
    #[inline]
    pub fn new(src: &'src str) -> Parser<'src> {
        Parser {
            decls: Vec::new(),
            lex: Lexer::new(src),
            prev: EOF,
            curr: EOF,
            ecx: ErrorCtx::new(src),
        }
    }

    pub fn parse(mut self) -> (Ast<'src>, Report) {
        self.advance();

        let top_level = top_level(&mut self);
        let decls = self.decls;
        (
            Ast {
                src: self.lex.src(),
                decls,
                top_level,
            },
            self.ecx.finish().err().unwrap_or_default(),
        )
    }

    pub fn try_parse(self) -> Result<Ast<'src>, Report> {
        let (ast, errors) = self.parse();

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(ast)
        }
    }

    #[inline]
    fn span(&self) -> Span {
        self.curr.span
    }

    #[inline]
    fn finish(&self, span: Span) -> Span {
        Span {
            start: span.start,
            end: self.prev.span.end,
        }
    }

    #[inline]
    fn end(&self) -> bool {
        self.curr.is(t![EOF])
    }

    #[inline]
    fn kind(&self) -> TokenKind {
        self.curr.kind
    }

    #[inline]
    fn at(&self, kind: TokenKind) -> bool {
        self.curr.is(kind)
    }

    #[inline]
    fn was(&self, kind: TokenKind) -> bool {
        self.prev.is(kind)
    }

    #[inline]
    fn at_any(&self, kinds: impl IntoIterator<Item = TokenKind>) -> bool {
        for kind in kinds {
            if self.curr.is(kind) {
                return true;
            }
        }

        false
    }

    #[inline]
    fn eat(&mut self, kind: TokenKind) -> bool {
        let found = self.at(kind);
        if found {
            self.advance();
        }
        found
    }

    #[inline]
    fn must_if(&mut self, cond: bool, kind: TokenKind) -> Result<()> {
        if !self.eat(kind) && cond {
            let span = if self.prev.span.end > 0 {
                (self.prev.span.end - 1..self.prev.span.end).into()
            } else if !self.end() {
                self.curr.span
            } else {
                Span::empty()
            };
            Err(self.ecx.expected_token(kind.name(), span))
        } else {
            Ok(())
        }
    }

    #[inline]
    fn must(&mut self, kind: TokenKind) -> Result<()> {
        if !self.eat(kind) {
            let span = if self.prev.span.end > 0 {
                (self.prev.span.end - 1..self.prev.span.end).into()
            } else if !self.end() {
                self.curr.span
            } else {
                Span::empty()
            };
            return Err(self.ecx.expected_token(kind.name(), span));
        }
        Ok(())
    }

    #[inline]
    fn advance(&mut self) -> Token {
        self.prev = self.curr;
        self.curr = loop {
            match self.lex.bump() {
                Ok(token) => break token,
                Err(e) => {
                    self.ecx.emit_unexpected_token(e.span);
                }
            }
        };
        self.prev
    }

    #[inline]
    fn lexeme(&self, token: &Token) -> &'src str {
        self.lex.lexeme(token)
    }
}

fn top_level<'src>(p: &mut Parser<'src>) -> Block<'src> {
    let s = p.span();

    let mut body = vec![];
    if !p.end() {
        top_level_stmt_or_sync(p, &mut body);
        while !p.end() {
            if let Err(e) = p.must_if(!p.was(t!["}"]), t![;]) {
                p.ecx.push(e);
            }
            if p.end() {
                // trailing semicolon
                break;
            }
            top_level_stmt_or_sync(p, &mut body);
        }
    }
    let mut tail = None;
    if !p.prev.is(t![;]) {
        match body.pop() {
            Some(Stmt {
                kind: stmt::StmtKind::Expr(inner),
                ..
            }) => tail = Some(inner),
            Some(other) => body.push(other),
            None => {}
        }
    }

    Block {
        span: p.finish(s),
        body,
        tail,
    }
}

fn top_level_stmt_or_sync<'src>(p: &mut Parser<'src>, out: &mut Vec<Stmt<'src>>) {
    match top_level_stmt(p) {
        Ok(Top::Stmt(stmt)) => out.push(stmt),
        Ok(Top::Decl(decl)) => p.decls.push(decl),
        Err(e) => sync(p, e, SyncCtx::TopLevel),
    }
}

fn stmt_or_sync<'src>(p: &mut Parser<'src>, out: &mut Vec<Stmt<'src>>) {
    match stmt(p) {
        Ok(stmt) => out.push(stmt),
        Err(e) => sync(p, e, SyncCtx::Inner),
    }
}

#[derive(Clone, Copy)]
enum SyncCtx {
    TopLevel,
    Inner,
}

fn sync(p: &mut Parser<'_>, e: Error, kind: SyncCtx) {
    p.ecx.push(e);

    // quick semicolon check
    // will be bumped by `stmt` parser
    if p.at(t![;]) {
        return;
    }

    // something else is likely to bump this curly brace
    if matches!(kind, SyncCtx::Inner) && p.at(t!["}"]) {
        return;
    }

    p.advance();
    while !p.end() {
        if p.at_any([t![fn], t![loop], t![if], t![let], t!["{"]]) {
            break;
        }

        if p.at_any([t!["}"], t![;]]) {
            p.advance();
            break;
        }

        p.advance();
    }
}

enum Top<'src> {
    Stmt(Stmt<'src>),
    Decl(Decl<'src>),
}

fn top_level_stmt<'src>(p: &mut Parser<'src>) -> Result<Top<'src>> {
    let extern_ = p.eat(t![extern]);
    match p.kind() {
        t![fn] => fn_(p, extern_).map(Top::Decl),
        t![type] => type_(p, extern_).map(Top::Decl),
        _ if extern_ => Err(p.ecx.unexpected_token(p.prev.span)),
        _ => stmt(p).map(Top::Stmt),
    }
}

fn fn_<'src>(p: &mut Parser<'src>, extern_: bool) -> Result<Decl<'src>> {
    let s = p.span();

    assert!(p.eat(t![fn]));
    let name = ident(p)?;
    let params = params(p)?;
    let ret = p.eat(t![->]).then(|| type_expr(p)).transpose()?;
    let body = if extern_ {
        if p.at(t!["{"]) {
            // parse and discard
            p.ecx.emit_extern_fn_body(p.span());
            let _ = block(p)?;
        }

        decl::Body::Extern
    } else {
        decl::Body::Block(block(p)?)
    };
    Ok(decl::Fn::new(p.finish(s), name, params, ret, body))
}

fn type_<'src>(p: &mut Parser<'src>, extern_: bool) -> Result<Decl<'src>> {
    let s = p.span();

    assert!(p.eat(t![type]));
    let name = ident(p)?;
    let fields = if extern_ {
        if p.at(t!["("]) {
            // parse and discard
            p.ecx.emit_extern_type_fields(p.span());
            let _ = fields(p)?;
        }

        decl::Fields::Extern
    } else {
        decl::Fields::Named(
            p.at(t!["("])
                .then(|| fields(p))
                .transpose()?
                .unwrap_or_default(),
        )
    };
    Ok(decl::TypeDef::new(p.finish(s), name, fields))
}

fn fields<'src>(p: &mut Parser<'src>) -> Result<Vec<decl::Field<'src>>> {
    paren_list(p, field)
}

fn field<'src>(p: &mut Parser<'src>) -> Result<decl::Field<'src>> {
    let name = ident(p)?;
    p.must(t![:])?;
    let ty = type_expr(p)?;
    Ok(decl::Field { name, ty })
}

fn stmt<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
    match p.kind() {
        t![fn] => {
            let span = p.curr.span;
            let _ = fn_(p, false)?;
            Err(p.ecx.no_nested_functions(span))
        }
        t![loop] => stmt_loop(p),
        t![let] => stmt_let(p),
        t![break] => expr_break(p).map(Expr::into_stmt),
        t![continue] => expr_continue(p).map(Expr::into_stmt),
        t![return] => expr_return(p).map(Expr::into_stmt),
        t![if] => expr_if(p).map(Expr::into_stmt),
        t!["{"] => block(p).map(|b| b.into_stmt()),
        _ => expr_assign(p).map(Expr::into_stmt),
    }
}

fn param<'src>(p: &mut Parser<'src>) -> Result<Param<'src>> {
    let name = ident(p)?;
    p.must(t![:])?;
    let ty = type_expr(p)?;
    Ok(Param { name, ty })
}

fn params<'src>(p: &mut Parser<'src>) -> Result<Vec<Param<'src>>> {
    paren_list(p, param)
}

fn paren_list<'src, F, T>(p: &mut Parser<'src>, f: F) -> Result<Vec<T>>
where
    F: Fn(&mut Parser<'src>) -> Result<T>,
{
    p.must(t!["("])?;
    let mut out = vec![];
    if !p.end() && !p.at(t![")"]) {
        out.push(f(p)?);
        while !p.end() && p.eat(t![,]) && !p.at(t![")"]) {
            out.push(f(p)?);
        }
    }
    p.must(t![")"])?;
    Ok(out)
}

fn stmt_loop<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
    let s = p.span();

    assert!(p.eat(t![loop]));
    let body = block(p)?;
    Ok(stmt::Loop::new(p.finish(s), body))
}

fn stmt_let<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
    assert!(p.eat(t![let]));
    let s = p.span();

    let name = ident(p)?;
    let ty = if p.eat(t![:]) {
        Some(type_expr(p)?)
    } else {
        None
    };
    p.must(t![=])?;
    let init = expr(p)?;

    Ok(stmt::Let::new(p.finish(s), name, ty, init))
}

fn expr_break<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![break]));
    Ok(expr::Break::new(p.prev.span))
}

fn expr_continue<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![continue]));
    Ok(expr::Continue::new(p.prev.span))
}

fn expr_return<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t![return]));
    let value = if !p.end() && !p.at_any([t!["}"], t![;]]) {
        Some(expr(p)?)
    } else {
        None
    };
    Ok(expr::Return::new(p.finish(s), value))
}

/* fn yield_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let s = p.span();

  assert!(p.eat(t![yield]));
  let value = if !p.at_any([t!["}"], t![;]]) {
    Some(expr(p)?)
  } else {
    None
  };
  let span = p.finish(s);
  Ok(Expr::make_yield( span, value))
} */

fn expr_if<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let if_token = p.span();

    assert!(p.eat(t![if]));
    let mut branches = vec![branch(p)?];
    let mut tail = None;
    while !p.end() && p.eat(t![else]) {
        if p.eat(t![if]) {
            branches.push(branch(p)?);
        } else {
            tail = Some(block(p)?);
        }
    }

    Ok(expr::If::new(p.finish(if_token), if_token, branches, tail))
}

fn branch<'src>(p: &mut Parser<'src>) -> Result<Branch<'src>> {
    Ok(Branch {
        cond: expr(p)?,
        body: block(p)?,
    })
}

fn type_expr<'src>(p: &mut Parser<'src>) -> Result<Ty<'src>> {
    match p.kind() {
        t![_] => {
            p.advance();
            Ok(ty::Empty::new(p.prev.span))
        }
        t![ident] => {
            let ident = ident(p)?;
            Ok(ty::Named::new(ident.span, ident))
        }
        _ => Err(p.ecx.unexpected_token(p.curr.span)),
    }
}

fn ident<'src>(p: &mut Parser<'src>) -> Result<Ident<'src>> {
    p.must(t![ident])?;

    Ok(Ident {
        span: p.prev.span,
        lexeme: p.lexeme(&p.prev),
    })
}

fn block<'src>(p: &mut Parser<'src>) -> Result<Block<'src>> {
    let s = p.span();

    p.must(t!["{"])?;
    let mut body = vec![];
    if !p.end() && !p.at(t!["}"]) {
        stmt_or_sync(p, &mut body);
        while !p.end() && !p.at(t!["}"]) {
            if let Err(e) = p.must_if(!p.was(t!["}"]), t![;]) {
                p.ecx.push(e);
            }
            if p.end() || p.at(t!["}"]) {
                break;
            }
            stmt_or_sync(p, &mut body);
        }
    }
    let mut tail = None;
    if !p.prev.is(t![;]) {
        match body.pop() {
            Some(Stmt {
                kind: stmt::StmtKind::Expr(inner),
                ..
            }) => tail = Some(inner),
            Some(other) => body.push(other),
            None => {}
        }
    }
    p.must(t!["}"])?;

    Ok(Block {
        span: p.finish(s),
        body,
        tail,
    })
}

fn expr_assign<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let lhs = expr(p)?;
    let op = match p.kind() {
        t![=] => None,
        t![+=] => Some(binop![+]),
        t![-=] => Some(binop![-]),
        t![/=] => Some(binop![/]),
        t![*=] => Some(binop![*]),
        t![%=] => Some(binop![%]),
        t![**=] => Some(binop![**]),
        t![??=] => Some(binop![??]),
        _ => return Ok(lhs),
    };
    p.advance();
    let value = expr(p)?;
    let span = lhs.span.to(value.span);
    use expr::ExprKind as E;
    match lhs.kind {
        E::UseVar(lhs) => Ok(expr::AssignVar::new(span, lhs.name, op, value)),
        E::UseField(lhs) => Ok(expr::AssignField::new(
            span, lhs.parent, lhs.name, op, value,
        )),
        E::UseIndex(lhs) => Ok(expr::AssignIndex::new(span, lhs.parent, lhs.key, op, value)),
        _ => Err(p.ecx.invalid_assign_target(lhs.span)),
    }
}

fn expr<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    match p.kind() {
        t![break] => expr_break(p),
        t![continue] => expr_continue(p),
        t![return] => expr_return(p),
        // t![yield] => yield_(p),
        _ => expr_opt(p),
    }
}

fn expr_opt<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_or(p)?;
    while !p.end() && p.eat(t![??]) {
        let rhs = expr_or(p)?;
        lhs = expr::Binary::new(lhs, binop![??], rhs);
    }
    Ok(lhs)
}

fn expr_or<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_and(p)?;
    while !p.end() && p.eat(t![||]) {
        let rhs = expr_and(p)?;
        lhs = expr::Binary::new(lhs, binop![||], rhs);
    }
    Ok(lhs)
}

fn expr_and<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_eq(p)?;
    while !p.end() && p.eat(t![&&]) {
        let rhs = expr_eq(p)?;
        lhs = expr::Binary::new(lhs, binop![&&], rhs);
    }
    Ok(lhs)
}

fn expr_eq<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_cmp(p)?;
    while !p.end() {
        let op = match p.kind() {
            t![==] => binop![==],
            t![!=] => binop![!=],
            _ => break,
        };
        p.advance(); // op
        let rhs = expr_cmp(p)?;
        lhs = expr::Binary::new(lhs, op, rhs);
    }
    Ok(lhs)
}

fn expr_cmp<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_add(p)?;
    while !p.end() {
        let op = match p.kind() {
            t![>] => binop![>],
            t![>=] => binop![>=],
            t![<] => binop![<],
            t![<=] => binop![<=],
            _ => break,
        };
        p.advance(); // op
        let rhs = expr_add(p)?;
        lhs = expr::Binary::new(lhs, op, rhs);
    }
    Ok(lhs)
}

fn expr_add<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_mul(p)?;
    while !p.end() {
        let op = match p.kind() {
            t![+] => binop![+],
            t![-] => binop![-],
            _ => break,
        };
        p.advance(); // op
        let rhs = expr_mul(p)?;
        lhs = expr::Binary::new(lhs, op, rhs);
    }
    Ok(lhs)
}

fn expr_mul<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_pow(p)?;
    while !p.end() {
        let op = match p.kind() {
            t![*] => binop![*],
            t![/] => binop![/],
            t![%] => binop![%],
            _ => break,
        };
        p.advance(); // op
        let rhs = expr_pow(p)?;
        lhs = expr::Binary::new(lhs, op, rhs);
    }
    Ok(lhs)
}

fn expr_pow<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = expr_pre(p)?;
    while !p.end() && p.eat(t![**]) {
        let rhs = expr_pre(p)?;
        lhs = expr::Binary::new(lhs, binop![**], rhs);
    }
    Ok(lhs)
}

fn expr_pre<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let op = match p.kind() {
        t![-] => unop![-],
        t![!] => unop![!],
        t![?] => unop![?],
        _ => return expr_post(p),
    };
    let tok = p.advance();
    let rhs = expr_pre(p)?;
    Ok(expr::Unary::new(tok.span.to(rhs.span), op, rhs))
}

fn expr_post<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut expr = expr_primary(p)?;
    while !p.end() {
        match p.kind() {
            t!["("] => expr = expr_call(p, expr)?,
            t!["["] => expr = expr_index(p, expr)?,
            t![.] => expr = expr_field(p, expr)?,
            _ => break,
        };
    }
    Ok(expr)
}

fn expr_call<'src>(p: &mut Parser<'src>, target: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["("]));
    let mut args = vec![];
    if !p.end() && !p.at(t![")"]) {
        args.push(arg(p)?);
        while !p.end() && p.eat(t![,]) && !p.at(t![")"]) {
            args.push(arg(p)?);
        }
    }
    p.must(t![")"])?;

    let span = p.finish(s);
    match target {
        Expr {
            kind: expr::ExprKind::UseField(field),
            ..
        } => Ok(expr::MethodCall::new(span, field.parent, field.name, args)),
        target => Ok(expr::Call::new(span, target, args)),
    }
}

fn arg<'src>(p: &mut Parser<'src>) -> Result<Arg<'src>> {
    let value = expr(p)?;

    if !p.eat(t![:]) {
        return Ok(Arg { key: None, value });
    }

    match value {
        Expr {
            kind: expr::ExprKind::UseVar(var),
            ..
        } => Ok(Arg {
            key: Some(var.name),
            value: expr(p)?,
        }),
        value => Err(p.ecx.invalid_arg_key(value.span)),
    }
}

fn expr_index<'src>(p: &mut Parser<'src>, parent: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let key = expr(p)?;
    p.must(t!["]"])?;

    Ok(expr::UseIndex::new(p.finish(s), parent, key))
}

fn expr_field<'src>(p: &mut Parser<'src>, parent: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t![.]));
    let name = ident(p)?;

    Ok(expr::UseField::new(p.finish(s), parent, name))
}

fn expr_primary<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    match p.kind() {
        t![int] => expr_int(p),
        t![float] => expr_float(p),
        t![bool] => expr_bool(p),
        t![str] => expr_str(p),
        t![if] => expr_if(p),
        t![do] => expr_do(p),
        t!["["] => expr_array(p),
        t!["("] => expr_group(p),
        t![ident] => expr_use(p),
        _ => Err(p.ecx.unexpected_token(p.curr.span)),
    }
}

fn expr_int<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![int]));
    let span = p.prev.span;
    let v = p
        .lexeme(&p.prev)
        .parse::<i64>()
        .map_err(|_| p.ecx.invalid_int(span))?;
    Ok(expr::Primitive::int(span, v))
}

fn expr_float<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![float]));
    let span = p.prev.span;
    let v = p
        .lexeme(&p.prev)
        .parse::<f64n>()
        .map_err(|_| p.ecx.invalid_float(span))?;
    Ok(expr::Primitive::num(span, v))
}

fn expr_bool<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![bool]));
    let lexeme = p.lexeme(&p.prev);
    let v = match lexeme {
        "true" => true,
        "false" => false,
        _ => {
            panic!("non-bool tokenized as bool: {lexeme}")
        }
    };
    Ok(expr::Primitive::bool(p.prev.span, v))
}

fn expr_str<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![str]));
    // TODO: fmt strings
    let span = p.prev.span;
    let lexeme = p.lexeme(&p.prev);
    let lexeme = &lexeme[1..lexeme.len() - 1]; // strip quotes
    let lexeme = match unescape(lexeme) {
        Ok(v) => v,
        Err(e) => {
            return Err(p.ecx.invalid_escape_sequence(Span {
                start: span.start + 1 + e.pos as u32,
                end: span.start + 1 + e.pos as u32 + 1,
            }))
        }
    };
    Ok(expr::Primitive::str(p.prev.span, lexeme))
}

fn expr_do<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![do]));
    let s = p.prev.span;
    let body = block(p)?;
    let mut e = body.into_expr();
    e.span = s.to(e.span); // extend span to include `do` keyword
    Ok(e)
}

fn expr_array<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let mut items = vec![];
    if !p.end() && !p.at(t!["]"]) {
        items.push(expr(p)?);
        while !p.end() && p.eat(t![,]) && !p.at(t!["]"]) {
            items.push(expr(p)?);
        }
    }
    p.must(t!["]"])?;

    Ok(expr::Array::new(p.finish(s), items))
}

fn expr_group<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t!["("]));
    let inner = expr(p)?;
    p.must(t![")"])?;
    Ok(inner)
}

fn expr_use<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let ident = ident(p)?;
    Ok(expr::UseVar::new(ident.span, ident))
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
/// Unescapes the given string in-place. Returns `None` if the string contains
/// an invalid escape sequence.
fn unescape(s: &str) -> Result<Str<'_>, InvalidEscape> {
    let result = if s.chars().any(|c| c == '\\') {
        actually_unescape(s).map(Str::owned)
    } else {
        Ok(Str::borrowed(s))
    };

    result.map_err(|i| {
        // count the number of utf8 bytes
        let pos = s.chars().take(i).map(|c| c.len_utf8()).sum();
        InvalidEscape { pos }
    })
}

struct InvalidEscape {
    pos: usize,
}

// In case of error returns the character index of the invalid escape
fn actually_unescape(s: &str) -> Result<String, usize> {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().enumerate();
    while let Some((i, prefix)) = chars.next() {
        if prefix == '\\' {
            if let Some((_, ch)) = chars.next() {
                let escape = match ch {
                    'a' => Some('\u{07}'),
                    'b' => Some('\u{08}'),
                    'v' => Some('\u{0B}'),
                    'f' => Some('\u{0C}'),
                    'n' => Some('\n'),
                    'r' => Some('\r'),
                    't' => Some('\t'),
                    '\'' => Some('\''),
                    '"' => Some('"'),
                    '\\' => Some('\\'),
                    'e' | 'E' => Some('\u{1B}'),
                    'x' => Some(parse_hex_code(&mut chars).ok_or(i)?),
                    'u' => Some(parse_unicode(&mut chars).ok_or(i)?),
                    _ => None,
                };
                match escape {
                    Some(esc) => {
                        out.push(esc);
                    }
                    None => return Err(i),
                }
            }
        } else {
            out.push(prefix);
        }
    }
    Ok(out)
}

fn parse_hex_code<I>(chars: &mut I) -> Option<char>
where
    I: Iterator<Item = (usize, char)>,
{
    let digits = [
        u8::try_from(chars.next()?.1).ok()?,
        u8::try_from(chars.next()?.1).ok()?,
    ];
    let digits = std::str::from_utf8(&digits[..]).ok()?;
    let c = u32::from_str_radix(digits, 16).ok()?;
    char::from_u32(c)
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
fn parse_unicode<I>(chars: &mut I) -> Option<char>
where
    I: Iterator<Item = (usize, char)>,
{
    match chars.next() {
        Some((_, '{')) => {}
        _ => {
            return None;
        }
    }

    let unicode_seq: String = chars
        .take_while(|(_, c)| *c != '}')
        .map(|(_, c)| c)
        .collect();

    u32::from_str_radix(&unicode_seq, 16)
        .ok()
        .and_then(char::from_u32)
}

#[cfg(test)]
mod tests;
