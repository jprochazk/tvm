use crate::ast::*;
use crate::error::{Error, ErrorCtx, Result};
use crate::lex::{Lexer, Span, Token, TokenKind, EOF};

pub fn parse(s: &str) -> (Ast<'_>, Vec<Error>) {
    Parser::new(s).parse()
}

pub fn try_parse(s: &str) -> Result<Ast<'_>, Vec<Error>> {
    Parser::new(s).try_parse()
}

// TODO: top-level "block" exprs are not primary
//       they should instead be parsed eagerly
//       and if you enter grouping, you can use
//       them as a subexpression in binary, unary, etc.
// TODO: yield may only appear in `gen` fn

struct Parser<'src> {
    decls: Vec<Decl<'src, ()>>,
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

    pub fn parse(mut self) -> (Ast<'src>, Vec<Error>) {
        self.advance();

        let top_level = top_level(&mut self);
        let decls = self.decls;
        (
            Ast {
                src: self.lex.src(),
                decls,
                top_level,
            },
            self.ecx.finish(),
        )
    }

    pub fn try_parse(self) -> Result<Ast<'src>, Vec<Error>> {
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
            Err(self.ecx.expected_token(kind, span))
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
            return Err(self.ecx.expected_token(kind, span));
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
                    let e = self.ecx.unexpected_token(e.span);
                    self.ecx.push(e)
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

fn top_level<'src>(p: &mut Parser<'src>) -> Block<'src, ()> {
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

fn top_level_stmt_or_sync<'src>(p: &mut Parser<'src>, out: &mut Vec<Stmt<'src, ()>>) {
    match top_level_stmt(p) {
        Ok(StmtOrDecl::Stmt(stmt)) => out.push(stmt),
        Ok(StmtOrDecl::Decl(decl)) => p.decls.push(decl),
        Err(e) => sync(p, e, SyncCtx::TopLevel),
    }
}

fn stmt_or_sync<'src>(p: &mut Parser<'src>, out: &mut Vec<Stmt<'src, ()>>) {
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

    // something else is likely to bump this curly brace
    if matches!(kind, SyncCtx::Inner) && p.at(t!["}"]) {
        return;
    }

    p.advance();
    while !p.end() {
        // break on these
        if p.at_any([t![fn], t![loop], t![if], t![let], t!["{"]]) {
            break;
        }

        // bump these
        if p.at_any([t!["}"], t![;]]) {
            p.advance();
            break;
        }

        p.advance();
    }
}

enum StmtOrDecl<'src> {
    Stmt(Stmt<'src, ()>),
    Decl(Decl<'src, ()>),
}

fn top_level_stmt<'src>(p: &mut Parser<'src>) -> Result<StmtOrDecl<'src>> {
    match p.kind() {
        t![fn] => fn_(p).map(StmtOrDecl::Decl),
        _ => stmt(p).map(StmtOrDecl::Stmt),
    }
}

fn fn_<'src>(p: &mut Parser<'src>) -> Result<Decl<'src, ()>> {
    let s = p.span();

    assert!(p.eat(t![fn]));
    let name = ident(p)?;
    let params = params(p)?;
    let ret = p.eat(t![->]).then(|| type_(p)).transpose()?;
    let body = block(p)?;
    Ok(decl::Fn::new(p.finish(s), name, params, ret, body))
}

fn stmt<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src, ()>> {
    match p.kind() {
        t![fn] => {
            let span = p.curr.span;
            let _ = fn_(p)?;
            Err(p.ecx.no_nested_functions(span))
        }
        t![loop] => stmt_loop(p),
        t![let] => stmt_let(p),
        t![break] => expr_break(p).map(Expr::into_stmt),
        t![continue] => expr_continue(p).map(Expr::into_stmt),
        t![return] => expr_return(p).map(Expr::into_stmt),
        t![if] => expr_if(p).map(Expr::into_stmt),
        t!["{"] => block(p).map(|b| b.into_stmt(())),
        _ => expr_assign(p).map(Expr::into_stmt),
    }
}

fn param<'src>(p: &mut Parser<'src>) -> Result<Param<'src>> {
    let name = ident(p)?;
    p.must(t![:])?;
    let ty = type_(p)?;
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

fn stmt_loop<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src, ()>> {
    let s = p.span();

    assert!(p.eat(t![loop]));
    let body = block(p)?;
    Ok(stmt::Loop::new(p.finish(s), body))
}

fn stmt_let<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src, ()>> {
    assert!(p.eat(t![let]));
    let s = p.span();

    let name = ident(p)?;
    let ty = if p.eat(t![:]) { Some(type_(p)?) } else { None };
    p.must(t![=])?;
    let init = expr(p)?;

    Ok(stmt::Let::new(p.finish(s), name, ty, init))
}

fn expr_break<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![break]));
    Ok(expr::Break::new(p.prev.span, ()))
}

fn expr_continue<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![continue]));
    Ok(expr::Continue::new(p.prev.span, ()))
}

fn expr_return<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let s = p.span();

    assert!(p.eat(t![return]));
    let value = if !p.at_any([t!["}"], t![;]]) {
        Some(expr(p)?)
    } else {
        None
    };
    Ok(expr::Return::new(p.finish(s), value))
}

/* fn yield_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_if<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let s = p.span();

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

    Ok(expr::If::new(p.finish(s), branches, tail))
}

fn branch<'src>(p: &mut Parser<'src>) -> Result<Branch<'src, ()>> {
    Ok(Branch {
        cond: expr(p)?,
        body: block(p)?,
    })
}

fn type_<'src>(p: &mut Parser<'src>) -> Result<Type<'src>> {
    match p.kind() {
        t![_] => {
            p.advance();
            Ok(ty::Empty::new(p.prev.span))
        }
        t![ident] => {
            let ident = ident(p)?;
            let ty = ty::Named::new(ident.span, ident);
            Ok(type_opt(p, ty))
        }
        t!["["] => {
            let start = p.curr.span.start;
            p.advance();
            let element = type_(p)?;
            let end = p.prev.span.end;
            p.must(t!["]"])?;
            let ty = ty::Array::new(start..end, element);
            Ok(type_opt(p, ty))
        }
        t!["("] => {
            let start = p.curr.span.start;

            let end = p.prev.span.end;
            let params = paren_list(p, type_)?;
            p.must(t![->])?;
            let ret = type_(p)?;
            Ok(ty::Fn::new(start..end, params, ret))
        }
        _ => Err(p.ecx.unexpected_token(p.curr.span)),
    }
}

fn type_opt<'src>(p: &mut Parser<'src>, inner: Type<'src>) -> Type<'src> {
    if p.eat(t![?]) {
        ty::Opt::new(inner.span.to(p.prev.span), inner)
    } else {
        inner
    }
}

fn ident<'src>(p: &mut Parser<'src>) -> Result<Ident<'src>> {
    p.must(t![ident])?;

    Ok(Ident::from_token(&p.lex, &p.prev))
}

fn block<'src>(p: &mut Parser<'src>) -> Result<Block<'src, ()>> {
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

fn expr_assign<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    match p.kind() {
        t![break] => expr_break(p),
        t![continue] => expr_continue(p),
        t![return] => expr_return(p),
        // t![yield] => yield_(p),
        _ => expr_opt(p),
    }
}

fn expr_opt<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let mut lhs = expr_or(p)?;
    while !p.end() && p.eat(t![??]) {
        let rhs = expr_or(p)?;
        lhs = expr::Binary::new(lhs, binop![??], rhs);
    }
    Ok(lhs)
}

fn expr_or<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let mut lhs = expr_and(p)?;
    while !p.end() && p.eat(t![||]) {
        let rhs = expr_and(p)?;
        lhs = expr::Binary::new(lhs, binop![||], rhs);
    }
    Ok(lhs)
}

fn expr_and<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let mut lhs = expr_eq(p)?;
    while !p.end() && p.eat(t![&&]) {
        let rhs = expr_eq(p)?;
        lhs = expr::Binary::new(lhs, binop![&&], rhs);
    }
    Ok(lhs)
}

fn expr_eq<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_cmp<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_add<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_mul<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_pow<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let mut lhs = expr_pre(p)?;
    while !p.end() && p.eat(t![**]) {
        let rhs = expr_pre(p)?;
        lhs = expr::Binary::new(lhs, binop![**], rhs);
    }
    Ok(lhs)
}

fn expr_pre<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_post<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_call<'src>(p: &mut Parser<'src>, target: Expr<'src, ()>) -> Result<Expr<'src, ()>> {
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

fn arg<'src>(p: &mut Parser<'src>) -> Result<Arg<'src, ()>> {
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
        value => Err(p.ecx.invalid_label(value.span)),
    }
}

fn expr_index<'src>(p: &mut Parser<'src>, parent: Expr<'src, ()>) -> Result<Expr<'src, ()>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let key = expr(p)?;
    p.must(t!["]"])?;

    Ok(expr::UseIndex::new(p.finish(s), parent, key))
}

fn expr_field<'src>(p: &mut Parser<'src>, parent: Expr<'src, ()>) -> Result<Expr<'src, ()>> {
    let s = p.span();

    assert!(p.eat(t![.]));
    let name = ident(p)?;

    Ok(expr::UseField::new(p.finish(s), parent, name))
}

fn expr_primary<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
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

fn expr_int<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![int]));
    let span = p.prev.span;
    let v = p
        .lexeme(&p.prev)
        .parse::<i64>()
        .map_err(|_| p.ecx.invalid_int(span))?;
    Ok(expr::Primitive::new_int(span, v))
}

fn expr_float<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![float]));
    let span = p.prev.span;
    let v = p
        .lexeme(&p.prev)
        .parse::<f64>()
        .map_err(|_| p.ecx.invalid_float(span))?;
    Ok(expr::Primitive::new_num(span, v))
}

fn expr_bool<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![bool]));
    let v = match p.lexeme(&p.prev) {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    };
    Ok(expr::Primitive::new_bool(p.prev.span, v))
}

fn expr_str<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![str]));
    // TODO: unescape
    // TODO: fmt
    Ok(expr::Primitive::new_str(p.prev.span, p.lexeme(&p.prev)))
}

fn expr_do<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![do]));
    let s = p.prev.span;
    let body = block(p)?;
    let mut e = body.into_expr(());
    e.span = s.to(e.span); // extend span to include `do` keyword
    Ok(e)
}

fn expr_array<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let array = if !p.end() && !p.at(t!["]"]) {
        let first_item = expr(p)?;
        if p.eat(t![;]) {
            let len = expr(p)?;
            expr::Array::Len(first_item, len)
        } else {
            let mut elems = vec![first_item];
            while !p.end() && p.eat(t![,]) && !p.at(t!["]"]) {
                elems.push(expr(p)?);
            }
            expr::Array::Csv(elems)
        }
    } else {
        expr::Array::Csv(vec![])
    };
    p.must(t!["]"])?;

    Ok(array.into_expr(p.finish(s)))
}

fn expr_group<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t!["("]));
    let inner = expr(p)?;
    p.must(t![")"])?;
    Ok(inner)
}

fn expr_use<'src>(p: &mut Parser<'src>) -> Result<Expr<'src, ()>> {
    assert!(p.eat(t![ident]));
    let name = Ident::from_token(&p.lex, &p.prev);
    Ok(expr::UseVar::new(p.prev.span, name))
}

#[cfg(test)]
mod tests;
