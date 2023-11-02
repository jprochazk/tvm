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
  decls: Vec<Decl<'src>>,

  decl_id: IdGen<DeclId>,
  stmt_id: IdGen<StmtId>,
  type_id: IdGen<TypeId>,
  expr_id: IdGen<ExprId>,

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

      decl_id: IdGen::new(),
      stmt_id: IdGen::new(),
      type_id: IdGen::new(),
      expr_id: IdGen::new(),

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

  fn decl_id(&mut self) -> DeclId {
    self.decl_id.next()
  }

  fn stmt_id(&mut self) -> StmtId {
    self.stmt_id.next()
  }

  fn type_id(&mut self) -> TypeId {
    self.type_id.next()
  }

  fn expr_id(&mut self) -> ExprId {
    self.expr_id.next()
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
  let tail = if !p.prev.is(t![;]) && body.last().is_some_and(Stmt::is_expr) {
    Some(body.pop().unwrap().into_expr().unwrap().inner)
  } else {
    None
  };

  Block {
    span: p.finish(s),
    body,
    tail,
  }
}

fn top_level_stmt_or_sync<'src>(p: &mut Parser<'src>, out: &mut Vec<Stmt<'src>>) {
  match top_level_stmt(p) {
    Ok(StmtOrDecl::Stmt(stmt)) => out.push(stmt),
    Ok(StmtOrDecl::Decl(decl)) => p.decls.push(decl),
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
  Stmt(Stmt<'src>),
  Decl(Decl<'src>),
}

fn top_level_stmt<'src>(p: &mut Parser<'src>) -> Result<StmtOrDecl<'src>> {
  match p.kind() {
    t![fn] => fn_(p).map(StmtOrDecl::Decl),
    _ => stmt(p).map(StmtOrDecl::Stmt),
  }
}

fn fn_<'src>(p: &mut Parser<'src>) -> Result<Decl<'src>> {
  let s = p.span();

  assert!(p.eat(t![fn]));
  let name = ident(p)?;
  let params = params(p)?;
  let ret = p.eat(t![->]).then(|| type_(p)).transpose()?;
  let body = block(p)?;
  Ok(Decl::make_fn(
    p.decl_id(),
    p.finish(s),
    name,
    params,
    ret,
    body,
  ))
}

fn stmt<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
  match p.kind() {
    t![fn] => {
      let span = p.curr.span;
      let _ = fn_(p)?;
      Err(p.ecx.no_nested_functions(span))
    }
    t![loop] => loop_(p),
    t![let] => let_(p),
    t![break] => break_(p).map(|e| expr_to_stmt(p, e)),
    t![continue] => continue_(p).map(|e| expr_to_stmt(p, e)),
    t![return] => return_(p).map(|e| expr_to_stmt(p, e)),
    t![if] => if_(p).map(|e| expr_to_stmt(p, e)),
    // t![yield] => yield_(p).map(|e| expr_to_stmt(p, e)),
    t!["{"] => block(p).map(|block| {
      let e = Expr::make_block(p.expr_id(), block.span, block);
      expr_to_stmt(p, e)
    }),
    _ => assign(p).map(|e| expr_to_stmt(p, e)),
  }
}

fn expr_to_stmt<'src>(p: &mut Parser<'_>, e: Expr<'src>) -> Stmt<'src> {
  Stmt::make_expr(p.stmt_id(), e.span, e)
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

fn loop_<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
  let s = p.span();

  assert!(p.eat(t![loop]));
  let body = block(p)?;
  Ok(Stmt::make_loop(p.stmt_id(), p.finish(s), body))
}

fn let_<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
  assert!(p.eat(t![let]));
  let s = p.span();

  let name = ident(p)?;
  let ty = if p.eat(t![:]) { Some(type_(p)?) } else { None };
  p.must(t![=])?;
  let init = expr(p)?;

  Ok(Stmt::make_let(p.stmt_id(), p.finish(s), name, ty, init))
}

fn break_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  assert!(p.eat(t![break]));
  Ok(Expr::make_break(p.expr_id(), p.prev.span))
}

fn continue_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  assert!(p.eat(t![continue]));
  Ok(Expr::make_continue(p.expr_id(), p.prev.span))
}

fn return_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let s = p.span();

  assert!(p.eat(t![return]));
  let value = if !p.at_any([t!["}"], t![;]]) {
    Some(expr(p)?)
  } else {
    None
  };
  Ok(Expr::make_return(p.expr_id(), p.finish(s), value))
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
  Ok(Expr::make_yield(p.expr_id(), span, value))
} */

fn if_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
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

  Ok(Expr::make_if(p.expr_id(), p.finish(s), branches, tail))
}

fn branch<'src>(p: &mut Parser<'src>) -> Result<Branch<'src>> {
  Ok(Branch {
    cond: expr(p)?,
    body: block(p)?,
  })
}

fn type_<'src>(p: &mut Parser<'src>) -> Result<Type<'src>> {
  match p.kind() {
    t![_] => {
      p.advance();
      Ok(Type::make_empty(p.type_id(), p.prev.span))
    }
    t![ident] => {
      let ident = ident(p)?;
      let ty = Type::make_named(p.type_id(), ident.span, ident);
      Ok(opt(p, ty))
    }
    t!["["] => {
      let start = p.curr.span.start;
      p.advance();
      let element = type_(p)?;
      let end = p.prev.span.end;
      p.must(t!["]"])?;
      let ty = Type::make_array(p.type_id(), start..end, element);
      Ok(opt(p, ty))
    }
    t!["("] => {
      let start = p.curr.span.start;

      let end = p.prev.span.end;
      let params = paren_list(p, type_)?;
      p.must(t![->])?;
      let ret = type_(p)?;
      Ok(Type::make_fn(p.type_id(), start..end, params, ret))
    }
    _ => Err(p.ecx.unexpected_token(p.curr.span)),
  }
}

fn opt<'src>(p: &mut Parser<'src>, inner: Type<'src>) -> Type<'src> {
  if p.eat(t![?]) {
    Type::make_opt(p.type_id(), inner.span.to(p.prev.span), inner)
  } else {
    inner
  }
}

fn ident<'src>(p: &mut Parser<'src>) -> Result<Ident<'src>> {
  p.must(t![ident])?;

  Ok(Ident::from_token(&p.lex, &p.prev))
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
  let tail = if !p.was(t![;]) && body.last().is_some_and(Stmt::is_expr) {
    Some(body.pop().unwrap().into_expr().unwrap().inner)
  } else {
    None
  };
  p.must(t!["}"])?;

  Ok(Block {
    span: p.finish(s),
    body,
    tail,
  })
}

fn assign<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
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
  match lhs.kind {
    ExprKind::UseVar(lhs) => Ok(Expr::make_assign_var(
      p.expr_id(),
      span,
      lhs.name,
      op,
      value,
    )),
    ExprKind::UseField(lhs) => Ok(Expr::make_assign_field(
      p.expr_id(),
      span,
      lhs.parent,
      lhs.name,
      op,
      value,
    )),
    ExprKind::UseIndex(lhs) => Ok(Expr::make_assign_index(
      p.expr_id(),
      span,
      lhs.parent,
      lhs.key,
      op,
      value,
    )),
    _ => Err(p.ecx.invalid_assign_target(lhs.span)),
  }
}

fn expr<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  match p.kind() {
    t![break] => break_(p),
    t![continue] => continue_(p),
    t![return] => return_(p),
    // t![yield] => yield_(p),
    _ => expr::opt(p),
  }
}

mod expr {
  use super::*;

  fn binary<'src>(
    p: &mut Parser<'src>,
    lhs: Expr<'src>,
    op: BinaryOp,
    rhs: Expr<'src>,
  ) -> Expr<'src> {
    Expr::make_binary(p.expr_id(), lhs.span.to(rhs.span), lhs, op, rhs)
  }

  pub(super) fn opt<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = or(p)?;
    while !p.end() && p.eat(t![??]) {
      let rhs = or(p)?;
      lhs = binary(p, lhs, binop![??], rhs);
    }
    Ok(lhs)
  }

  pub fn or<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = and(p)?;
    while !p.end() && p.eat(t![||]) {
      let rhs = and(p)?;
      lhs = binary(p, lhs, binop![||], rhs);
    }
    Ok(lhs)
  }

  fn and<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = eq(p)?;
    while !p.end() && p.eat(t![&&]) {
      let rhs = eq(p)?;
      lhs = binary(p, lhs, binop![&&], rhs);
    }
    Ok(lhs)
  }

  fn eq<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = cmp(p)?;
    while !p.end() {
      let op = match p.kind() {
        t![==] => binop![==],
        t![!=] => binop![!=],
        _ => break,
      };
      p.advance(); // op
      let rhs = cmp(p)?;
      lhs = binary(p, lhs, op, rhs);
    }
    Ok(lhs)
  }

  fn cmp<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = add(p)?;
    while !p.end() {
      let op = match p.kind() {
        t![>] => binop![>],
        t![>=] => binop![>=],
        t![<] => binop![<],
        t![<=] => binop![<=],
        _ => break,
      };
      p.advance(); // op
      let rhs = add(p)?;
      lhs = binary(p, lhs, op, rhs);
    }
    Ok(lhs)
  }

  fn add<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = mul(p)?;
    while !p.end() {
      let op = match p.kind() {
        t![+] => binop![+],
        t![-] => binop![-],
        _ => break,
      };
      p.advance(); // op
      let rhs = mul(p)?;
      lhs = binary(p, lhs, op, rhs);
    }
    Ok(lhs)
  }

  fn mul<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = pow(p)?;
    while !p.end() {
      let op = match p.kind() {
        t![*] => binop![*],
        t![/] => binop![/],
        t![%] => binop![%],
        _ => break,
      };
      p.advance(); // op
      let rhs = pow(p)?;
      lhs = binary(p, lhs, op, rhs);
    }
    Ok(lhs)
  }

  fn pow<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = pre(p)?;
    while !p.end() && p.eat(t![**]) {
      let rhs = pre(p)?;
      lhs = binary(p, lhs, binop![**], rhs);
    }
    Ok(lhs)
  }

  fn pre<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let op = match p.kind() {
      t![-] => unop![-],
      t![!] => unop![!],
      t![?] => unop![?],
      _ => return post(p),
    };
    let tok = p.advance();
    let rhs = pre(p)?;
    Ok(Expr::make_unary(
      p.expr_id(),
      tok.span.to(rhs.span),
      op,
      rhs,
    ))
  }

  fn post<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut expr = primary(p)?;
    while !p.end() {
      match p.kind() {
        t!["("] => expr = call(p, expr)?,
        t!["["] => expr = index(p, expr)?,
        t![.] => expr = field(p, expr)?,
        _ => break,
      };
    }
    Ok(expr)
  }

  fn call<'src>(p: &mut Parser<'src>, target: Expr<'src>) -> Result<Expr<'src>> {
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
    match target.into_use_field() {
      Ok(UseFieldExpr { parent, name, .. }) => Ok(Expr::make_method_call(
        p.expr_id(),
        span,
        parent,
        name,
        args,
      )),
      Err(target) => Ok(Expr::make_call(p.expr_id(), span, target, args)),
    }
  }

  fn arg<'src>(p: &mut Parser<'src>) -> Result<Arg<'src>> {
    let value = expr(p)?;
    if !p.eat(t![:]) {
      return Ok(Arg { key: None, value });
    }

    match value.into_use_var() {
      Ok(UseVarExpr { name: key, .. }) => Ok(Arg {
        key: Some(key),
        value: expr(p)?,
      }),
      Err(value) => Err(p.ecx.invalid_label(value.span)),
    }
  }

  fn index<'src>(p: &mut Parser<'src>, parent: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let key = expr(p)?;
    p.must(t!["]"])?;

    Ok(Expr::make_use_index(p.expr_id(), p.finish(s), parent, key))
  }

  fn field<'src>(p: &mut Parser<'src>, parent: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t![.]));
    let name = ident(p)?;

    Ok(Expr::make_use_field(p.expr_id(), p.finish(s), parent, name))
  }

  fn primary<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    match p.kind() {
      t![int] => int(p),
      t![float] => float(p),
      t![bool] => bool(p),
      t![str] => str(p),
      t![if] => if_(p),
      t![do] => do_(p),
      t!["["] => array(p),
      t!["("] => group(p),
      t![ident] => use_(p),
      _ => Err(p.ecx.unexpected_token(p.curr.span)),
    }
  }

  fn int<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![int]));
    let span = p.prev.span;
    let v = p
      .lexeme(&p.prev)
      .parse::<i64>()
      .map_err(|_| p.ecx.invalid_int(span))?;
    Ok(Expr::make_literal(p.expr_id(), span, lit!(int, v)))
  }

  fn float<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![float]));
    let span = p.prev.span;
    let v = p
      .lexeme(&p.prev)
      .parse::<f64>()
      .map_err(|_| p.ecx.invalid_float(span))?;
    Ok(Expr::make_literal(p.expr_id(), span, lit!(float, v)))
  }

  fn bool<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![bool]));
    let v = match p.lexeme(&p.prev) {
      "true" => true,
      "false" => false,
      _ => unreachable!(),
    };
    Ok(Expr::make_literal(p.expr_id(), p.prev.span, lit!(bool, v)))
  }

  fn str<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![str]));
    // TODO: unescape
    // TODO: fmt
    Ok(Expr::make_literal(
      p.expr_id(),
      p.prev.span,
      lit!(str, p.lexeme(&p.prev)),
    ))
  }

  fn do_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![do]));
    let s = p.prev.span;
    let body = block(p)?;
    Ok(Expr::make_block(p.expr_id(), s.to(body.span), body))
  }

  fn array<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let mut array = Array::List(vec![]);
    if !p.end() && !p.at(t!["]"]) {
      let first_item = expr(p)?;
      if p.eat(t![;]) {
        let len = expr(p)?;
        array = Array::Copy(first_item, len);
      } else {
        let mut elems = vec![first_item];
        while !p.end() && p.eat(t![,]) && !p.at(t!["]"]) {
          elems.push(expr(p)?);
        }
        array = Array::List(elems);
      }
    };
    p.must(t!["]"])?;

    Ok(Expr::make_literal(
      p.expr_id(),
      p.finish(s),
      lit!(array, array),
    ))
  }

  fn group<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t!["("]));
    let inner = expr(p)?;
    p.must(t![")"])?;
    Ok(inner)
  }

  fn use_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![ident]));
    let name = Ident::from_token(&p.lex, &p.prev);
    Ok(Expr::make_use_var(p.expr_id(), p.prev.span, name))
  }
}

#[cfg(test)]
mod tests;
