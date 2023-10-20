use crate::ast::*;
use crate::error::{Error, Result};
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
// TODO: declarations may only exist at the top level

pub struct Parser<'src> {
  decls: Vec<Decl<'src>>,

  lex: Lexer<'src>,
  prev: Token,
  curr: Token,
  errors: Vec<Error>,
}

impl<'src> Parser<'src> {
  #[inline]
  pub fn new(src: &'src str) -> Parser<'src> {
    Parser {
      decls: Vec::new(),

      lex: Lexer::new(src),
      prev: EOF,
      curr: EOF,
      errors: Vec::new(),
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
      self.errors,
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
      Err(err!(@span, ExpectedToken, kind))
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
      return Err(err!(@span, ExpectedToken, kind));
    }
    Ok(())
  }

  #[inline]
  fn advance(&mut self) -> Token {
    self.prev = self.curr;
    self.curr = loop {
      match self.lex.bump() {
        Ok(token) => break token,
        Err(e) => self.errors.push(e),
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
        p.errors.push(e);
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
  p.errors.push(e);

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
  Ok(Decl::make_fn(p.finish(s), name, params, ret, body))
}

fn stmt<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
  match p.kind() {
    t![fn] => {
      let span = p.curr.span;
      let _ = fn_(p)?;
      Err(err!(@span, NoNestedFunctions))
    }
    t![loop] => loop_(p),
    t![let] => let_(p),
    t![break] => break_(p).map(Stmt::from),
    t![continue] => continue_(p).map(Stmt::from),
    t![return] => return_(p).map(Stmt::from),
    t![if] => if_(p).map(Stmt::from),
    t![yield] => yield_(p).map(Stmt::from),
    t!["{"] => block(p).map(Stmt::from),
    _ => assign(p).map(Stmt::from),
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

fn loop_<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
  let s = p.span();

  assert!(p.eat(t![loop]));
  let body = block(p)?;
  Ok(Stmt::make_loop(p.finish(s), body))
}

fn let_<'src>(p: &mut Parser<'src>) -> Result<Stmt<'src>> {
  assert!(p.eat(t![let]));
  let s = p.span();

  let name = ident(p)?;
  let ty = if p.eat(t![:]) { Some(type_(p)?) } else { None };
  p.must(t![=])?;
  let init = expr(p)?;

  Ok(Stmt::make_let(p.finish(s), name, ty, init))
}

fn break_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  assert!(p.eat(t![break]));
  Ok(Expr::make_break(p.prev.span))
}

fn continue_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  assert!(p.eat(t![continue]));
  Ok(Expr::make_continue(p.prev.span))
}

fn return_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let s = p.span();

  assert!(p.eat(t![return]));
  let value = if !p.at_any([t!["}"], t![;]]) {
    Some(expr(p)?)
  } else {
    None
  };
  Ok(Expr::make_return(p.finish(s), value))
}

fn yield_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let s = p.span();

  assert!(p.eat(t![yield]));
  let value = if !p.at_any([t!["}"], t![;]]) {
    Some(expr(p)?)
  } else {
    None
  };
  let span = p.finish(s);
  Ok(Expr::make_yield(span, value))
}

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

  Ok(Expr::make_if(p.finish(s), branches, tail))
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
      Ok(Type::make_empty(p.prev.span))
    }
    t![ident] => {
      let ident = ident(p)?;
      let ty = Type::make_named(ident.span, ident);
      Ok(opt(p, ty))
    }
    t!["["] => {
      let start = p.curr.span.start;
      p.advance();
      let element = type_(p)?;
      let end = p.prev.span.end;
      p.must(t!["]"])?;
      let ty = Type::make_array(start..end, element);
      Ok(opt(p, ty))
    }
    t!["{"] => {
      let start = p.curr.span.start;
      p.advance();
      let key = type_(p)?;
      let ty = if p.eat(t![->]) {
        let value = type_(p)?;
        p.must(t!["}"])?;
        let end = p.prev.span.end;
        Type::make_map(start..end, key, value)
      } else {
        p.must(t!["}"])?;
        let end = p.prev.span.end;
        Type::make_set(start..end, key)
      };
      Ok(opt(p, ty))
    }
    t!["("] => {
      let start = p.curr.span.start;

      let end = p.prev.span.end;
      let params = paren_list(p, type_)?;
      p.must(t![->])?;
      let ret = type_(p)?;
      Ok(Type::make_fn(start..end, params, ret))
    }
    _ => Err(err!(@p.curr.span, UnexpectedToken)),
  }
}

fn opt<'src>(p: &mut Parser<'src>, inner: Type<'src>) -> Type<'src> {
  if p.eat(t![?]) {
    Type::make_opt(inner.span.to(p.prev.span), inner)
  } else {
    inner
  }
}

fn ident<'src>(p: &mut Parser<'src>) -> Result<Ident<'src>> {
  p.must(t![ident])?;

  Ok(Ident {
    span: p.prev.span,
    lexeme: p.lex.lexeme(&p.prev),
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
        p.errors.push(e);
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
  let place = expr_to_place(lhs)?;
  Ok(Expr::make_assign(span, place, op, value))
}

fn expr_to_place(v: Expr<'_>) -> Result<Place<'_>> {
  match v.kind {
    ExprKind::Use(inner) => Ok(inner.place),
    _ => Err(err!(@v.span, InvalidPlace)),
  }
}

fn expr<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  match p.kind() {
    t![break] => break_(p),
    t![continue] => continue_(p),
    t![return] => return_(p),
    t![yield] => yield_(p),
    _ => expr::opt(p),
  }
}

mod expr {
  use super::*;

  fn binary<'src>(lhs: Expr<'src>, op: BinaryOp, rhs: Expr<'src>) -> Expr<'src> {
    Expr::make_binary(lhs.span.to(rhs.span), lhs, op, rhs)
  }

  pub(super) fn opt<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = or(p)?;
    while !p.end() && p.eat(t![??]) {
      let rhs = or(p)?;
      lhs = binary(lhs, binop![??], rhs);
    }
    Ok(lhs)
  }

  pub fn or<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = and(p)?;
    while !p.end() && p.eat(t![||]) {
      let rhs = and(p)?;
      lhs = binary(lhs, binop![||], rhs);
    }
    Ok(lhs)
  }

  fn and<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = eq(p)?;
    while !p.end() && p.eat(t![&&]) {
      let rhs = eq(p)?;
      lhs = binary(lhs, binop![&&], rhs);
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
      lhs = binary(lhs, op, rhs);
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
      lhs = binary(lhs, op, rhs);
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
      lhs = binary(lhs, op, rhs);
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
      lhs = binary(lhs, op, rhs);
    }
    Ok(lhs)
  }

  fn pow<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let mut lhs = pre(p)?;
    while !p.end() && p.eat(t![**]) {
      let rhs = pre(p)?;
      lhs = binary(lhs, binop![**], rhs);
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
    Ok(Expr::make_unary(tok.span.to(rhs.span), op, rhs))
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
    Ok(Expr::make_call(span, target, args))
  }

  fn arg<'src>(p: &mut Parser<'src>) -> Result<Arg<'src>> {
    let value = expr(p)?;
    let (key, value) = if value.as_use().is_some_and(|v| v.place.is_var()) && p.eat(t![:]) {
      let key = Some(value.into_use().unwrap().place.into_var().unwrap());
      let value = expr(p)?;
      (key, value)
    } else {
      (None, value)
    };
    Ok(Arg { key, value })
  }

  fn index<'src>(p: &mut Parser<'src>, parent: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["["]));
    let key = expr(p)?;
    p.must(t!["]"])?;

    Ok(Expr::make_use(p.finish(s), Place::Index { parent, key }))
  }

  fn field<'src>(p: &mut Parser<'src>, parent: Expr<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t![.]));
    let name = ident(p)?;

    Ok(Expr::make_use(p.finish(s), Place::Field { parent, name }))
  }

  fn primary<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    match p.kind() {
      t![int] => int(p),
      t![float] => float(p),
      t![bool] => bool(p),
      t![none] => none(p),
      t![str] => str(p),
      t![if] => if_(p),
      t![do] => do_(p),
      t!["["] => array(p),
      t!["{"] => set_or_map(p),
      t!["("] => group(p),
      t![ident] => use_(p),
      _ => Err(err!(@p.curr.span, UnexpectedToken)),
    }
  }

  fn int<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![int]));
    let span = p.prev.span;
    let v = p
      .lexeme(&p.prev)
      .parse::<i64>()
      .map_err(|_| err!(@span, InvalidInt))?;
    Ok(Expr::make_literal(span, lit!(int, v)))
  }

  fn float<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![float]));
    let span = p.prev.span;
    let v = p
      .lexeme(&p.prev)
      .parse::<f64>()
      .map_err(|_| err!(@span, InvalidInt))?;
    Ok(Expr::make_literal(span, lit!(float, v)))
  }

  fn bool<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![bool]));
    let v = match p.lexeme(&p.prev) {
      "true" => true,
      "false" => false,
      _ => unreachable!(),
    };
    Ok(Expr::make_literal(p.prev.span, lit!(bool, v)))
  }

  fn none<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![none]));
    Ok(Expr::make_literal(p.prev.span, lit!(none)))
  }

  fn str<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![str]));
    // TODO: unescape
    // TODO: fmt
    Ok(Expr::make_literal(
      p.prev.span,
      lit!(str, p.lexeme(&p.prev)),
    ))
  }

  fn do_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    assert!(p.eat(t![do]));
    let s = p.prev.span;
    let body = block(p)?;
    Ok(Expr::make_block(s.to(body.span), body))
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

    Ok(Expr::make_literal(p.finish(s), lit!(array, array)))
  }

  fn set_or_map<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
    let s = p.span();

    assert!(p.eat(t!["{"]));
    if p.end() {
      p.must(t!["}"])?;
      return Ok(Expr::make_literal(p.finish(s), lit!(map, vec![])));
    }

    if p.eat(t!["}"]) {
      return Ok(Expr::make_literal(p.finish(s), lit!(map, vec![])));
    }

    let s = p.span();
    let first_key = expr(p)?;
    if p.eat(t![:]) {
      let first_value = expr(p)?;
      map(p, s, first_key, first_value)
    } else {
      set(p, s, first_key)
    }
  }

  fn map<'src>(
    p: &mut Parser<'src>,
    s: Span,
    first_key: Expr<'src>,
    first_value: Expr<'src>,
  ) -> Result<Expr<'src>> {
    let mut items = vec![(first_key, first_value)];
    while !p.end() && p.eat(t![,]) && !p.at(t!["}"]) {
      let key = expr(p)?;
      p.must(t![:])?;
      let value = expr(p)?;
      items.push((key, value));
    }
    p.must(t!["}"])?;
    Ok(Expr::make_literal(p.finish(s), lit!(map, items)))
  }

  fn set<'src>(p: &mut Parser<'src>, s: Span, first_key: Expr<'src>) -> Result<Expr<'src>> {
    let mut items = vec![first_key];
    while !p.end() && p.eat(t![,]) && !p.at(t!["}"]) {
      let key = expr(p)?;
      items.push(key);
    }
    p.must(t!["}"])?;
    Ok(Expr::make_literal(p.finish(s), lit!(set, items)))
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
    Ok(Expr::make_use(p.prev.span, Place::Var { name }))
  }
}

#[cfg(test)]
mod tests;
