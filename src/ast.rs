#![allow(clippy::new_without_default)]

#[macro_use]
mod macros;

use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::util::JoinIter;
use crate::Str;

pub(crate) struct IdGen<T: Next>(T);

impl<T: Copy + Next + Default> IdGen<T> {
  pub fn new() -> Self {
    Self(T::default())
  }

  pub fn next(&mut self) -> T {
    let v = self.0;
    self.0 = self.0.next();
    v
  }
}

#[derive(Clone)]
pub struct Ast<'src> {
  pub src: &'src str,
  pub decls: Vec<Decl<'src>>,
  pub top_level: Block<'src>,
}

syntax_node! {
  Decl<'src> {
    Fn {
      name: Ident<'src>,
      params: Vec<Param<'src>>,
      ret: Option<Type<'src>>,
      body: Block<'src>,
    }
  }
}

syntax_node! {
  Stmt<'src> {
    Let {
      name: Ident<'src>,
      ty: Option<Type<'src>>,
      init: Expr<'src>,
    },
    Loop {
      body: Block<'src>,
    },
    Expr {
      inner: Expr<'src>,
    },
  }
}

syntax_node! {
  Type<'src> {
    Empty,
    Named {
      name: Ident<'src>,
    },
    Array {
      item: Type<'src>,
    },
    Fn {
      params: Vec<Type<'src>>,
      ret: Type<'src>,
    },
    Opt {
      inner: Type<'src>,
    },
  }
}

syntax_node! {
  Expr<'src> {
    Return {
      value: Option<Expr<'src>>,
    },
    /* Yield {
      value: Option<Expr<'src>>,
    }, */
    Break,
    Continue,
    Block {
      inner: Block<'src>,
    },
    If {
      branches: Vec<Branch<'src>>,
      tail: Option<Block<'src>>,
    },
    Binary {
      left: Expr<'src>,
      op: BinaryOp,
      right: Expr<'src>,
    },
    Unary {
      op: UnaryOp,
      right: Expr<'src>,
    },
    Literal {
      value: Literal<'src>,
    },
    UseVar {
      name: Ident<'src>,
    },
    UseField {
      parent: Expr<'src>,
      name: Ident<'src>,
    },
    UseIndex {
      parent: Expr<'src>,
      key: Expr<'src>,
    },
    AssignVar {
      name: Ident<'src>,
      op: Option<BinaryOp>,
      value: Expr<'src>,
    },
    AssignField {
      parent: Expr<'src>,
      name: Ident<'src>,
      op: Option<BinaryOp>,
      value: Expr<'src>,
    },
    AssignIndex {
      parent: Expr<'src>,
      key: Expr<'src>,
      op: Option<BinaryOp>,
      value: Expr<'src>,
    },
    Call {
      callee: Expr<'src>,
      args: Vec<Arg<'src>>,
    },
    MethodCall {
      receiver: Expr<'src>,
      method: Ident<'src>,
      args: Vec<Arg<'src>>,
    }
  }
}

#[derive(Clone, Hash)]
pub struct Param<'src> {
  pub name: Ident<'src>,
  pub ty: Type<'src>,
}

#[derive(Clone)]
pub struct Branch<'src> {
  pub cond: Expr<'src>,
  pub body: Block<'src>,
}

#[derive(Clone)]
pub struct Arg<'src> {
  pub key: Option<Ident<'src>>,
  pub value: Expr<'src>,
}

#[derive(Clone, Copy)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Pow,
  Eq,
  Ne,
  Gt,
  Lt,
  Ge,
  Le,
  And,
  Or,
  Opt,
}

macro_rules! binop {
  [+] => ($crate::ast::BinaryOp::Add);
  [-] => ($crate::ast::BinaryOp::Sub);
  [*] => ($crate::ast::BinaryOp::Mul);
  [/] => ($crate::ast::BinaryOp::Div);
  [%] => ($crate::ast::BinaryOp::Rem);
  [**] => ($crate::ast::BinaryOp::Pow);
  [==] => ($crate::ast::BinaryOp::Eq);
  [!=] => ($crate::ast::BinaryOp::Ne);
  [>] => ($crate::ast::BinaryOp::Gt);
  [<] => ($crate::ast::BinaryOp::Lt);
  [>=] => ($crate::ast::BinaryOp::Ge);
  [<=] => ($crate::ast::BinaryOp::Le);
  [&&] => ($crate::ast::BinaryOp::And);
  [||] => ($crate::ast::BinaryOp::Or);
  [??] => ($crate::ast::BinaryOp::Opt);
}

#[derive(Clone, Copy)]
pub enum UnaryOp {
  Minus,
  Not,
  Opt,
}

macro_rules! unop {
  [-] => ($crate::ast::UnaryOp::Minus);
  [!] => ($crate::ast::UnaryOp::Not);
  [?] => ($crate::ast::UnaryOp::Opt);
}

#[derive(Clone)]
pub enum Literal<'src> {
  Int(i64),
  Float(f64),
  Bool(bool),
  String(Str<'src>),
  Array(Array<'src>),
}

#[derive(Clone)]
pub enum Array<'src> {
  List(Vec<Expr<'src>>),
  Copy(Expr<'src>, Expr<'src>),
}

macro_rules! lit {
  (int, $v:expr) => {
    $crate::ast::Literal::Int(($v).into())
  };
  (float, $v:expr) => {
    $crate::ast::Literal::Float(($v).into())
  };
  (bool, $v:expr) => {
    $crate::ast::Literal::Bool(($v).into())
  };
  (str, $v:expr) => {
    $crate::ast::Literal::String(($v).into())
  };
  (array, $v:expr) => {
    $crate::ast::Literal::Array(($v).into())
  };
}

impl Default for Literal<'_> {
  fn default() -> Self {
    Self::Int(0)
  }
}

#[derive(Default, Clone)]
pub struct Block<'src> {
  pub span: Span,
  pub body: Vec<Stmt<'src>>,
  pub tail: Option<Expr<'src>>,
}

#[derive(Clone)]
pub struct Ident<'src> {
  pub span: Span,
  pub lexeme: &'src str,
}

impl<'src> Ident<'src> {
  pub fn from_token(l: &Lexer<'src>, t: &Token) -> Self {
    assert!(matches!(t.kind, TokenKind::Ident));
    Self {
      span: t.span,
      lexeme: l.lexeme(t),
    }
  }

  pub fn raw(lexeme: &'src str) -> Self {
    Self {
      span: Span::empty(),
      lexeme,
    }
  }

  pub fn as_str(&self) -> &'src str {
    self.lexeme
  }
}

pub(crate) trait Next: Sized {
  fn next(&self) -> Self;
}

trait WrapBox {
  type Boxed;

  fn wrap_box(self) -> Self::Boxed;
}

trait UnwrapBox {
  type Unboxed;

  fn unwrap_box(self) -> Self::Unboxed;
}

impl Debug for Ast<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Ast")
      .field("decls", &self.decls)
      .field("top_level", &self.top_level)
      .finish()
  }
}

impl Debug for Param<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_tuple("Param")
      .field(&self.name)
      .field(&self.ty)
      .finish()
  }
}

impl Debug for Branch<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_tuple("Branch")
      .field(&self.cond)
      .field(&self.body)
      .finish()
  }
}

impl Debug for Arg<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut t = f.debug_tuple("Arg");
    match &self.key {
      Some(key) => t.field(key).field(&self.value).finish(),
      None => t.field(&self.value).finish(),
    }
  }
}

impl Debug for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Add => f.write_str("Op(+)"),
      Self::Sub => f.write_str("Op(-)"),
      Self::Mul => f.write_str("Op(*)"),
      Self::Div => f.write_str("Op(/)"),
      Self::Rem => f.write_str("Op(%)"),
      Self::Pow => f.write_str("Op(**)"),
      Self::Eq => f.write_str("Op(==)"),
      Self::Ne => f.write_str("Op(!=)"),
      Self::Gt => f.write_str("Op(>)"),
      Self::Lt => f.write_str("Op(<)"),
      Self::Ge => f.write_str("Op(>=)"),
      Self::Le => f.write_str("Op(<=)"),
      Self::And => f.write_str("Op(&&)"),
      Self::Or => f.write_str("Op(||)"),
      Self::Opt => f.write_str("Op(??)"),
    }
  }
}

impl Debug for UnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Minus => f.write_str("Op(-)"),
      Self::Not => f.write_str("Op(!)"),
      Self::Opt => f.write_str("Op(?)"),
    }
  }
}

impl Debug for Block<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Block")
      .field("body", &self.body)
      .field("tail", &self.tail)
      .finish()
  }
}

impl Debug for Ident<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Ident({:?})", self.lexeme)
  }
}

impl Debug for Literal<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Int(arg0) => write!(f, "Int({arg0:?})"),
      Self::Float(arg0) => write!(f, "Float({arg0:?})"),
      Self::Bool(arg0) => write!(f, "Bool({arg0:?})"),
      Self::String(arg0) => write!(f, "String({arg0:?})"),
      Self::Array(arg0) => Debug::fmt(arg0, f),
    }
  }
}

impl Debug for Array<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::List(arg0) => f.debug_tuple("Array").field(arg0).finish(),
      Self::Copy(arg0, arg1) => f
        .debug_struct("Array")
        .field("item", arg0)
        .field("len", arg1)
        .finish(),
    }
  }
}

impl Hash for Type<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.kind.hash(state);
  }
}

impl Hash for TypeKind<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    core::mem::discriminant(self).hash(state);
    match self {
      TypeKind::Empty(ty) => ty.hash(state),
      TypeKind::Named(ty) => ty.hash(state),
      TypeKind::Array(ty) => ty.hash(state),
      TypeKind::Fn(ty) => ty.hash(state),
      TypeKind::Opt(ty) => ty.hash(state),
    }
  }
}

impl Hash for EmptyType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.0.hash(state);
  }
}

impl Hash for NamedType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.name.hash(state);
  }
}

impl Hash for ArrayType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.item.hash(state);
  }
}

impl Hash for FnType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.params.hash(state);
    self.ret.hash(state);
  }
}

impl Hash for OptType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.inner.hash(state);
  }
}

impl Hash for Ident<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.lexeme.hash(state);
  }
}

impl Display for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(match self {
      BinaryOp::Add => "+",
      BinaryOp::Sub => "-",
      BinaryOp::Mul => "*",
      BinaryOp::Div => "/",
      BinaryOp::Rem => "%",
      BinaryOp::Pow => "**",
      BinaryOp::Eq => "==",
      BinaryOp::Ne => "!=",
      BinaryOp::Gt => ">",
      BinaryOp::Lt => "<",
      BinaryOp::Ge => ">=",
      BinaryOp::Le => "<=",
      BinaryOp::And => "&&",
      BinaryOp::Or => "||",
      BinaryOp::Opt => "??",
    })
  }
}

impl Display for UnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(match self {
      UnaryOp::Minus => "-",
      UnaryOp::Not => "!",
      UnaryOp::Opt => "?",
    })
  }
}

impl Display for Ident<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(self.lexeme)
  }
}

impl Display for Type<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.kind {
      TypeKind::Empty(_) => f.write_str("_"),
      TypeKind::Named(ty) => f.write_str(ty.name.lexeme),
      TypeKind::Array(ty) => write!(f, "[{}]", ty.item),
      TypeKind::Fn(ty) => write!(f, "({}) -> {}", ty.params.iter().join(", "), ty.ret),
      TypeKind::Opt(ty) => write!(f, "{}?", ty.inner),
    }
  }
}
