#![allow(clippy::new_without_default)]

#[macro_use]
mod macros;

use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::ty::TypeId;
use crate::Cow;

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
    Set {
      value: Type<'src>,
    },
    Map {
      key: Type<'src>,
      value: Type<'src>,
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
  Expr<'src> (ty: TypeId) {
    Return {
      value: Option<Expr<'src>>,
    },
    Yield {
      value: Option<Expr<'src>>,
    },
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
    Use {
      place: Place<'src>,
    },
    Assign {
      place: Place<'src>,
      op: Option<BinaryOp>,
      value: Expr<'src>,
    },
    Call {
      callee: Expr<'src>,
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

impl<'src> UseExpr<'src> {
  pub fn place(&self) -> &Place {
    &self.place
  }
}

#[derive(Clone)]
pub enum Place<'src> {
  Var {
    name: Ident<'src>,
  },
  Field {
    parent: Expr<'src>,
    name: Ident<'src>,
  },
  Index {
    parent: Expr<'src>,
    key: Expr<'src>,
  },
}

impl<'src> Place<'src> {
  pub fn is_var(&self) -> bool {
    matches!(self, Self::Var { .. })
  }

  pub fn into_var(self) -> Option<Ident<'src>> {
    if let Self::Var { name } = self {
      Some(name)
    } else {
      None
    }
  }
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
  None,
  Int(i64),
  Float(f64),
  Bool(bool),
  String(Cow<'src, str>),
  Array(Array<'src>),
  // TODO: during type check, empty `map` can coerce to `set`
  Set(Vec<Expr<'src>>),
  Map(Vec<(Expr<'src>, Expr<'src>)>),
}

#[derive(Clone)]
pub enum Array<'src> {
  List(Vec<Expr<'src>>),
  Copy(Expr<'src>, Expr<'src>),
}

macro_rules! lit {
  (none) => {
    $crate::ast::Literal::None
  };
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
  (set, $v:expr) => {
    $crate::ast::Literal::Set(($v).into())
  };
  (map, $v:expr) => {
    $crate::ast::Literal::Map(($v).into())
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

impl<'src> From<Block<'src>> for Stmt<'src> {
  fn from(block: Block<'src>) -> Self {
    Stmt::make_expr(block.span, Expr::make_block(block.span, block))
  }
}

impl<'src> From<Expr<'src>> for Stmt<'src> {
  fn from(value: Expr<'src>) -> Self {
    Stmt::make_expr(value.span, value)
  }
}

#[derive(Clone, Copy)]
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

  pub fn raw(s: &'src str) -> Self {
    Self {
      span: Span::empty(),
      lexeme: s,
    }
  }
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

impl Debug for Place<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Var { name } => write!(f, "Var({name:?})"),
      Self::Field { parent, name } => f.debug_tuple("Field").field(parent).field(name).finish(),
      Self::Index { parent, key } => f.debug_tuple("Index").field(parent).field(key).finish(),
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
      Self::None => write!(f, "None"),
      Self::Int(arg0) => write!(f, "Int({arg0:?})"),
      Self::Float(arg0) => write!(f, "Float({arg0:?})"),
      Self::Bool(arg0) => write!(f, "Bool({arg0:?})"),
      Self::String(arg0) => write!(f, "String({arg0:?})"),
      Self::Array(arg0) => Debug::fmt(arg0, f),
      Self::Set(arg0) => f.debug_tuple("Set").field(arg0).finish(),
      Self::Map(arg0) => f.debug_tuple("Map").field(arg0).finish(),
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
      TypeKind::Set(ty) => ty.hash(state),
      TypeKind::Map(ty) => ty.hash(state),
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

impl Hash for SetType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.value.hash(state);
  }
}

impl Hash for MapType<'_> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.key.hash(state);
    self.value.hash(state);
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
