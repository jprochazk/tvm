#![allow(clippy::new_without_default)]

mod make;
mod print;

use std::hash::Hash;

use crate::lex::Span;
use crate::vm2::value::f64n;
use crate::Str;

pub struct Ast<'src> {
    pub src: &'src str,
    pub decls: Vec<Decl<'src>>,
    pub top_level: Block<'src>,
}

pub use decl::Decl;
pub mod decl {
    pub use super::*;

    pub struct Decl<'src> {
        pub span: Span,
        pub kind: DeclKind<'src>,
    }

    pub enum DeclKind<'src> {
        Fn(Box<Fn<'src>>),
        Type(Box<TypeDef<'src>>),
    }

    pub struct Fn<'src> {
        pub name: Ident<'src>,
        pub params: Vec<Param<'src>>,
        pub ret: Option<Ty<'src>>,
        pub body: Body<'src>,
    }

    pub enum Body<'src> {
        Extern,
        Block(Block<'src>),
    }

    impl Body<'_> {
        pub fn is_extern(&self) -> bool {
            matches!(self, Body::Extern)
        }
    }

    pub struct TypeDef<'src> {
        pub name: Ident<'src>,
        pub fields: Fields<'src>,
    }

    pub enum Fields<'src> {
        Extern,
        Named(Vec<Field<'src>>),
    }

    pub struct Field<'src> {
        pub name: Ident<'src>,
        pub ty: Ty<'src>,
    }
}

pub use stmt::Stmt;
pub mod stmt {
    pub use super::*;

    pub struct Stmt<'src> {
        pub span: Span,
        pub kind: StmtKind<'src>,
    }

    pub enum StmtKind<'src> {
        Let(Box<Let<'src>>),
        Loop(Box<Loop<'src>>),
        Expr(Expr<'src>),
    }

    pub struct Let<'src> {
        pub name: Ident<'src>,
        pub ty: Option<Ty<'src>>,
        pub init: Expr<'src>,
    }

    pub struct Loop<'src> {
        pub body: Block<'src>,
    }
}

pub use ty::Ty;
pub mod ty {
    pub use super::*;

    #[derive(Clone)]
    pub struct Ty<'src> {
        pub span: Span,
        pub kind: TyKind<'src>,
    }

    #[derive(Clone)]
    pub enum TyKind<'src> {
        Empty,
        Named(Named<'src>),
    }

    #[derive(Clone)]
    pub struct Empty;

    #[derive(Clone)]
    pub struct Named<'src> {
        pub name: Ident<'src>,
    }
}

pub use expr::Expr;
pub mod expr {
    pub use super::*;

    pub struct Expr<'src> {
        pub span: Span,
        pub kind: ExprKind<'src>,
    }

    pub enum ExprKind<'src> {
        Return(Box<Return<'src>>),
        Break,
        Continue,
        Block(Box<Block<'src>>),
        If(Box<If<'src>>),
        Binary(Box<Binary<'src>>),
        Unary(Box<Unary<'src>>),
        Primitive(Box<Primitive<'src>>),
        Array(Box<Array<'src>>),
        UseVar(Box<UseVar<'src>>),
        UseField(Box<UseField<'src>>),
        UseIndex(Box<UseIndex<'src>>),
        AssignVar(Box<AssignVar<'src>>),
        AssignField(Box<AssignField<'src>>),
        AssignIndex(Box<AssignIndex<'src>>),
        Call(Box<Call<'src>>),
        MethodCall(Box<MethodCall<'src>>),
    }

    pub struct Return<'src> {
        pub value: Option<Expr<'src>>,
    }

    pub struct Break;

    pub struct Continue;

    pub struct If<'src> {
        pub if_token: Span,
        pub branches: Vec<Branch<'src>>,
        pub tail: Option<Block<'src>>,
    }

    pub struct Binary<'src> {
        pub lhs: Expr<'src>,
        pub op: BinaryOp,
        pub rhs: Expr<'src>,
    }

    pub struct Unary<'src> {
        pub op: UnaryOp,
        pub rhs: Expr<'src>,
    }

    pub enum Primitive<'src> {
        Int(i64),
        Num(f64n),
        Bool(bool),
        Str(Str<'src>),
    }

    pub struct Array<'src> {
        pub items: Vec<Expr<'src>>,
    }

    pub struct UseVar<'src> {
        pub name: Ident<'src>,
    }

    pub struct UseField<'src> {
        pub parent: Expr<'src>,
        pub name: Ident<'src>,
    }

    pub struct UseIndex<'src> {
        pub parent: Expr<'src>,
        pub key: Expr<'src>,
    }

    pub struct AssignVar<'src> {
        pub name: Ident<'src>,
        pub op: Option<BinaryOp>,
        pub value: Expr<'src>,
    }

    pub struct AssignField<'src> {
        pub parent: Expr<'src>,
        pub name: Ident<'src>,
        pub op: Option<BinaryOp>,
        pub value: Expr<'src>,
    }

    pub struct AssignIndex<'src> {
        pub parent: Expr<'src>,
        pub key: Expr<'src>,
        pub op: Option<BinaryOp>,
        pub value: Expr<'src>,
    }

    pub struct Call<'src> {
        pub callee: Expr<'src>,
        pub args: Vec<Arg<'src>>,
    }

    pub struct MethodCall<'src> {
        pub receiver: Expr<'src>,
        pub method: Ident<'src>,
        pub args: Vec<Arg<'src>>,
    }
}

pub struct Param<'src> {
    pub name: Ident<'src>,
    pub ty: Ty<'src>,
}

pub struct Branch<'src> {
    pub cond: Expr<'src>,
    pub body: Block<'src>,
}

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

pub struct Block<'src> {
    pub span: Span,
    pub body: Vec<Stmt<'src>>,
    pub tail: Option<Expr<'src>>,
}

#[derive(Clone, Copy)]
pub struct Ident<'src> {
    pub span: Span,
    pub lexeme: &'src str,
}

impl<'src> Ident<'src> {
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

impl PartialEq for Ident<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme
    }
}
impl Eq for Ident<'_> {}
impl PartialOrd for Ident<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ord::cmp(self, other))
    }
}
impl Ord for Ident<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.lexeme.cmp(other.lexeme)
    }
}
impl Hash for Ident<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
    }
}

impl AsRef<str> for Ident<'_> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl std::borrow::Borrow<str> for Ident<'_> {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}
impl std::ops::Deref for Ident<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
