#![allow(clippy::new_without_default)]

mod make;
mod print;

// TODO: finish ast refactor

use std::hash::Hash;

use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::Str;

pub struct Ast<'src, T = ()> {
    pub src: &'src str,
    pub decls: Vec<Decl<'src, T>>,
    pub top_level: Block<'src, T>,
}

pub use decl::Decl;
pub mod decl {
    pub use super::*;

    pub struct Decl<'src, T = ()> {
        pub span: Span,
        pub kind: DeclKind<'src, T>,
    }

    pub enum DeclKind<'src, T = ()> {
        Fn(Box<Fn<'src, T>>),
        Type(Box<Type<'src>>),
    }

    pub struct Fn<'src, T = ()> {
        pub name: Ident<'src>,
        pub params: Vec<Param<'src>>,
        pub ret: Option<TypeExpr<'src>>,
        pub body: Body<'src, T>,
    }

    pub enum Body<'src, T = ()> {
        Extern,
        Block(Block<'src, T>),
    }

    pub struct Type<'src> {
        pub name: Ident<'src>,
        pub fields: Fields<'src>,
    }

    pub enum Fields<'src> {
        Extern,
        Named(Vec<Field<'src>>),
    }

    pub struct Field<'src> {
        pub name: Ident<'src>,
        pub ty: TypeExpr<'src>,
    }
}

pub use stmt::Stmt;
pub mod stmt {
    pub use super::*;

    pub struct Stmt<'src, T = ()> {
        pub span: Span,
        pub kind: StmtKind<'src, T>,
    }

    pub enum StmtKind<'src, T = ()> {
        Let(Box<Let<'src, T>>),
        Loop(Box<Loop<'src, T>>),
        Expr(Expr<'src, T>),
    }

    pub struct Let<'src, T = ()> {
        pub name: Ident<'src>,
        pub ty: Option<TypeExpr<'src>>,
        pub init: Expr<'src, T>,
    }

    pub struct Loop<'src, T = ()> {
        pub body: Block<'src, T>,
    }
}

pub use ty::TypeExpr;
pub mod ty {
    pub use super::*;

    #[derive(Clone)]
    pub struct TypeExpr<'src> {
        pub span: Span,
        pub kind: TypeExprKind<'src>,
    }

    #[derive(Clone)]
    pub enum TypeExprKind<'src> {
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

    pub struct Expr<'src, T = ()> {
        pub span: Span,
        pub ty: T,
        pub kind: ExprKind<'src, T>,
    }

    pub enum ExprKind<'src, T = ()> {
        Return(Box<Return<'src, T>>),
        Break,
        Continue,
        Block(Box<Block<'src, T>>),
        If(Box<If<'src, T>>),
        Binary(Box<Binary<'src, T>>),
        Unary(Box<Unary<'src, T>>),
        Primitive(Box<Primitive<'src>>),
        Array(Box<Array<'src, T>>),
        UseVar(Box<UseVar<'src>>),
        UseField(Box<UseField<'src, T>>),
        UseIndex(Box<UseIndex<'src, T>>),
        AssignVar(Box<AssignVar<'src, T>>),
        AssignField(Box<AssignField<'src, T>>),
        AssignIndex(Box<AssignIndex<'src, T>>),
        Call(Box<Call<'src, T>>),
        MethodCall(Box<MethodCall<'src, T>>),
    }

    pub struct Return<'src, T = ()> {
        pub value: Option<Expr<'src, T>>,
    }

    pub struct Break;

    pub struct Continue;

    pub struct If<'src, T = ()> {
        pub branches: Vec<Branch<'src, T>>,
        pub tail: Option<Block<'src, T>>,
    }

    pub struct Binary<'src, T = ()> {
        pub lhs: Expr<'src, T>,
        pub op: BinaryOp,
        pub rhs: Expr<'src, T>,
    }

    pub struct Unary<'src, T = ()> {
        pub op: UnaryOp,
        pub rhs: Expr<'src, T>,
    }

    pub enum Primitive<'src> {
        Int(i64),
        Num(f64),
        Bool(bool),
        Str(Str<'src>),
    }

    pub struct Array<'src, T = ()> {
        pub items: Vec<Expr<'src, T>>,
    }

    pub struct UseVar<'src> {
        pub name: Ident<'src>,
    }

    pub struct UseField<'src, T = ()> {
        pub parent: Expr<'src, T>,
        pub name: Ident<'src>,
    }

    pub struct UseIndex<'src, T = ()> {
        pub parent: Expr<'src, T>,
        pub key: Expr<'src, T>,
    }

    pub struct AssignVar<'src, T = ()> {
        pub name: Ident<'src>,
        pub op: Option<BinaryOp>,
        pub value: Expr<'src, T>,
    }

    pub struct AssignField<'src, T = ()> {
        pub parent: Expr<'src, T>,
        pub name: Ident<'src>,
        pub op: Option<BinaryOp>,
        pub value: Expr<'src, T>,
    }

    pub struct AssignIndex<'src, T = ()> {
        pub parent: Expr<'src, T>,
        pub key: Expr<'src, T>,
        pub op: Option<BinaryOp>,
        pub value: Expr<'src, T>,
    }

    pub struct Call<'src, T = ()> {
        pub callee: Expr<'src, T>,
        pub args: Vec<Arg<'src, T>>,
    }

    pub struct MethodCall<'src, T = ()> {
        pub receiver: Expr<'src, T>,
        pub method: Ident<'src>,
        pub args: Vec<Arg<'src, T>>,
    }
}

pub struct Param<'src> {
    pub name: Ident<'src>,
    pub ty: TypeExpr<'src>,
}

pub struct Branch<'src, T = ()> {
    pub cond: Expr<'src, T>,
    pub body: Block<'src, T>,
}

pub struct Arg<'src, T = ()> {
    pub key: Option<Ident<'src>>,
    pub value: Expr<'src, T>,
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

pub struct Block<'src, T = ()> {
    pub span: Span,
    pub body: Vec<Stmt<'src, T>>,
    pub tail: Option<Expr<'src, T>>,
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

impl Hash for Ident<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
    }
}
