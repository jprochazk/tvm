#![allow(clippy::new_ret_no_self)]

use super::*;

impl<'src> decl::Fn<'src> {
    pub fn new(
        span: impl Into<Span>,
        name: Ident<'src>,
        params: Vec<Param<'src>>,
        ret: Option<Ty<'src>>,
        body: decl::Body<'src>,
    ) -> Decl<'src> {
        Decl {
            span: span.into(),
            kind: decl::DeclKind::Fn(Box::new(Self {
                name,
                params,
                ret,
                body,
            })),
        }
    }
}

impl<'src> decl::TypeDef<'src> {
    pub fn new(span: impl Into<Span>, name: Ident<'src>, fields: decl::Fields<'src>) -> Decl<'src> {
        Decl {
            span: span.into(),
            kind: decl::DeclKind::Type(Box::new(Self { name, fields })),
        }
    }
}

impl<'src> stmt::Let<'src> {
    pub fn new(
        span: impl Into<Span>,
        name: Ident<'src>,
        ty: Option<Ty<'src>>,
        init: Expr<'src>,
    ) -> Stmt<'src> {
        Stmt {
            span: span.into(),
            kind: stmt::StmtKind::Let(Box::new(Self { name, ty, init })),
        }
    }
}

impl<'src> stmt::Loop<'src> {
    pub fn new(span: impl Into<Span>, body: Block<'src>) -> Stmt<'src> {
        Stmt {
            span: span.into(),
            kind: stmt::StmtKind::Loop(Box::new(Self { body })),
        }
    }
}

impl<'src> ty::Empty {
    pub fn new(span: impl Into<Span>) -> Ty<'src> {
        Ty {
            span: span.into(),
            kind: ty::TyKind::Empty,
        }
    }
}

impl<'src> ty::Named<'src> {
    pub fn new(span: impl Into<Span>, name: Ident<'src>) -> Ty<'src> {
        Ty {
            span: span.into(),
            kind: ty::TyKind::Named(ty::Named { name }),
        }
    }
}

impl<'src> Expr<'src> {
    pub fn into_stmt(self) -> Stmt<'src> {
        Stmt {
            span: self.span,
            kind: stmt::StmtKind::Expr(self),
        }
    }
}

impl<'src> expr::Break {
    pub fn new(span: impl Into<Span>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Break,
        }
    }
}

impl<'src> expr::Continue {
    pub fn new(span: impl Into<Span>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Continue,
        }
    }
}

impl<'src> expr::Return<'src> {
    pub fn new(span: impl Into<Span>, value: Option<Expr<'src>>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Return(Box::new(expr::Return { value })),
        }
    }
}

impl<'src> expr::If<'src> {
    pub fn new(
        span: impl Into<Span>,
        if_token: Span,
        branches: Vec<Branch<'src>>,
        tail: Option<Block<'src>>,
        is_stmt: bool,
    ) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::If(Box::new(expr::If {
                if_token,
                branches,
                tail,
                is_stmt,
            })),
        }
    }
}

impl<'src> expr::Binary<'src> {
    pub fn new(lhs: Expr<'src>, op: BinaryOp, rhs: Expr<'src>) -> Expr<'src> {
        Expr {
            span: lhs.span.to(rhs.span),

            kind: expr::ExprKind::Binary(Box::new(expr::Binary { lhs, op, rhs })),
        }
    }
}

impl<'src> expr::Unary<'src> {
    pub fn new(span: impl Into<Span>, op: UnaryOp, rhs: Expr<'src>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Unary(Box::new(expr::Unary { op, rhs })),
        }
    }
}

impl<'src> expr::UseVar<'src> {
    pub fn new(span: impl Into<Span>, name: Ident<'src>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::UseVar(Box::new(expr::UseVar { name })),
        }
    }
}

impl<'src> expr::UseField<'src> {
    pub fn new(span: impl Into<Span>, parent: Expr<'src>, name: Ident<'src>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::UseField(Box::new(expr::UseField { parent, name })),
        }
    }
}

impl<'src> expr::UseIndex<'src> {
    pub fn new(span: impl Into<Span>, parent: Expr<'src>, key: Expr<'src>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::UseIndex(Box::new(expr::UseIndex { parent, key })),
        }
    }
}

impl<'src> expr::AssignVar<'src> {
    pub fn new(
        span: impl Into<Span>,
        name: Ident<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src>,
    ) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::AssignVar(Box::new(expr::AssignVar { name, op, value })),
        }
    }
}

impl<'src> expr::AssignField<'src> {
    pub fn new(
        span: impl Into<Span>,
        parent: Expr<'src>,
        name: Ident<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src>,
    ) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::AssignField(Box::new(expr::AssignField {
                parent,
                name,
                op,
                value,
            })),
        }
    }
}

impl<'src> expr::AssignIndex<'src> {
    pub fn new(
        span: impl Into<Span>,
        parent: Expr<'src>,
        key: Expr<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src>,
    ) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::AssignIndex(Box::new(expr::AssignIndex {
                parent,
                key,
                op,
                value,
            })),
        }
    }
}

impl<'src> expr::Call<'src> {
    pub fn new(span: impl Into<Span>, callee: Expr<'src>, args: Vec<Arg<'src>>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Call(Box::new(expr::Call { callee, args })),
        }
    }
}

impl<'src> expr::MethodCall<'src> {
    pub fn new(
        span: impl Into<Span>,
        receiver: Expr<'src>,
        method: Ident<'src>,
        args: Vec<Arg<'src>>,
    ) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::MethodCall(Box::new(expr::MethodCall {
                receiver,
                method,
                args,
            })),
        }
    }
}

impl<'src> expr::Array<'src> {
    pub fn new(span: impl Into<Span>, items: Vec<Expr<'src>>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Array(Box::new(Self { items })),
        }
    }
}

impl<'src> expr::Primitive<'src> {
    pub fn int(span: impl Into<Span>, value: i64) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Int(value))),
        }
    }

    pub fn num(span: impl Into<Span>, value: f64n) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Num(value))),
        }
    }

    pub fn bool(span: impl Into<Span>, value: bool) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Bool(value))),
        }
    }

    pub fn str(span: impl Into<Span>, value: impl Into<Str<'src>>) -> Expr<'src> {
        Expr {
            span: span.into(),

            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Str(value.into()))),
        }
    }
}

impl<'src> Block<'src> {
    pub fn new(span: Span, body: Vec<Stmt<'src>>, tail: Option<Expr<'src>>) -> Block<'src> {
        Block { span, body, tail }
    }

    pub fn into_stmt(self) -> Stmt<'src> {
        Stmt {
            span: self.span,
            kind: stmt::StmtKind::Expr(self.into_expr()),
        }
    }

    pub fn into_expr(self) -> Expr<'src> {
        Expr {
            span: self.span,

            kind: expr::ExprKind::Block(Box::new(self)),
        }
    }
}
