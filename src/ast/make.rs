#![allow(clippy::new_ret_no_self)]

use super::*;

impl<'src, T> decl::Fn<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        name: Ident<'src>,
        params: Vec<Param<'src>>,
        ret: Option<Type<'src>>,
        body: Block<'src, T>,
    ) -> Decl<'src, T> {
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

impl<'src, T> stmt::Let<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        name: Ident<'src>,
        ty: Option<Type<'src>>,
        init: Expr<'src, T>,
    ) -> Stmt<'src, T> {
        Stmt {
            span: span.into(),
            kind: stmt::StmtKind::Let(Box::new(Self { name, ty, init })),
        }
    }
}

impl<'src, T> stmt::Loop<'src, T> {
    pub fn new(span: impl Into<Span>, body: Block<'src, T>) -> Stmt<'src, T> {
        Stmt {
            span: span.into(),
            kind: stmt::StmtKind::Loop(Box::new(Self { body })),
        }
    }
}

impl<'src> ty::Empty {
    pub fn new(span: impl Into<Span>) -> Type<'src> {
        Type {
            span: span.into(),
            kind: ty::TypeKind::Empty,
        }
    }
}

impl<'src> ty::Named<'src> {
    pub fn new(span: impl Into<Span>, name: Ident<'src>) -> Type<'src> {
        Type {
            span: span.into(),
            kind: ty::TypeKind::Named(ty::Named { name }),
        }
    }
}

impl<'src> ty::Opt<'src> {
    pub fn new(span: impl Into<Span>, inner: Type<'src>) -> Type<'src> {
        Type {
            span: span.into(),
            kind: ty::TypeKind::Opt(Box::new(ty::Opt { inner })),
        }
    }
}

impl<'src> ty::Array<'src> {
    pub fn new(span: impl Into<Span>, item: Type<'src>) -> Type<'src> {
        Type {
            span: span.into(),
            kind: ty::TypeKind::Array(Box::new(ty::Array { item })),
        }
    }
}

impl<'src> ty::Fn<'src> {
    pub fn new(span: impl Into<Span>, params: Vec<Type<'src>>, ret: Type<'src>) -> Type<'src> {
        Type {
            span: span.into(),
            kind: ty::TypeKind::Fn(Box::new(ty::Fn { params, ret })),
        }
    }
}

impl<'src, T> Expr<'src, T> {
    pub fn into_stmt(self) -> Stmt<'src, T> {
        Stmt {
            span: self.span,
            kind: stmt::StmtKind::Expr(self),
        }
    }
}

impl<'src> expr::Break {
    pub fn new<T>(span: impl Into<Span>, ty: T) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Break,
        }
    }
}

impl<'src> expr::Break {
    pub fn with<T>(span: impl Into<Span>, ty: T) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Break,
        }
    }
}

impl<'src> expr::Continue {
    pub fn new<T>(span: impl Into<Span>, ty: T) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Continue,
        }
    }
}

impl<'src> expr::Continue {
    pub fn with<T>(span: impl Into<Span>, ty: T) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Continue,
        }
    }
}

impl<'src, T: Default> expr::Return<'src, T> {
    pub fn new(span: impl Into<Span>, value: Option<Expr<'src, T>>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Return(Box::new(expr::Return { value })),
        }
    }
}

impl<'src, T> expr::Return<'src, T> {
    pub fn with(span: impl Into<Span>, ty: T, value: Option<Expr<'src, T>>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Return(Box::new(expr::Return { value })),
        }
    }
}

impl<'src, T: Default> expr::If<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        branches: Vec<Branch<'src, T>>,
        tail: Option<Block<'src, T>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::If(Box::new(expr::If { branches, tail })),
        }
    }
}

impl<'src, T> expr::If<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        branches: Vec<Branch<'src, T>>,
        tail: Option<Block<'src, T>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::If(Box::new(expr::If { branches, tail })),
        }
    }
}

impl<'src, T: Default> expr::Binary<'src, T> {
    pub fn new(lhs: Expr<'src, T>, op: BinaryOp, rhs: Expr<'src, T>) -> Expr<'src, T> {
        Expr {
            span: lhs.span.to(rhs.span),
            ty: Default::default(),
            kind: expr::ExprKind::Binary(Box::new(expr::Binary { lhs, op, rhs })),
        }
    }
}

impl<'src, T> expr::Binary<'src, T> {
    pub fn with(ty: T, lhs: Expr<'src, T>, op: BinaryOp, rhs: Expr<'src, T>) -> Expr<'src, T> {
        Expr {
            span: lhs.span.to(rhs.span),
            ty,
            kind: expr::ExprKind::Binary(Box::new(expr::Binary { lhs, op, rhs })),
        }
    }
}

impl<'src, T: Default> expr::Unary<'src, T> {
    pub fn new(span: impl Into<Span>, op: UnaryOp, rhs: Expr<'src, T>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Unary(Box::new(expr::Unary { op, rhs })),
        }
    }
}

impl<'src, T> expr::Unary<'src, T> {
    pub fn with(span: impl Into<Span>, ty: T, op: UnaryOp, rhs: Expr<'src, T>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Unary(Box::new(expr::Unary { op, rhs })),
        }
    }
}

impl<'src> expr::UseVar<'src> {
    pub fn new<T: Default>(span: impl Into<Span>, name: Ident<'src>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::UseVar(Box::new(expr::UseVar { name })),
        }
    }
}

impl<'src> expr::UseVar<'src> {
    pub fn with<T>(span: impl Into<Span>, ty: T, name: Ident<'src>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::UseVar(Box::new(expr::UseVar { name })),
        }
    }
}

impl<'src, T: Default> expr::UseField<'src, T> {
    pub fn new(span: impl Into<Span>, parent: Expr<'src, T>, name: Ident<'src>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::UseField(Box::new(expr::UseField { parent, name })),
        }
    }
}

impl<'src, T> expr::UseField<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        parent: Expr<'src, T>,
        name: Ident<'src>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::UseField(Box::new(expr::UseField { parent, name })),
        }
    }
}

impl<'src, T: Default> expr::UseIndex<'src, T> {
    pub fn new(span: impl Into<Span>, parent: Expr<'src, T>, key: Expr<'src, T>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::UseIndex(Box::new(expr::UseIndex { parent, key })),
        }
    }
}

impl<'src, T> expr::UseIndex<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        parent: Expr<'src, T>,
        key: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::UseIndex(Box::new(expr::UseIndex { parent, key })),
        }
    }
}

impl<'src, T: Default> expr::AssignVar<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        name: Ident<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::AssignVar(Box::new(expr::AssignVar { name, op, value })),
        }
    }
}

impl<'src, T> expr::AssignVar<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        name: Ident<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::AssignVar(Box::new(expr::AssignVar { name, op, value })),
        }
    }
}

impl<'src, T: Default> expr::AssignField<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        parent: Expr<'src, T>,
        name: Ident<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::AssignField(Box::new(expr::AssignField {
                parent,
                name,
                op,
                value,
            })),
        }
    }
}

impl<'src, T> expr::AssignField<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        parent: Expr<'src, T>,
        name: Ident<'src>,
        op: Option<BinaryOp>,
        value: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::AssignField(Box::new(expr::AssignField {
                parent,
                name,
                op,
                value,
            })),
        }
    }
}

impl<'src, T: Default> expr::AssignIndex<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        parent: Expr<'src, T>,
        key: Expr<'src, T>,
        op: Option<BinaryOp>,
        value: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::AssignIndex(Box::new(expr::AssignIndex {
                parent,
                key,
                op,
                value,
            })),
        }
    }
}

impl<'src, T> expr::AssignIndex<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        parent: Expr<'src, T>,
        key: Expr<'src, T>,
        op: Option<BinaryOp>,
        value: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::AssignIndex(Box::new(expr::AssignIndex {
                parent,
                key,
                op,
                value,
            })),
        }
    }
}

impl<'src, T: Default> expr::Call<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        callee: Expr<'src, T>,
        args: Vec<Arg<'src, T>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Call(Box::new(expr::Call { callee, args })),
        }
    }
}

impl<'src, T> expr::Call<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        callee: Expr<'src, T>,
        args: Vec<Arg<'src, T>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Call(Box::new(expr::Call { callee, args })),
        }
    }
}

impl<'src, T: Default> expr::MethodCall<'src, T> {
    pub fn new(
        span: impl Into<Span>,
        receiver: Expr<'src, T>,
        method: Ident<'src>,
        args: Vec<Arg<'src, T>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::MethodCall(Box::new(expr::MethodCall {
                receiver,
                method,
                args,
            })),
        }
    }
}

impl<'src, T> expr::MethodCall<'src, T> {
    pub fn with(
        span: impl Into<Span>,
        ty: T,
        receiver: Expr<'src, T>,
        method: Ident<'src>,
        args: Vec<Arg<'src, T>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::MethodCall(Box::new(expr::MethodCall {
                receiver,
                method,
                args,
            })),
        }
    }
}

impl<'src, T: Default> expr::Array<'src, T> {
    pub fn into_expr(self, span: impl Into<Span>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Array(Box::new(self)),
        }
    }
}

impl<'src, T> expr::Array<'src, T> {
    pub fn csv_with(span: impl Into<Span>, ty: T, items: Vec<Expr<'src, T>>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Array(Box::new(expr::Array::Csv(items))),
        }
    }

    pub fn len_with(
        span: impl Into<Span>,
        ty: T,
        item: Expr<'src, T>,
        len: Expr<'src, T>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Array(Box::new(expr::Array::Len(item, len))),
        }
    }
}

impl<'src> expr::Primitive<'src> {
    pub fn new_int<T: Default>(span: impl Into<Span>, value: i64) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Int(value))),
        }
    }

    pub fn new_num<T: Default>(span: impl Into<Span>, value: f64) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Num(value))),
        }
    }

    pub fn new_bool<T: Default>(span: impl Into<Span>, value: bool) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Bool(value))),
        }
    }

    pub fn new_str<T: Default>(
        span: impl Into<Span>,
        value: impl Into<Str<'src>>,
    ) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty: Default::default(),
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Str(value.into()))),
        }
    }
}

impl<'src> expr::Primitive<'src> {
    pub fn int_with<T>(span: impl Into<Span>, ty: T, value: i64) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Int(value))),
        }
    }

    pub fn num_with<T>(span: impl Into<Span>, ty: T, value: f64) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Num(value))),
        }
    }

    pub fn bool_with<T>(span: impl Into<Span>, ty: T, value: bool) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Bool(value))),
        }
    }

    pub fn str_with<T>(span: impl Into<Span>, ty: T, value: impl Into<Str<'src>>) -> Expr<'src, T> {
        Expr {
            span: span.into(),
            ty,
            kind: expr::ExprKind::Primitive(Box::new(expr::Primitive::Str(value.into()))),
        }
    }
}

impl<'src, T> Block<'src, T> {
    pub fn new(
        span: Span,
        body: Vec<Stmt<'src, T>>,
        tail: Option<Expr<'src, T>>,
    ) -> Block<'src, T> {
        Block { span, body, tail }
    }

    pub fn into_stmt(self, ty: T) -> Stmt<'src, T> {
        Stmt {
            span: self.span,
            kind: stmt::StmtKind::Expr(self.into_expr(ty)),
        }
    }

    pub fn into_expr(self, ty: T) -> Expr<'src, T> {
        Expr {
            span: self.span,
            ty,
            kind: expr::ExprKind::Block(Box::new(self)),
        }
    }
}
