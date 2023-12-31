use std::fmt::{Debug, Display, Formatter, Result};

impl Debug for super::Ast<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Ast")
            .field("decls", &self.decls)
            .field("top_level", &self.top_level)
            .finish()
    }
}

impl Debug for super::decl::Decl<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&self.kind, f)
    }
}

impl Debug for super::decl::DeclKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Fn(arg0) => Debug::fmt(arg0, f),
            Self::Type(arg0) => Debug::fmt(arg0, f),
        }
    }
}

impl Debug for super::decl::Fn<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Fn")
            .field("name", &self.name)
            .field("params", &self.params)
            .field("ret", &self.ret)
            .field("body", &self.body)
            .finish()
    }
}

impl Debug for super::decl::Body<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Extern => write!(f, "Extern"),
            Self::Block(arg0) => f.debug_tuple("Block").field(arg0).finish(),
        }
    }
}

impl Debug for super::decl::TypeDef<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Type")
            .field("name", &self.name)
            .field("fields", &self.fields)
            .finish()
    }
}

impl Debug for super::decl::Fields<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Extern => write!(f, "Extern"),
            Self::Named(arg0) => f.debug_tuple("Named").field(arg0).finish(),
        }
    }
}

impl Debug for super::decl::Field<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Field")
            .field("name", &self.name)
            .field("ty", &self.ty)
            .finish()
    }
}

impl Debug for super::stmt::Stmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&self.kind, f)
    }
}

impl Debug for super::stmt::StmtKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Let(arg0) => Debug::fmt(arg0, f),
            Self::Loop(arg0) => Debug::fmt(arg0, f),
            Self::Expr(arg0) => Debug::fmt(arg0, f),
        }
    }
}

impl Debug for super::stmt::Let<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Let")
            .field("name", &self.name)
            .field("ty", &self.ty)
            .field("init", &self.init)
            .finish()
    }
}

impl Debug for super::stmt::Loop<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Loop").field("body", &self.body).finish()
    }
}

impl Display for super::ty::Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use super::ty::TyKind as Ty;
        match &self.kind {
            Ty::Empty => f.write_str("_"),
            Ty::Named(ty) => f.write_str(ty.name.lexeme),
        }
    }
}

impl Debug for super::ty::Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&self.kind, f)
    }
}

impl Debug for super::ty::TyKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Named(arg0) => f.debug_tuple("Named").field(arg0).finish(),
        }
    }
}

impl Debug for super::ty::Named<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Named").field("name", &self.name).finish()
    }
}

impl Debug for super::expr::Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&self.kind, f)
    }
}

impl Debug for super::expr::ExprKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Return(arg0) => Debug::fmt(arg0, f),
            Self::Break => write!(f, "Break"),
            Self::Continue => write!(f, "Continue"),
            Self::Block(arg0) => Debug::fmt(arg0, f),
            Self::If(arg0) => Debug::fmt(arg0, f),
            Self::Binary(arg0) => Debug::fmt(arg0, f),
            Self::Unary(arg0) => Debug::fmt(arg0, f),
            Self::Primitive(arg0) => Debug::fmt(arg0, f),
            Self::Array(arg0) => Debug::fmt(arg0, f),
            Self::UseVar(arg0) => Debug::fmt(arg0, f),
            Self::UseField(arg0) => Debug::fmt(arg0, f),
            Self::UseIndex(arg0) => Debug::fmt(arg0, f),
            Self::AssignVar(arg0) => Debug::fmt(arg0, f),
            Self::AssignField(arg0) => Debug::fmt(arg0, f),
            Self::AssignIndex(arg0) => Debug::fmt(arg0, f),
            Self::Call(arg0) => Debug::fmt(arg0, f),
            Self::MethodCall(arg0) => Debug::fmt(arg0, f),
        }
    }
}

impl Debug for super::expr::Return<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Return")
            .field("value", &self.value)
            .finish()
    }
}

impl Debug for super::expr::If<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("If")
            .field("branches", &self.branches)
            .field("tail", &self.tail)
            .finish()
    }
}

impl Debug for super::expr::Binary<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Binary")
            .field("left", &self.lhs)
            .field("op", &self.op)
            .field("right", &self.rhs)
            .finish()
    }
}

impl Debug for super::expr::Unary<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Unary")
            .field("op", &self.op)
            .field("right", &self.rhs)
            .finish()
    }
}

impl Debug for super::expr::Primitive<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Num(arg0) => f.debug_tuple("Num").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Str(arg0) => f.debug_tuple("Str").field(arg0).finish(),
        }
    }
}

impl Debug for super::expr::Array<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Array").field("items", &self.items).finish()
    }
}

impl Debug for super::expr::UseVar<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("UseVar").field("name", &self.name).finish()
    }
}

impl Debug for super::expr::UseField<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("UseField")
            .field("parent", &self.parent)
            .field("name", &self.name)
            .finish()
    }
}

impl Debug for super::expr::UseIndex<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("UseIndex")
            .field("parent", &self.parent)
            .field("key", &self.key)
            .finish()
    }
}

impl Debug for super::expr::AssignVar<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("AssignVar")
            .field("name", &self.name)
            .field("op", &self.op)
            .field("value", &self.value)
            .finish()
    }
}

impl Debug for super::expr::AssignField<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("AssignField")
            .field("parent", &self.parent)
            .field("name", &self.name)
            .field("op", &self.op)
            .field("value", &self.value)
            .finish()
    }
}

impl Debug for super::expr::AssignIndex<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("AssignIndex")
            .field("parent", &self.parent)
            .field("key", &self.key)
            .field("op", &self.op)
            .field("value", &self.value)
            .finish()
    }
}

impl Debug for super::expr::Call<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Call")
            .field("callee", &self.callee)
            .field("args", &self.args)
            .finish()
    }
}

impl Debug for super::expr::MethodCall<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("MethodCall")
            .field("receiver", &self.receiver)
            .field("method", &self.method)
            .field("args", &self.args)
            .finish()
    }
}

impl Display for super::BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use super::BinaryOp as Op;
        f.write_str(match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Rem => "%",
            Op::Pow => "**",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::Gt => ">",
            Op::Lt => "<",
            Op::Ge => ">=",
            Op::Le => "<=",
            Op::And => "&&",
            Op::Or => "||",
            Op::Opt => "??",
        })
    }
}

impl Debug for super::BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Op({self})")
    }
}

impl Display for super::UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use super::UnaryOp as Op;
        f.write_str(match self {
            Op::Minus => "-",
            Op::Not => "!",
            Op::Opt => "?",
        })
    }
}

impl Debug for super::UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Op({self})")
    }
}

impl Debug for super::Param<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Param")
            .field(&self.name)
            .field(&self.ty)
            .finish()
    }
}

impl Debug for super::Branch<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Branch")
            .field(&self.cond)
            .field(&self.body)
            .finish()
    }
}

impl Debug for super::Arg<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = f.debug_tuple("Arg");
        match &self.key {
            Some(key) => t.field(key).field(&self.value).finish(),
            None => t.field(&self.value).finish(),
        }
    }
}

impl Debug for super::Block<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("body", &self.body)
            .field("tail", &self.tail)
            .finish()
    }
}

impl Display for super::Ident<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.lexeme)
    }
}

impl Debug for super::Ident<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Ident({:?})", self.lexeme)
    }
}
