use std::fmt::{Display, Write};

use super::{Hir, Type};
use crate::ast::*;

#[derive(Clone, Copy)]
struct Indent {
    level: usize,
}

impl Indent {
    fn next(mut self) -> Self {
        self.level += 2;
        self
    }
}

impl Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent = self.level;
        write!(f, "{:indent$}", "")
    }
}

impl Display for Hir<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let i = Indent { level: 0 };

        /* for decl in &self.decls {
          match &decl.kind {
            decl::DeclKind::Fn(decl) => {
              print_function_header(f, i, decl)?;
              print_block(f, i, &decl.body)?;
            }
          }
        } */

        {
            let b = &self.top_level;
            for stmt in &b.body {
                write!(f, "{i}")?;
                print_stmt(f, i, stmt)?;
                writeln!(f, ";")?;
            }
            if let Some(tail) = &b.tail {
                print_expr(f, i, tail)?;
            }
        }

        Ok(())
    }
}

fn print_function_header(
    f: &mut impl Write,
    i: Indent,
    decl: &decl::Fn<'_, Type>,
) -> std::fmt::Result {
    writeln!(f, "fn {}(", decl.name)?;
    for param in &decl.params {
        let i = i.next();
        writeln!(f, "{i}{}: {},", param.name, param.ty)?;
    }
    write!(f, "{i}) ")?;
    if let Some(ret) = &decl.ret {
        write!(f, "-> {ret} ")?;
    }
    print_block(f, i, &decl.body)?;
    Ok(())
}

fn print_block(f: &mut impl Write, i: Indent, b: &Block<'_, Type>) -> std::fmt::Result {
    writeln!(f, "{{")?;
    {
        let i = i.next();
        for stmt in &b.body {
            write!(f, "{i}")?;
            print_stmt(f, i, stmt)?;
            writeln!(f, ";")?;
        }
        if let Some(tail) = &b.tail {
            print_expr(f, i, tail)?;
        }
    }
    writeln!(f, "}}")?;

    Ok(())
}

fn print_stmt(f: &mut impl Write, i: Indent, s: &Stmt<'_, Type>) -> std::fmt::Result {
    match &s.kind {
        stmt::StmtKind::Let(s) => {
            write!(f, "let {}", s.name)?;
            if let Some(ty) = &s.ty {
                write!(f, ": {}", ty)?;
            }
            write!(f, " = ")?;
            print_expr(f, i, &s.init)?;
        }
        stmt::StmtKind::Loop(s) => {
            write!(f, "loop ")?;
            print_block(f, i, &s.body)?;
        }
        stmt::StmtKind::Expr(s) => {
            print_expr(f, i, s)?;
        }
    }

    Ok(())
}

fn print_expr(f: &mut impl Write, i: Indent, e: &Expr<'_, Type>) -> std::fmt::Result {
    match &e.kind {
        expr::ExprKind::Return(e) => {
            write!(f, "return ")?;
            if let Some(e) = &e.value {
                print_expr(f, i, e)?;
            }
        }
        /* expr::ExprKind::Yield(e) => {
          write!(f, "yield ")?;
          if let Some(e) = &e.value {
            print_expr(f, i, e)?;
          }
        } */
        expr::ExprKind::Break => {
            write!(f, "break")?;
        }
        expr::ExprKind::Continue => {
            write!(f, "continue")?;
        }
        expr::ExprKind::Block(e) => {
            print_block(f, i, e)?;
        }
        expr::ExprKind::If(e) => {
            let mut first = true;
            for branch in &e.branches {
                if !first {
                    write!(f, " else ")?;
                }
                first = false;
                write!(f, "if ")?;
                print_expr(f, i, &branch.cond)?;
                write!(f, " ")?;
                print_block(f, i, &branch.body)?;
            }
            if let Some(tail) = &e.tail {
                write!(f, " else ")?;
                print_block(f, i, tail)?;
            }
        }
        expr::ExprKind::Binary(e) => {
            print_expr(f, i, &e.lhs)?;
            write!(f, " {} ", e.op)?;
            print_expr(f, i, &e.rhs)?;
        }
        expr::ExprKind::Unary(e) => {
            write!(f, "{} ", e.op)?;
            print_expr(f, i, &e.rhs)?;
        }
        expr::ExprKind::Primitive(e) => match &**e {
            expr::Primitive::Int(v) => write!(f, "{v}")?,
            expr::Primitive::Num(v) => write!(f, "{v}")?,
            expr::Primitive::Bool(v) => write!(f, "{v}")?,
            expr::Primitive::Str(v) => write!(f, "{v:?}")?,
        },
        expr::ExprKind::Array(e) => match &**e {
            expr::Array::Csv(a) => {
                write!(f, "[")?;
                for e in a {
                    let i = i.next();
                    print_expr(f, i, e)?;
                    write!(f, ",")?;
                }
                write!(f, "]")?;
            }
            expr::Array::Len(e, l) => {
                write!(f, "[")?;
                print_expr(f, i, e)?;
                write!(f, ";")?;
                print_expr(f, i, l)?;
                write!(f, "]")?;
            }
        },
        expr::ExprKind::UseVar(e) => write!(f, "{}", e.name)?,
        expr::ExprKind::UseField(e) => {
            print_expr(f, i, &e.parent)?;
            write!(f, ".{}", e.name)?;
        }
        expr::ExprKind::UseIndex(e) => {
            print_expr(f, i, &e.parent)?;
            write!(f, "[")?;
            print_expr(f, i, &e.key)?;
            write!(f, "]")?;
        }
        expr::ExprKind::AssignVar(e) => {
            write!(f, "{}", e.name)?;
            match e.op {
                Some(op) => write!(f, " {op}= ")?,
                None => write!(f, " = ")?,
            }
            print_expr(f, i, &e.value)?;
        }
        expr::ExprKind::AssignField(e) => {
            print_expr(f, i, &e.parent)?;
            write!(f, ".{}", e.name)?;
            match e.op {
                Some(op) => write!(f, " {op}= ")?,
                None => write!(f, " = ")?,
            }
            print_expr(f, i, &e.value)?;
        }
        expr::ExprKind::AssignIndex(e) => {
            print_expr(f, i, &e.parent)?;
            write!(f, "[")?;
            print_expr(f, i, &e.key)?;
            write!(f, "]")?;
            match e.op {
                Some(op) => write!(f, " {op}= ")?,
                None => write!(f, " = ")?,
            }
            print_expr(f, i, &e.value)?;
        }
        expr::ExprKind::Call(e) => {
            print_expr(f, i, &e.callee)?;
            write!(f, "(")?;
            for arg in &e.args {
                if let Some(key) = &arg.key {
                    write!(f, "{key}: ")?;
                }
                print_expr(f, i, &arg.value)?;
                write!(f, ",")?;
            }
            write!(f, ")")?;
        }
        expr::ExprKind::MethodCall(e) => {
            print_expr(f, i, &e.receiver)?;
            write!(f, ".{}", e.method)?;
            write!(f, "(")?;
            for arg in &e.args {
                if let Some(key) = &arg.key {
                    write!(f, "{key}: ")?;
                }
                print_expr(f, i, &arg.value)?;
                write!(f, ",")?;
            }
            write!(f, ")")?;
        }
    }
    write!(f, "/*{}*/", e.ty)?;

    Ok(())
}
