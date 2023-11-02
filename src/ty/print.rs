use std::fmt::{Display, Write};

use super::TypeDb;
use crate::ast;
use crate::ast::Ast;

pub struct TypedAst<'a> {
  ast: &'a Ast<'a>,
  db: &'a TypeDb,
}

impl<'a> TypedAst<'a> {
  pub fn new(ast: &'a Ast, db: &'a TypeDb) -> Self {
    Self { ast, db }
  }
}

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

impl<'a> Display for TypedAst<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let db = self.db;
    let i = Indent { level: 0 };

    for decl in &self.ast.decls {
      match &decl.kind {
        crate::ast::DeclKind::Fn(decl) => {
          print_function_header(f, i, db, decl)?;
          print_block(f, i, db, &decl.body)?;
        }
      }
    }

    {
      let b = &self.ast.top_level;
      for stmt in &b.body {
        write!(f, "{i}")?;
        print_stmt(f, i, db, stmt)?;
        writeln!(f, ";")?;
      }
      if let Some(tail) = &b.tail {
        print_expr(f, i, db, tail)?;
      }
    }

    Ok(())
  }
}

fn print_function_header(
  f: &mut impl Write,
  i: Indent,
  db: &TypeDb,
  decl: &ast::FnDecl<'_>,
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
  print_block(f, i, db, &decl.body)?;
  Ok(())
}

fn print_block(f: &mut impl Write, i: Indent, db: &TypeDb, b: &ast::Block<'_>) -> std::fmt::Result {
  writeln!(f, "{{")?;
  {
    let i = i.next();
    for stmt in &b.body {
      write!(f, "{i}")?;
      print_stmt(f, i, db, stmt)?;
      writeln!(f, ";")?;
    }
    if let Some(tail) = &b.tail {
      print_expr(f, i, db, tail)?;
    }
  }
  writeln!(f, "}}")?;

  Ok(())
}

fn print_stmt(f: &mut impl Write, i: Indent, db: &TypeDb, s: &ast::Stmt<'_>) -> std::fmt::Result {
  match &s.kind {
    ast::StmtKind::Let(s) => {
      write!(f, "let {}", s.name)?;
      if let Some(ty) = &s.ty {
        write!(f, ": {}", ty)?;
      }
      write!(f, " = ")?;
      print_expr(f, i, db, &s.init)?;
    }
    ast::StmtKind::Loop(s) => {
      write!(f, "loop ")?;
      print_block(f, i, db, &s.body)?;
    }
    ast::StmtKind::Expr(s) => {
      print_expr(f, i, db, &s.inner)?;
    }
  }

  Ok(())
}

fn print_expr(f: &mut impl Write, i: Indent, db: &TypeDb, e: &ast::Expr<'_>) -> std::fmt::Result {
  if let Some(ty) = db.expr_to_ty.get(&e.id) {
    write!(f, "/*{ty}*/")?;
  } else {
    write!(f, "/*{{unknown}}*/")?;
  }
  match &e.kind {
    ast::ExprKind::Return(e) => {
      write!(f, "return ")?;
      if let Some(e) = &e.value {
        print_expr(f, i, db, e)?;
      }
    }
    /* ast::ExprKind::Yield(e) => {
      write!(f, "yield ")?;
      if let Some(e) = &e.value {
        print_expr(f, i, db, e)?;
      }
    } */
    ast::ExprKind::Break(_) => {
      write!(f, "break")?;
    }
    ast::ExprKind::Continue(_) => {
      write!(f, "continue")?;
    }
    ast::ExprKind::Block(e) => {
      print_block(f, i, db, &e.inner)?;
    }
    ast::ExprKind::If(e) => {
      let mut first = true;
      for branch in &e.branches {
        if !first {
          write!(f, " else ")?;
        }
        first = false;
        write!(f, "if ")?;
        print_expr(f, i, db, &branch.cond)?;
        write!(f, " ")?;
        print_block(f, i, db, &branch.body)?;
      }
      if let Some(tail) = &e.tail {
        write!(f, " else ")?;
        print_block(f, i, db, tail)?;
      }
    }
    ast::ExprKind::Binary(e) => {
      print_expr(f, i, db, &e.left)?;
      write!(f, " {} ", e.op)?;
      print_expr(f, i, db, &e.right)?;
    }
    ast::ExprKind::Unary(e) => {
      write!(f, "{} ", e.op)?;
      print_expr(f, i, db, &e.right)?;
    }
    ast::ExprKind::Literal(e) => match &e.value {
      ast::Literal::Int(v) => write!(f, "{v}")?,
      ast::Literal::Float(v) => write!(f, "{v}")?,
      ast::Literal::Bool(v) => write!(f, "{v}")?,
      ast::Literal::String(v) => write!(f, "{v:?}")?,
      ast::Literal::Array(v) => match v {
        ast::Array::List(v) => {
          writeln!(f, "[")?;
          for e in v {
            let i = i.next();
            write!(f, "{i}")?;
            print_expr(f, i, db, e)?;
            writeln!(f, ",")?;
          }
          write!(f, "]")?;
        }
        ast::Array::Copy(v, l) => {
          write!(f, "[")?;
          print_expr(f, i, db, v)?;
          write!(f, ";")?;
          print_expr(f, i, db, l)?;
          write!(f, "]")?;
        }
      },
    },
    ast::ExprKind::UseVar(e) => write!(f, "{}", e.name)?,
    ast::ExprKind::UseField(e) => {
      print_expr(f, i, db, &e.parent)?;
      write!(f, ".{}", e.name)?;
    }
    ast::ExprKind::UseIndex(e) => {
      print_expr(f, i, db, &e.parent)?;
      write!(f, "[")?;
      print_expr(f, i, db, &e.key)?;
      write!(f, "]")?;
    }
    ast::ExprKind::AssignVar(e) => {
      write!(f, "{}", e.name)?;
      match e.op {
        Some(op) => write!(f, " {op}= ")?,
        None => write!(f, " = ")?,
      }
      print_expr(f, i, db, &e.value)?;
    }
    ast::ExprKind::AssignField(e) => {
      print_expr(f, i, db, &e.parent)?;
      write!(f, ".{}", e.name)?;
      match e.op {
        Some(op) => write!(f, " {op}= ")?,
        None => write!(f, " = ")?,
      }
      print_expr(f, i, db, &e.value)?;
    }
    ast::ExprKind::AssignIndex(e) => {
      print_expr(f, i, db, &e.parent)?;
      write!(f, "[")?;
      print_expr(f, i, db, &e.key)?;
      write!(f, "]")?;
      match e.op {
        Some(op) => write!(f, " {op}= ")?,
        None => write!(f, " = ")?,
      }
      print_expr(f, i, db, &e.value)?;
    }
    ast::ExprKind::Call(e) => {
      print_expr(f, i, db, &e.callee)?;
      write!(f, "(")?;
      for arg in &e.args {
        if let Some(key) = &arg.key {
          write!(f, "{key}: ")?;
        }
        print_expr(f, i, db, &arg.value)?;
        write!(f, ",")?;
      }
      write!(f, ")")?;
    }
    ast::ExprKind::MethodCall(e) => {
      print_expr(f, i, db, &e.receiver)?;
      write!(f, ".{}", e.method)?;
      write!(f, "(")?;
      for arg in &e.args {
        if let Some(key) = &arg.key {
          write!(f, "{key}: ")?;
        }
        print_expr(f, i, db, &arg.value)?;
        write!(f, ",")?;
      }
      write!(f, ")")?;
    }
  }

  Ok(())
}
