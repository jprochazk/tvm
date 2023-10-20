use std::fmt::{Display, Write};

use super::*;

macro_rules! ty {
  ($hir:ident, $id:expr) => {
    &$hir.types.by_id[($id).to_index()]
  };
}

impl Display for Hir<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "/*\ntypes:")?;
    print_types(self, f)?;
    writeln!(f, "*/")?;

    let top_level_tail_ty = match &self.top_level.tail {
      Some(tail) => tail.ty,
      None => TypeId::None,
    };
    print_function(self, f, "__main__", &[], top_level_tail_ty, &self.top_level)?;
    for decl in &self.decls.array {
      #[allow(irrefutable_let_patterns)]
      if let Decl::Fn(decl) = decl {
        print_function(
          self,
          f,
          decl.name.lexeme,
          &decl.params,
          decl.ret,
          &decl.body,
        )?;
      }
    }
    Ok(())
  }
}

fn print_types(hir: &Hir<'_>, f: &mut impl Write) -> std::fmt::Result {
  let indent = 2;
  for ty in &hir.types.by_id {
    write!(f, "{:indent$}", "")?;
    print_type(hir, f, ty)?;
    writeln!(f)?;
  }
  Ok(())
}

fn print_type(hir: &Hir<'_>, f: &mut impl Write, ty: &Type) -> std::fmt::Result {
  match ty {
    Type::Unknown => write!(f, "unknown")?,
    Type::None => write!(f, "none")?,
    Type::Int => write!(f, "int")?,
    Type::Num => write!(f, "num")?,
    Type::Bool => write!(f, "bool")?,
    Type::Str => write!(f, "str")?,
    Type::Array(item) => {
      write!(f, "[")?;
      print_type(hir, f, ty!(hir, item))?;
      write!(f, "]")?;
    }
    Type::Set(key) => {
      write!(f, "{{")?;
      print_type(hir, f, ty!(hir, key))?;
      write!(f, "}}")?;
    }
    Type::Map(key, val) => {
      write!(f, "{{")?;
      print_type(hir, f, ty!(hir, key))?;
      write!(f, " -> ")?;
      print_type(hir, f, ty!(hir, val))?;
      write!(f, "}}")?;
    }
    Type::Fn(params, ret) => {
      write!(f, "(")?;
      for param in params {
        print_type(hir, f, ty!(hir, param))?;
      }
      write!(f, ")")?;
      write!(f, " -> ")?;
      print_type(hir, f, ty!(hir, ret))?;
    }
    Type::Opt(inner) => {
      print_type(hir, f, ty!(hir, inner))?;
      write!(f, "?")?;
    }
  }

  Ok(())
}

fn print_block(
  hir: &Hir<'_>,
  f: &mut impl Write,
  indent: usize,
  block: &Block<'_>,
) -> std::fmt::Result {
  writeln!(f, "{{")?;
  for stmt in &block.body {
    let indent = indent + 2;
    write!(f, "{:indent$}", "")?;
    print_stmt(hir, f, indent, stmt)?;
    writeln!(f)?;
  }
  if let Some(tail) = &block.tail {
    let indent = indent + 2;
    write!(f, "{:indent$}", "")?;
    print_expr_top_level(hir, f, indent, tail)?;
    writeln!(f)?;
  }
  write!(f, "{:indent$}", "")?;
  write!(f, "}}")?;

  Ok(())
}

fn print_stmt(
  hir: &Hir<'_>,
  f: &mut impl Write,
  indent: usize,
  stmt: &ast::Stmt<'_>,
) -> std::fmt::Result {
  match &stmt.kind {
    ast::StmtKind::Let(node) => {
      write!(f, "/*inferred: ")?;
      print_type(hir, f, ty!(hir, node.init.ty))?;
      writeln!(f, "*/")?;

      write!(f, "{:indent$}", "")?;
      write!(f, "let {}", node.name.lexeme)?;
      if let Some(ty) = &node.ty {
        write!(f, ": {}", &hir.src[ty.span])?;
      }
      write!(f, " = ")?;
      print_expr_top_level(hir, f, indent, &node.init)?;
      write!(f, ";")?;
    }
    ast::StmtKind::Loop(node) => {
      write!(f, "loop ")?;
      print_block(hir, f, indent, &node.body)?;
    }
    ast::StmtKind::Expr(node) => {
      print_expr_top_level(hir, f, indent, &node.inner)?;
      write!(f, ";")?;
    }
  }
  Ok(())
}

fn print_expr(
  hir: &Hir<'_>,
  f: &mut impl Write,
  indent: usize,
  expr: &ast::Expr<'_>,
) -> std::fmt::Result {
  _print_expr(hir, f, indent, expr, true)
}

fn print_expr_top_level(
  hir: &Hir<'_>,
  f: &mut impl Write,
  indent: usize,
  expr: &ast::Expr<'_>,
) -> std::fmt::Result {
  _print_expr(hir, f, indent, expr, false)
}

fn _print_expr(
  hir: &Hir<'_>,
  f: &mut impl Write,
  indent: usize,
  expr: &ast::Expr<'_>,
  print_ty: bool,
) -> std::fmt::Result {
  if print_ty {
    write!(f, "(")?;
  }
  match &expr.kind {
    ast::ExprKind::Return(node) => {
      write!(f, "return")?;
      if let Some(value) = &node.value {
        write!(f, " ")?;
        print_expr(hir, f, indent, value)?;
      }
    }
    ast::ExprKind::Yield(node) => {
      write!(f, "return")?;
      if let Some(value) = &node.value {
        write!(f, " ")?;
        print_expr(hir, f, indent, value)?;
      }
    }
    ast::ExprKind::Break(_) => write!(f, "break")?,
    ast::ExprKind::Continue(_) => write!(f, "continue")?,
    ast::ExprKind::Block(node) => print_block(hir, f, indent, &node.inner)?,
    ast::ExprKind::If(node) => {
      let mut branches = node.branches.iter().peekable();
      while let Some(branch) = branches.next() {
        write!(f, "if ")?;
        print_expr(hir, f, indent, &branch.cond)?;
        write!(f, " ")?;
        print_block(hir, f, indent, &branch.body)?;
        if branches.peek().is_some() {
          writeln!(f, " else ")?;
        }
      }
      if let Some(tail) = &node.tail {
        write!(f, " else ")?;
        print_block(hir, f, indent, tail)?;
      }
    }
    ast::ExprKind::Binary(node) => {
      print_expr(hir, f, indent, &node.left)?;
      write!(f, " {} ", node.op)?;
      print_expr(hir, f, indent, &node.right)?;
    }
    ast::ExprKind::Unary(node) => {
      write!(f, "{}", node.op)?;
      print_expr(hir, f, indent, &node.right)?;
    }
    ast::ExprKind::Literal(node) => match &node.value {
      ast::Literal::None => write!(f, "none")?,
      ast::Literal::Int(v) => write!(f, "{v}")?,
      ast::Literal::Float(v) => write!(f, "{v}")?,
      ast::Literal::Bool(v) => write!(f, "{v}")?,
      ast::Literal::String(v) => write!(f, "{v:?}")?,
      ast::Literal::Array(v) => match v {
        ast::Array::List(v) => {
          if v.len() > 4 {
            writeln!(f, "[")?;
            for item in v {
              write!(f, "{:indent$}", "")?;
              print_expr(hir, f, indent + 2, item)?;
              writeln!(f, ",")?;
            }
            write!(f, "{:indent$}", "")?;
            write!(f, "]")?;
          } else {
            write!(f, "[")?;
            let mut items = v.iter().peekable();
            while let Some(item) = items.next() {
              print_expr(hir, f, indent, item)?;
              if items.peek().is_some() {
                write!(f, ", ")?;
              }
            }
            write!(f, "]")?;
          }
        }
        ast::Array::Copy(item, len) => {
          write!(f, "[")?;
          print_expr(hir, f, indent, item)?;
          write!(f, ";")?;
          print_expr(hir, f, indent, len)?;
          write!(f, "]")?;
        }
      },
      ast::Literal::Set(v) => {
        if v.len() > 4 {
          writeln!(f, "{{")?;
          for item in v {
            write!(f, "{:indent$}", "")?;
            print_expr(hir, f, indent + 2, item)?;
            writeln!(f, ",")?;
          }
          write!(f, "}}")?;
        } else {
          write!(f, "{{")?;
          let mut items = v.iter().peekable();
          while let Some(item) = items.next() {
            print_expr(hir, f, indent, item)?;
            if items.peek().is_some() {
              write!(f, ", ")?;
            }
          }
          write!(f, "}}")?;
        }
      }
      ast::Literal::Map(v) => {
        if v.len() > 4 {
          writeln!(f, "{{")?;
          for (key, val) in v {
            write!(f, "{:indent$}", "")?;
            print_expr(hir, f, indent + 2, key)?;
            write!(f, ": ")?;
            print_expr(hir, f, indent + 2, val)?;
            writeln!(f, ",")?;
          }
          write!(f, "}}")?;
        } else {
          write!(f, "{{")?;
          let mut items = v.iter().peekable();
          while let Some((key, val)) = items.next() {
            print_expr(hir, f, indent, key)?;
            write!(f, ": ")?;
            print_expr(hir, f, indent, val)?;
            if items.peek().is_some() {
              write!(f, ", ")?;
            }
          }
          write!(f, "}}")?;
        }
      }
    },
    ast::ExprKind::Use(node) => match &node.place {
      ast::Place::Var { name } => write!(f, "{}", name.lexeme)?,
      ast::Place::Field { parent, name } => {
        print_expr(hir, f, indent, parent)?;
        write!(f, ".{}", name.lexeme)?;
      }
      ast::Place::Index { parent, key } => {
        print_expr(hir, f, indent, parent)?;
        write!(f, "[")?;
        print_expr(hir, f, indent, key)?;
        write!(f, "]")?;
      }
    },
    ast::ExprKind::Assign(node) => {
      match &node.place {
        ast::Place::Var { name } => write!(f, "{}", name.lexeme)?,
        ast::Place::Field { parent, name } => {
          print_expr(hir, f, indent, parent)?;
          write!(f, ".{}", name.lexeme)?;
        }
        ast::Place::Index { parent, key } => {
          print_expr(hir, f, indent, parent)?;
          write!(f, "[")?;
          print_expr(hir, f, indent, key)?;
          write!(f, "]")?;
        }
      }
      write!(f, " ")?;
      if let Some(op) = node.op {
        write!(f, "{op}")?;
      }
      write!(f, "= ")?;
      print_expr(hir, f, indent, &node.value)?;
    }
    ast::ExprKind::Call(node) => {
      print_expr(hir, f, indent, &node.callee)?;
      write!(f, "(")?;
      let mut args = node.args.iter().peekable();
      while let Some(arg) = args.next() {
        if let Some(key) = &arg.key {
          write!(f, "{}: ", key.lexeme)?;
        }
        print_expr(hir, f, indent, &arg.value)?;
        if args.peek().is_some() {
          write!(f, ", ")?;
        }
      }
      write!(f, ")")?;
    }
  }
  if print_ty {
    write!(f, "/*:")?;
    print_type(hir, f, ty!(hir, expr.ty))?;
    write!(f, "*/)")?;
  }
  Ok(())
}

fn print_function(
  hir: &Hir<'_>,
  f: &mut impl Write,
  name: &str,
  params: &[FnParam<'_>],
  ret: TypeId,
  body: &Block<'_>,
) -> std::fmt::Result {
  write!(f, "fn {name}(")?;
  if !params.is_empty() {
    writeln!(f)?;
  }
  for param in params {
    write!(f, "  {}: ", param.name.lexeme)?;
    print_type(hir, f, ty!(hir, param.ty))?;
    writeln!(f, ",")?;
  }
  write!(f, ") -> ")?;
  print_type(hir, f, ty!(hir, ret))?;
  write!(f, " ")?;
  print_block(hir, f, 0, body)?;
  Ok(())
}