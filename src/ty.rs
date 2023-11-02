use std::borrow::Cow as MaybeRef;
use std::fmt::Display;

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::error::{Error, ErrorCtx, FoldError, Result};
use crate::lex::Span;
use crate::util::Discard;
use crate::{ast, HashMap};

pub fn type_check(ast: &ast::Ast<'_>) -> Result<TypeDb, Vec<Error>> {
  let mut tcx = TypeCtx::new(ast.src);

  for decl in &ast.decls {
    tcx.push_decl(decl).fold_error(tcx.ecx());
  }

  tcx
    .infer(|icx| {
      icx.infer_block(&ast.top_level);
    })
    .fold_error(tcx.ecx());

  for decl in ast.decls.iter().flat_map(|v| v.as_fn()) {
    tcx
      .infer(|icx| {
        icx.infer_block(&decl.body);
      })
      .fold_error(tcx.ecx());
  }

  let errors = tcx.ecx.finish();
  if !errors.is_empty() {
    return Err(errors);
  }

  Ok(TypeDb {
    expr_to_ty: tcx.expr_to_ty,
  })
}

pub struct TypeDb {
  expr_to_ty: HashMap<ast::ExprId, Ty>,
}

type Scope<'src> = HashMap<&'src str, Ty>;

struct TypeCtx<'src> {
  scopes: Vec<Scope<'src>>,
  expr_to_ty: HashMap<ast::ExprId, Ty>,
  ecx: ErrorCtx<'src>,
}

impl<'src> TypeCtx<'src> {
  fn new(src: &'src str) -> Self {
    Self {
      scopes: Vec::new(),
      expr_to_ty: HashMap::default(),
      ecx: ErrorCtx::new(src),
    }
  }

  fn by_name(&self, name: &str) -> Result<Ty> {
    match name {
      "int" => Ok(Ty::Prim(Prim::Int)),
      "num" => Ok(Ty::Prim(Prim::Num)),
      "bool" => Ok(Ty::Prim(Prim::Bool)),
      "str" => Ok(Ty::Prim(Prim::Str)),
      _ => todo!("resolve type by name from decls"),
    }
  }

  fn ecx(&mut self) -> &mut ErrorCtx<'src> {
    &mut self.ecx
  }

  fn enter_scope(&mut self) {
    self.scopes.push(HashMap::default());
  }

  fn leave_scope(&mut self) {
    self.scopes.pop().unwrap();
  }

  fn infer<'a>(&'a mut self, f: impl FnOnce(&mut InferCtx<'a, 'src>)) -> Result<()> {
    let mut icx = InferCtx {
      tcx: self,
      expr_to_ty: HashMap::default(),
      table: InPlaceUnificationTable::default(),
      constraints: Vec::new(),
    };
    f(&mut icx);
    icx.unify()?;
    icx.substitute_exprs();

    icx.tcx.expr_to_ty.extend(icx.expr_to_ty);

    Ok(())
  }

  fn push_decl(&mut self, decl: &ast::Decl<'src>) -> Result<()> {
    todo!()
  }
}

struct InferCtx<'a, 'src> {
  tcx: &'a mut TypeCtx<'src>,
  expr_to_ty: HashMap<ast::ExprId, Ty>,
  table: InPlaceUnificationTable<Var>,
  constraints: Vec<(Span, Constraint)>,
}

impl<'a, 'src> InferCtx<'a, 'src> {
  #[inline]
  fn ecx(&mut self) -> &mut ErrorCtx<'src> {
    &mut self.tcx.ecx
  }

  #[inline]
  fn scope(&mut self) -> &mut Scope<'src> {
    self.tcx.scopes.last_mut().unwrap()
  }

  fn unify(&mut self) -> Result<()> {
    for (span, constraint) in std::mem::take(&mut self.constraints) {
      match constraint {
        Constraint::Eq(lhs, rhs) => self.unify_eq(span, lhs, rhs)?,
      }
    }

    Ok(())
  }

  fn unify_eq(&mut self, span: Span, mut lhs: Ty, mut rhs: Ty) -> Result<()> {
    self.normalize(&mut lhs)?;
    self.normalize(&mut rhs)?;
    match (lhs, rhs) {
      (Ty::Never, _) | (_, Ty::Never) => Ok(()),
      (Ty::Prim(lhs), Ty::Prim(rhs)) if lhs == rhs => Ok(()),
      (Ty::Fn(lhs), Ty::Fn(rhs)) if lhs.params.len() == rhs.params.len() => {
        let lparams = lhs.params.into_iter();
        let rparams = rhs.params.into_iter();
        for (lhs, rhs) in lparams.zip(rparams) {
          self.unify_eq(span, lhs, rhs)?;
        }
        self.unify_eq(span, *lhs.ret, *rhs.ret)
      }
      (Ty::App(lhs), Ty::App(rhs)) if lhs.name == rhs.name => {
        let largs = lhs.args.into_iter();
        let rargs = rhs.args.into_iter();
        for (lhs, rhs) in largs.zip(rargs) {
          self.unify_eq(span, lhs, rhs)?;
        }
        Ok(())
      }
      (Ty::Var(lhs), Ty::Var(rhs)) => self
        .table
        .unify_var_var(lhs, rhs)
        .map_err(|(lhs, rhs)| self.ecx().type_mismatch(span, lhs, rhs)),
      (Ty::Var(var), ty) | (ty, Ty::Var(var)) => {
        if var.occurs_in(&ty) {
          return Err(self.ecx().infinite_type(span, ty));
        }

        self
          .table
          .unify_var_value(var, Some(ty))
          .map_err(|(lhs, rhs)| self.ecx().type_mismatch(span, lhs, rhs))
      }
      (lhs, rhs) => Err(self.ecx().type_mismatch(span, lhs, rhs)),
    }
  }

  fn normalize(&mut self, ty: &mut Ty) -> Result<()> {
    match ty {
      Ty::Never => Ok(()),
      Ty::Prim(_) => Ok(()),
      Ty::Fn(ty) => {
        for param in &mut ty.params {
          self.normalize(param)?;
        }
        self.normalize(&mut ty.ret)?;
        Ok(())
      }
      Ty::Array(Array { inner }) => self.normalize(inner),
      Ty::App(ty) => {
        for arg in &mut ty.args {
          self.normalize(arg)?;
        }
        Ok(())
      }
      Ty::Var(var) => match self.table.probe_value(*var) {
        Some(mut inner) => {
          self.normalize(&mut inner)?;
          *ty = inner;
          Ok(())
        }
        None => Ok(()),
      },
    }
  }

  fn substitute_exprs(&mut self) {
    let mut expr_to_ty = std::mem::take(&mut self.expr_to_ty);
    for ty in expr_to_ty.values_mut() {
      self.substitute(ty);
    }
    self.expr_to_ty = expr_to_ty;
  }

  fn substitute(&mut self, ty: &mut Ty) {
    match ty {
      Ty::Never => {}
      Ty::Prim(_) => {}
      Ty::Fn(Fn { params, ret }) => {
        for param in params {
          self.substitute(param);
        }
        self.substitute(ret);
      }
      Ty::App(App { args, .. }) => {
        for arg in args {
          self.substitute(arg);
        }
      }
      Ty::Array(Array { inner }) => {
        self.substitute(inner);
      }
      Ty::Var(var) => {
        let root = self.table.find(*var);
        match self.table.probe_value(root) {
          Some(mut inner) => {
            self.substitute(&mut inner);
            *ty = inner;
          }
          None => *ty = Ty::Never,
        }
      }
    }
  }

  fn infer_block(&mut self, block: &ast::Block<'src>) -> Ty {
    self.tcx.enter_scope();
    for stmt in &block.body {
      self.infer_stmt(stmt).fold_error(self.ecx());
    }

    let ty = if let Some(tail) = &block.tail {
      match self.infer_expr(tail) {
        Ok(ty) => ty,
        Err(e) => {
          self.ecx().push(e);
          Ty::Var(self.type_var())
        }
      }
    } else {
      Ty::Var(self.type_var())
    };

    self.tcx.leave_scope();

    ty
  }

  fn infer_stmt(&mut self, stmt: &ast::Stmt<'src>) -> Result<()> {
    match &stmt.kind {
      ast::StmtKind::Let(node) => self.infer_let(node),
      ast::StmtKind::Loop(node) => {
        self.infer_block(&node.body);
        Ok(())
      }
      ast::StmtKind::Expr(node) => self.infer_expr(&node.inner).discard(),
    }
  }

  fn infer_let(&mut self, node: &ast::LetStmt<'src>) -> Result<()> {
    let ann_ty = match &node.ty {
      Some(ty) => Some(self.ast_ty_to_ty(ty)?),
      None => None,
    };
    let init_ty = match ann_ty {
      Some(ann_ty) => {
        let init_ty = self.infer_expr(&node.init)?;
        self.check(
          node.init.span,
          MaybeRef::Owned(init_ty),
          MaybeRef::Borrowed(&ann_ty),
        )?;
        ann_ty
      }
      None => self.infer_expr(&node.init)?,
    };
    self.scope().insert(node.name.as_str(), init_ty);
    Ok(())
  }

  fn infer_expr(&mut self, expr: &ast::Expr<'src>) -> Result<Ty> {
    let ty = match &expr.kind {
      ast::ExprKind::Return(_) => Ty::Never,
      // ast::ExprKind::Yield(_) => todo!(),
      ast::ExprKind::Break(_) => Ty::Never,
      ast::ExprKind::Continue(_) => Ty::Never,
      ast::ExprKind::Block(block) => self.infer_block(&block.inner),
      ast::ExprKind::If(node) => self.infer_if(node)?,
      ast::ExprKind::Binary(node) => self.infer_binary(node)?,
      ast::ExprKind::Unary(node) => self.infer_unary(node)?,
      ast::ExprKind::Literal(node) => self.infer_literal(node)?,
      ast::ExprKind::UseVar(node) => self.infer_use_var(node)?,
      ast::ExprKind::UseField(node) => self.infer_use_field(node)?,
      ast::ExprKind::UseIndex(node) => self.infer_use_index(node)?,
      ast::ExprKind::AssignVar(node) => self.infer_assign_var(node)?,
      ast::ExprKind::AssignField(node) => self.infer_assign_field(node)?,
      ast::ExprKind::AssignIndex(node) => self.infer_assign_index(node)?,
      ast::ExprKind::Call(node) => self.infer_call(node)?,
      ast::ExprKind::MethodCall(node) => self.infer_method_call(node)?,
    };
    self.expr_to_ty.insert(expr.id, ty.clone());
    Ok(ty)
  }

  fn infer_if(&mut self, node: &ast::IfExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_binary(&mut self, node: &ast::BinaryExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_unary(&mut self, node: &ast::UnaryExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_literal(&mut self, node: &ast::LiteralExpr<'src>) -> Result<Ty> {
    match &node.value {
      ast::Literal::Int(_) => Ok(Ty::Prim(Prim::Int)),
      ast::Literal::Float(_) => Ok(Ty::Prim(Prim::Num)),
      ast::Literal::Bool(_) => Ok(Ty::Prim(Prim::Bool)),
      ast::Literal::String(_) => Ok(Ty::Prim(Prim::Str)),
      ast::Literal::Array(node) => self.infer_array_literal(node),
    }
  }

  fn infer_array_literal(&mut self, node: &ast::Array<'src>) -> Result<Ty> {
    match node {
      ast::Array::List(items) => {
        let inner = if items.is_empty() {
          Ty::Var(self.type_var())
        } else {
          let ty = self.infer_expr(&items[0])?;
          for item in &items[1..] {
            let item_ty = self.infer_expr(item)?;
            self.check(item.span, MaybeRef::Owned(item_ty), MaybeRef::Borrowed(&ty))?;
          }
          ty
        };
        Ok(Ty::Array(Array {
          inner: Box::new(inner),
        }))
      }
      ast::Array::Copy(item, len) => {
        let inner = self.infer_expr(item)?;
        let len_ty = self.infer_expr(len)?;
        self.check(
          len.span,
          MaybeRef::Borrowed(&len_ty),
          MaybeRef::Borrowed(&Ty::Prim(Prim::Int)),
        )?;
        Ok(Ty::Array(Array {
          inner: Box::new(inner),
        }))
      }
    }
  }

  fn infer_use_var(&mut self, node: &ast::UseVarExpr<'src>) -> Result<Ty> {
    match self.scope().get(node.name.as_str()) {
      Some(ty) => Ok(ty.clone()),
      None => Err(self.ecx().undefined_var(node.name.span)),
    }
  }

  fn infer_use_field(&mut self, node: &ast::UseFieldExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_use_index(&mut self, node: &ast::UseIndexExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_assign_var(&mut self, node: &ast::AssignVarExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_assign_field(&mut self, node: &ast::AssignFieldExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_assign_index(&mut self, node: &ast::AssignIndexExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_call(&mut self, node: &ast::CallExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn infer_method_call(&mut self, node: &ast::MethodCallExpr<'src>) -> Result<Ty> {
    todo!()
  }

  fn check(&mut self, span: Span, lhs: MaybeRef<'_, Ty>, rhs: MaybeRef<'_, Ty>) -> Result<()> {
    match (lhs.as_ref(), rhs.as_ref()) {
      (Ty::Prim(Prim::Int), Ty::Prim(Prim::Int)) => Ok(()),
      (Ty::Prim(Prim::Num), Ty::Prim(Prim::Num)) => Ok(()),
      (Ty::Prim(Prim::Bool), Ty::Prim(Prim::Bool)) => Ok(()),
      (Ty::Prim(Prim::Str), Ty::Prim(Prim::Str)) => Ok(()),
      (Ty::Array(_), Ty::Array(_)) => {
        let lhs = map_maybe_ref(lhs, Ty::as_array_inner, Ty::into_array_inner);
        let rhs = map_maybe_ref(rhs, Ty::as_array_inner, Ty::into_array_inner);
        self.check(span, lhs, rhs)?;
        Ok(())
      }
      _ => {
        let lhs = lhs.into_owned();
        let rhs = rhs.into_owned();
        self.eq(span, lhs, rhs);
        Ok(())
      }
    }
  }

  fn eq(&mut self, span: impl Into<Span>, lhs: Ty, rhs: Ty) {
    self
      .constraints
      .push((span.into(), Constraint::Eq(lhs, rhs)))
  }

  fn type_var(&mut self) -> Var {
    self.table.new_key(None)
  }

  fn ast_ty_to_ty(&mut self, ty: &ast::Type<'src>) -> Result<Ty> {
    match &ty.kind {
      ast::TypeKind::Empty(_) => Ok(Ty::Var(self.type_var())),
      ast::TypeKind::Named(ty) => self.tcx.by_name(ty.name.as_str()),
      ast::TypeKind::Array(_) => todo!(),
      ast::TypeKind::Fn(_) => todo!(),
      ast::TypeKind::Opt(_) => todo!(),
    }
  }
}

enum Constraint {
  Eq(Ty, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
  Never,
  Prim(Prim),
  Fn(Fn),
  App(App),
  Array(Array),
  Var(Var),
}

impl Ty {
  pub fn as_array_inner(&self) -> &Ty {
    match self {
      Ty::Array(v) => &v.inner,
      _ => unreachable!(),
    }
  }

  pub fn into_array_inner(self) -> Ty {
    match self {
      Ty::Array(v) => *v.inner,
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prim {
  Int,
  Num,
  Bool,
  Str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fn {
  pub params: Vec<Ty>,
  pub ret: Box<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct App {
  pub name: String,
  pub args: Vec<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
  pub inner: Box<Ty>,
}

impl EqUnifyValue for Ty {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(u32);

impl Var {
  fn occurs_in(&self, ty: &Ty) -> bool {
    match ty {
      Ty::Never => false,
      Ty::Prim(_) => false,
      Ty::Fn(Fn { params, ret }) => {
        params.iter().any(|ty| self.occurs_in(ty)) || self.occurs_in(ret)
      }
      Ty::Array(Array { inner }) => self.occurs_in(inner),
      Ty::App(App { args, .. }) => args.iter().any(|ty| self.occurs_in(ty)),
      Ty::Var(var) => self == var,
    }
  }
}

impl UnifyKey for Var {
  type Value = Option<Ty>;

  fn index(&self) -> u32 {
    self.0
  }

  fn from_index(u: u32) -> Self {
    Self(u)
  }

  fn tag() -> &'static str {
    "Var"
  }
}

impl Display for Var {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "'{}", self.0)
  }
}

impl Display for Ty {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Ty::Never => f.write_str("!"),
      Ty::Prim(Prim::Int) => f.write_str("int"),
      Ty::Prim(Prim::Num) => f.write_str("num"),
      Ty::Prim(Prim::Bool) => f.write_str("bool"),
      Ty::Prim(Prim::Str) => f.write_str("str"),
      Ty::Fn(Fn { params, ret }) => {
        f.write_str("(")?;
        let mut params = params.iter().peekable();
        while let Some(param) = params.next() {
          Display::fmt(param, f)?;
          if params.peek().is_some() {
            f.write_str(", ")?;
          }
        }
        f.write_str(") -> ")?;
        Display::fmt(ret, f)
      }
      Ty::Array(Array { inner }) => write!(f, "[{inner}]"),
      Ty::App(App { name, args }) => {
        f.write_str(name)?;
        if !args.is_empty() {
          f.write_str("<")?;
          let mut args = args.iter().peekable();
          while let Some(arg) = args.next() {
            Display::fmt(arg, f)?;
            if args.peek().is_some() {
              f.write_str(", ")?;
            }
          }
          f.write_str(">")?;
        }
        Ok(())
      }
      Ty::Var(var) => Display::fmt(var, f),
    }
  }
}

#[inline]
fn map_maybe_ref<T: Clone, U: Clone>(
  v: MaybeRef<'_, T>,
  borrowed: impl FnOnce(&T) -> &U,
  owned: impl FnOnce(T) -> U,
) -> MaybeRef<'_, U> {
  match v {
    MaybeRef::Borrowed(v) => MaybeRef::Borrowed(borrowed(v)),
    MaybeRef::Owned(v) => MaybeRef::Owned(owned(v)),
  }
}

pub mod print;

#[cfg(test)]
mod tests;
