use std::fmt::Display;

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::ast::*;
use crate::error::{Error, ErrorCtx, Result};
use crate::lex::Span;
use crate::{Cow, HashMap};

// TODO: figure out a sane way to re-use the allocated AST nodes
//       instead of allocating new ones while type checking
// TODO: type info:
//       - use functions instead of a table, that way static data can be a
//         static function
// TODO: field exprs:
//       - `v.x` will probe the type of `v`, if it is not known yet, then emit
//         an error. this works well in practice for Rust.

// NOTE: whenever matching on types, make sure that the types are normalized.

pub struct Hir<'src> {
  pub src: &'src str,
  pub top_level: Block<'src, Type>,
}

pub fn type_check<'src>(ast: &Ast<'src>) -> Result<Hir<'src>, Vec<Error>> {
  let mut tcx = TypeCtx::new(ast.src);

  /* for decl in &ast.decls {
    tcx.push_decl(decl).fold_error(tcx.ecx());
  } */

  let top_level = tcx.infer(
    |icx| icx.infer_block(&ast.top_level),
    |icx, mut block| {
      icx.substitute_block(&mut block);
      block
    },
  );

  /* for decl in ast.decls.iter().flat_map(|v| v.as_fn()) {
    tcx.infer(|icx| icx.infer_block(&decl.body));
  } */

  tcx.ecx.finish_result()?;

  Ok(Hir {
    src: ast.src,
    top_level,
  })
}

type Scope<'src> = HashMap<&'src str, Type>;

struct TypeCtx<'src> {
  scopes: Vec<Scope<'src>>,
  ecx: ErrorCtx<'src>,
}

impl<'src> TypeCtx<'src> {
  fn new(src: &'src str) -> Self {
    Self {
      scopes: Vec::new(),
      ecx: ErrorCtx::new(src),
    }
  }

  fn by_name(&self, name: &str) -> Result<Type> {
    match name {
      "int" => Ok(Type::Int),
      "num" => Ok(Type::Num),
      "bool" => Ok(Type::Bool),
      "str" => Ok(Type::Str),
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

  fn infer<'a, T>(
    &'a mut self,
    i: impl FnOnce(&mut InferCtx<'a, 'src>) -> T,
    sub: impl FnOnce(&mut InferCtx<'a, 'src>, T) -> T,
  ) -> T {
    let icx = &mut InferCtx {
      tcx: self,
      table: InPlaceUnificationTable::default(),
    };
    let ret = i(icx);
    sub(icx, ret)
  }

  fn push_decl(&mut self, decl: &Decl<'src>) -> Result<()> {
    todo!()
  }
}

struct InferCtx<'a, 'src> {
  tcx: &'a mut TypeCtx<'src>,
  table: InPlaceUnificationTable<Var>,
  // constraints: Vec<(Span, Type, Type)>,
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

  fn report(&mut self, e: Error) -> Type {
    self.ecx().push(e);
    Type::Error
  }

  /// Unify `lhs` and `rhs`
  fn unify(&mut self, span: Span, mut lhs: Type, mut rhs: Type) -> Result<()> {
    self.normalize(&mut lhs);
    self.normalize(&mut rhs);
    match (lhs, rhs) {
      (Type::Error, _) | (_, Type::Error) => {
        // emitted as a result of some other error
        Ok(())
      }
      (Type::Int, Type::Int) => Ok(()),
      (Type::Num, Type::Num) => Ok(()),
      (Type::Num, Type::Int) | (Type::Int, Type::Num) => Ok(()),
      (Type::Bool, Type::Bool) => Ok(()),
      (Type::Str, Type::Str) => Ok(()),
      (Type::Func(lhs), Type::Func(rhs)) if lhs.params.len() == rhs.params.len() => {
        let lparams = lhs.params.into_iter();
        let rparams = rhs.params.into_iter();
        for (lhs, rhs) in lparams.zip(rparams) {
          self.unify(span, lhs, rhs)?;
        }
        self.unify(span, *lhs.ret, *rhs.ret)
      }
      (Type::App(lhs), Type::App(rhs)) if lhs.name == rhs.name => {
        let largs = lhs.args.into_iter();
        let rargs = rhs.args.into_iter();
        for (lhs, rhs) in largs.zip(rargs) {
          self.unify(span, lhs, rhs)?;
        }
        Ok(())
      }
      (Type::Var(lhs), Type::Var(rhs)) => self
        .table
        .unify_var_var(lhs, rhs)
        .map_err(|(lhs, rhs)| self.ecx().type_mismatch(span, lhs, rhs)),
      (Type::Var(var), ty) | (ty, Type::Var(var)) => {
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

  fn normalize(&mut self, ty: &mut Type) {
    match ty {
      Type::Error | Type::Void | Type::Int | Type::Num | Type::Bool | Type::Str => {}
      Type::Func(ty) => {
        for param in &mut ty.params {
          self.normalize(param)
        }
        self.normalize(&mut ty.ret)
      }
      Type::App(ty) => {
        for arg in &mut ty.args {
          self.normalize(arg)
        }
      }
      Type::Array(Array { item: inner }) => self.normalize(inner),
      Type::Opt(Opt { inner }) => self.normalize(inner),
      Type::Var(var) => {
        if let Some(mut inner) = self.table.probe_value(*var) {
          self.normalize(&mut inner);
          *ty = inner;
        }
      }
    }
  }

  fn substitute_block(&mut self, block: &mut Block<'src, Type>) {
    for stmt in &mut block.body {
      self.substitute_stmt(stmt);
    }

    if let Some(tail) = &mut block.tail {
      self.substitute_expr(tail)
    }
  }

  fn substitute_stmt(&mut self, stmt: &mut Stmt<'src, Type>) {
    use stmt::StmtKind as S;
    match &mut stmt.kind {
      S::Let(node) => self.substitute_expr(&mut node.init),
      S::Loop(node) => self.substitute_block(&mut node.body),
      S::Expr(node) => self.substitute_expr(node),
    }
  }

  fn substitute_expr(&mut self, expr: &mut Expr<'src, Type>) {
    use expr::ExprKind as E;
    self.normalize(&mut expr.ty);
    match &mut expr.kind {
      E::Return(node) => {
        if let Some(value) = &mut node.value {
          self.substitute_expr(value)
        }
      }
      E::Break => {}
      E::Continue => {}
      E::Block(node) => self.substitute_block(node),
      E::If(node) => {
        for branch in &mut node.branches {
          self.substitute_expr(&mut branch.cond);
          self.substitute_block(&mut branch.body)
        }
        if let Some(tail) = &mut node.tail {
          self.substitute_block(tail)
        }
      }
      E::Binary(node) => {
        self.substitute_expr(&mut node.lhs);
        self.substitute_expr(&mut node.rhs)
      }
      E::Unary(node) => self.substitute_expr(&mut node.rhs),
      E::Primitive(_) => {}
      E::Array(node) => match &mut **node {
        expr::Array::Csv(items) => {
          for item in items {
            self.substitute_expr(item)
          }
        }
        expr::Array::Len(item, len) => {
          self.substitute_expr(item);
          self.substitute_expr(len)
        }
      },
      E::UseVar(_) => {}
      E::UseField(node) => self.substitute_expr(&mut node.parent),
      E::UseIndex(node) => {
        self.substitute_expr(&mut node.parent);
        self.substitute_expr(&mut node.key)
      }
      E::AssignVar(node) => self.substitute_expr(&mut node.value),
      E::AssignField(node) => {
        self.substitute_expr(&mut node.parent);
        self.substitute_expr(&mut node.value)
      }
      E::AssignIndex(node) => {
        self.substitute_expr(&mut node.parent);
        self.substitute_expr(&mut node.key);
        self.substitute_expr(&mut node.value)
      }
      E::Call(node) => {
        self.substitute_expr(&mut node.callee);
        for arg in &mut node.args {
          self.substitute_expr(&mut arg.value)
        }
      }
      E::MethodCall(node) => {
        self.substitute_expr(&mut node.receiver);
        for arg in &mut node.args {
          self.substitute_expr(&mut arg.value)
        }
      }
    }
  }

  fn infer_block(&mut self, block: &Block<'src>) -> Block<'src, Type> {
    self.tcx.enter_scope();
    let span = block.span;
    let body = block
      .body
      .iter()
      .map(|stmt| self.infer_stmt(stmt))
      .collect();
    let tail = match &block.tail {
      Some(tail) => Some(self.infer_expr(tail)),
      None => None,
    };
    self.tcx.leave_scope();

    Block { span, body, tail }
  }

  fn infer_stmt(&mut self, stmt: &Stmt<'src>) -> Stmt<'src, Type> {
    use stmt::StmtKind as S;
    match &stmt.kind {
      S::Let(node) => self.infer_let_stmt(stmt.span, node),
      S::Loop(node) => self.infer_loop_stmt(stmt.span, node),
      S::Expr(node) => self.infer_expr_stmt(node),
    }
  }

  fn infer_let_stmt(&mut self, span: Span, node: &stmt::Let<'src>) -> Stmt<'src, Type> {
    let ann_ty = node.ty.as_ref().map(|ty| self.ast_ty_to_ty(ty));
    let init = match &ann_ty {
      Some(ann_ty) => self.check(&node.init, ann_ty),
      None => self.infer_expr(&node.init),
    };
    self.scope().insert(node.name.as_str(), init.ty.clone());
    stmt::Let::new(span, node.name.clone(), node.ty.clone(), init)
  }

  fn infer_loop_stmt(&mut self, span: Span, node: &stmt::Loop<'src>) -> Stmt<'src, Type> {
    stmt::Loop::new(span, self.infer_block(&node.body))
  }

  fn infer_expr_stmt(&mut self, node: &Expr<'src>) -> Stmt<'src, Type> {
    self.infer_expr(node).into_stmt()
  }

  fn infer_expr(&mut self, expr: &Expr<'src>) -> Expr<'src, Type> {
    use expr::ExprKind as E;
    match &expr.kind {
      E::Return(node) => self.infer_return(expr.span, node),
      E::Break => self.infer_break(expr.span),
      E::Continue => self.infer_continue(expr.span),
      E::Block(node) => self.infer_block_expr(expr.span, node),
      E::If(node) => self.infer_if(expr.span, node),
      E::Binary(node) => self.infer_binary(expr.span, node),
      E::Unary(node) => self.infer_unary(expr.span, node),
      E::Primitive(node) => self.infer_primitive(expr.span, node),
      E::Array(node) => self.infer_array(expr.span, node),
      E::UseVar(node) => self.infer_use_var(expr.span, node),
      E::UseField(node) => self.infer_use_field(expr.span, node),
      E::UseIndex(node) => self.infer_use_index(expr.span, node),
      E::AssignVar(node) => self.infer_assign_var(expr.span, node),
      E::AssignField(node) => self.infer_assign_field(expr.span, node),
      E::AssignIndex(node) => self.infer_assign_index(expr.span, node),
      E::Call(node) => self.infer_call(expr.span, node),
      E::MethodCall(node) => self.infer_method_call(expr.span, node),
    }
  }

  fn infer_return(&mut self, span: Span, node: &expr::Return<'src>) -> Expr<'src, Type> {
    let (value, ty) = match &node.value {
      Some(value) => {
        let value = self.infer_expr(value);
        let ty = value.ty.clone();
        (Some(value), ty)
      }
      None => (None, Type::Var(self.type_var())),
    };
    expr::Return::with(span, ty, value)
  }

  fn infer_break(&mut self, span: Span) -> Expr<'src, Type> {
    expr::Break::with(span, Type::Var(self.type_var()))
  }

  fn infer_continue(&mut self, span: Span) -> Expr<'src, Type> {
    expr::Continue::with(span, Type::Var(self.type_var()))
  }

  fn block_ty(&mut self, block: Option<&Block<'src, Type>>) -> Type {
    match block {
      Some(Block {
        tail: Some(tail), ..
      }) => tail.ty.clone(),
      _ => Type::Var(self.type_var()),
    }
  }

  fn infer_block_expr(&mut self, span: Span, node: &Block<'src>) -> Expr<'src, Type> {
    let block = self.infer_block(node);
    let ty = self.block_ty(Some(&block));
    let mut e = block.into_expr(ty);
    e.span = span; // block expr has a `do` keyword
    e
  }

  fn infer_if(&mut self, span: Span, node: &expr::If<'src>) -> Expr<'src, Type> {
    let tail = match &node.tail {
      Some(tail) => Some(self.infer_block(tail)),
      None => None,
    };
    let ty = self.block_ty(tail.as_ref());

    let branches = node
      .branches
      .iter()
      .map(|branch| {
        let cond = self.check(&branch.cond, &Type::Bool);
        let body = self.check_block(&branch.body, &ty);

        Branch { cond, body }
      })
      .collect::<Vec<_>>();

    expr::If::with(span, ty, branches, tail)
  }

  fn infer_binary(&mut self, span: Span, node: &expr::Binary<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_unary(&mut self, span: Span, node: &expr::Unary<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_primitive(&mut self, span: Span, node: &expr::Primitive<'src>) -> Expr<'src, Type> {
    use expr::Primitive as P;
    match node {
      P::Int(value) => expr::Primitive::int_with(span, Type::Int, *value),
      P::Num(value) => expr::Primitive::num_with(span, Type::Num, *value),
      P::Bool(value) => expr::Primitive::bool_with(span, Type::Bool, *value),
      P::Str(value) => expr::Primitive::str_with(span, Type::Str, value.clone()),
    }
  }

  fn infer_array(&mut self, span: Span, node: &expr::Array<'src>) -> Expr<'src, Type> {
    use expr::Array as A;
    match node {
      A::Csv(items) => {
        let items: Vec<_> = items.iter().map(|item| self.infer_expr(item)).collect();
        let ty = match items.len() {
          0 => Type::Var(self.type_var()),
          _ => items[0].ty.clone(),
        };
        expr::Array::csv_with(span, Array::new(ty), items)
      }
      A::Len(item, len) => {
        let item = self.infer_expr(item);
        let len = self.check(len, &Type::Int);
        expr::Array::len_with(span, Array::new(item.ty.clone()), item, len)
      }
    }
  }

  fn infer_use_var(&mut self, span: Span, node: &expr::UseVar<'src>) -> Expr<'src, Type> {
    let ty = match self.scope().get(node.name.as_str()) {
      Some(ty) => ty.clone(),
      None => {
        let e = self.ecx().undefined_var(node.name.span);
        self.report(e)
      }
    };
    expr::UseVar::with(span, ty, node.name.clone())
  }

  fn infer_use_field(&mut self, span: Span, node: &expr::UseField<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_use_index(&mut self, span: Span, node: &expr::UseIndex<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_assign_var(&mut self, span: Span, node: &expr::AssignVar<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_assign_field(&mut self, span: Span, node: &expr::AssignField<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_assign_index(&mut self, span: Span, node: &expr::AssignIndex<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_call(&mut self, span: Span, node: &expr::Call<'src>) -> Expr<'src, Type> {
    todo!()
  }

  fn infer_method_call(&mut self, span: Span, node: &expr::MethodCall<'src>) -> Expr<'src, Type> {
    fn inner<'src>(
      icx: &mut InferCtx<'_, 'src>,
      span: Span,
      receiver: &Expr<'src, Type>,
      node: &expr::MethodCall<'src>,
    ) -> Result<CheckedCall<'src>> {
      let method = icx.method_ty(receiver, node.method.as_str())?;
      icx.check_call(span, &node.args, method)
    }

    let receiver = self.infer_expr(&node.receiver);
    match inner(self, span, &receiver, node) {
      Ok(call) => expr::MethodCall::with(span, call.ret, receiver, node.method.clone(), call.args),
      Err(e) => {
        let args = node
          .args
          .iter()
          .map(|arg| Arg {
            key: arg.key.clone(),
            value: self.infer_expr(&arg.value),
          })
          .collect();
        expr::MethodCall::with(span, self.report(e), receiver, node.method.clone(), args)
      }
    }
  }

  fn method_ty(&mut self, receiver: &Expr<'src, Type>, name: &str) -> Result<Func> {
    let mut receiver_ty = receiver.ty.clone();
    self.normalize(&mut receiver_ty);
    let ty = match dbg!(receiver_ty) {
      Type::Error | Type::Void | Type::Int | Type::Num | Type::Bool | Type::Str | Type::Func(_) => {
        None
      }
      Type::App(_) => todo!(),
      // TODO: synchronize implementation types with this match
      Type::Array(array) => match name {
        "push" => Some(Func {
          params: vec![*array.item],
          ret: Box::new(Type::Var(self.type_var())),
        }),
        _ => None,
      },
      Type::Opt(_) => todo!(),
      Type::Var(_) => return Err(self.ecx().undefined_var(receiver.span)),
    };

    ty.ok_or_else(|| {
      self
        .ecx()
        .unknown_method(receiver.span, receiver.ty.clone(), name)
    })
  }

  fn check_call(
    &mut self,
    span: Span,
    args: &[Arg<'src>],
    func: Func,
  ) -> Result<CheckedCall<'src>> {
    if func.params.len() != args.len() {
      let e = self
        .ecx()
        .param_mismatch(span, func.params.len(), args.len());
      return Err(e);
    }

    let args: Vec<_> = args
      .iter()
      .zip(func.params.iter())
      .map(|(arg, param)| Arg {
        key: arg.key.clone(),
        value: self.check(&arg.value, param),
      })
      .collect();

    Ok(CheckedCall {
      args,
      ret: *func.ret,
    })
  }

  fn check(&mut self, expr: &Expr<'src>, expected: &Type) -> Expr<'src, Type> {
    let mut expr = self.infer_expr(expr);
    match (&expr.ty, expected) {
      (Type::Error, _) | (_, Type::Error) => {
        expr.ty = Type::Error;
        expr
      }
      (Type::Int, Type::Int) => expr,
      (Type::Num, Type::Num) => expr,
      (Type::Bool, Type::Bool) => expr,
      (Type::Str, Type::Str) => expr,
      _ => match self.unify(expr.span, expr.ty.clone(), expected.clone()) {
        Ok(()) => {
          self.normalize(&mut expr.ty);
          expr
        }
        Err(e) => {
          expr.ty = self.report(e);
          expr
        }
      },
    }
  }

  fn check_block(&mut self, block: &Block<'src>, expected: &Type) -> Block<'src, Type> {
    let span = block.span;
    let body = block
      .body
      .iter()
      .map(|stmt| self.infer_stmt(stmt))
      .collect();
    let tail = match &block.tail {
      Some(tail) => Some(self.check(tail, expected)),
      None => None,
    };
    self.tcx.leave_scope();

    Block { span, body, tail }
  }

  fn type_var(&mut self) -> Var {
    self.table.new_key(None)
  }

  fn ast_ty_to_ty(&mut self, ty: &ty::Type<'src>) -> Type {
    fn inner(icx: &mut InferCtx<'_, '_>, ty: &ty::Type<'_>) -> Result<Type> {
      use ty::TypeKind as T;
      match &ty.kind {
        T::Empty => Ok(Type::Var(icx.type_var())),
        T::Named(ty) => icx.tcx.by_name(ty.name.as_str()),
        T::Array(ty) => Ok(Type::Array(Array {
          item: Box::new(inner(icx, &ty.item)?),
        })),
        T::Fn(ty) => {
          let mut params = Vec::with_capacity(ty.params.len());
          for param in &ty.params {
            params.push(inner(icx, param)?);
          }
          let ret = inner(icx, &ty.ret)?;
          Ok(Type::Func(Func {
            params,
            ret: Box::new(ret),
          }))
        }
        T::Opt(ty) => Ok(Type::Opt(Opt {
          inner: Box::new(inner(icx, &ty.inner)?),
        })),
      }
    }

    match inner(self, ty) {
      Ok(ty) => ty,
      Err(e) => self.report(e),
    }
  }
}

struct CheckedCall<'src> {
  args: Vec<Arg<'src, Type>>,
  ret: Type,
}

enum Expect<'a> {
  Eq(Cow<'a, Type>),
  To(Cow<'a, Type>),
}

enum ExpectRef<'a> {
  /// Must equal `ty`
  Eq(&'a Type),
  /// Must coerce to `ty`
  To(&'a Type),
}

impl<'a> Expect<'a> {
  fn as_ref(&self) -> ExpectRef<'_> {
    match self {
      Expect::Eq(ty) => ExpectRef::Eq(ty.as_ref()),
      Expect::To(ty) => ExpectRef::To(ty.as_ref()),
    }
  }

  fn inner(&self) -> &Type {
    match self {
      Expect::Eq(ty) => ty,
      Expect::To(ty) => ty,
    }
  }

  fn into_inner(self) -> Cow<'a, Type> {
    match self {
      Expect::Eq(ty) => ty,
      Expect::To(ty) => ty,
    }
  }

  fn into_constraint(self, lhs: Type) -> Constraint {
    match self {
      Expect::Eq(rhs) => Constraint::Eq(lhs, rhs.into_owned()),
      Expect::To(rhs) => Constraint::Coerce(lhs, rhs.into_owned()),
    }
  }
}

impl<'a> From<&'a Type> for Cow<'a, Type> {
  fn from(value: &'a Type) -> Self {
    Cow::Borrowed(value)
  }
}

enum Constraint {
  Eq(Type, Type),
  Coerce(Type, Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  /// Type error
  Error,
  /// No value
  Void,
  /// Int
  Int,
  /// Generic number type
  Num,
  /// Boolean
  Bool,
  /// String
  Str,
  /// Function
  Func(Func),
  /// Named type with args
  App(App),
  /// Homogenous array
  Array(Array),
  /// Optional
  Opt(Opt),
  /// Type variable
  Var(Var),
}

impl Type {
  pub fn as_array_inner(&self) -> &Type {
    match self {
      Type::Array(v) => &v.item,
      _ => unreachable!(),
    }
  }

  pub fn into_array_inner(self) -> Type {
    match self {
      Type::Array(v) => *v.item,
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Func {
  pub params: Vec<Type>,
  pub ret: Box<Type>,
}

impl Func {
  pub fn new(params: Vec<Type>, ret: Type) -> Type {
    Type::Func(Func {
      params,
      ret: Box::new(ret),
    })
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct App {
  pub name: String,
  pub args: Vec<Type>,
}

impl App {
  pub fn new(name: String, args: Vec<Type>) -> Type {
    Type::App(App { name, args })
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
  pub item: Box<Type>,
}

impl Array {
  pub fn new(item: Type) -> Type {
    Type::Array(Array {
      item: Box::new(item),
    })
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Opt {
  pub inner: Box<Type>,
}

impl Opt {
  pub fn new(inner: Type) -> Type {
    Type::Opt(Opt {
      inner: Box::new(inner),
    })
  }
}

impl EqUnifyValue for Type {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(u32);

impl Var {
  fn occurs_in(&self, ty: &Type) -> bool {
    // note: this is called during unification, so types are normalized
    match ty {
      Type::Error | Type::Void | Type::Int | Type::Num | Type::Bool | Type::Str => false,
      Type::Func(Func { params, ret }) => {
        params.iter().any(|ty| self.occurs_in(ty)) || self.occurs_in(ret)
      }
      Type::App(App { args, .. }) => args.iter().any(|ty| self.occurs_in(ty)),
      Type::Array(Array { item: inner }) => self.occurs_in(inner),
      Type::Opt(Opt { inner }) => self.occurs_in(inner),
      Type::Var(var) => self == var,
    }
  }
}

impl UnifyKey for Var {
  type Value = Option<Type>;

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

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Error => write!(f, "{{error}}"),
      Type::Void => f.write_str("_"),
      Type::Int => f.write_str("int"),
      Type::Num => f.write_str("num"),
      Type::Bool => f.write_str("bool"),
      Type::Str => f.write_str("str"),
      Type::Func(Func { params, ret }) => {
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
      Type::App(App { name, args }) => {
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
      Type::Array(Array { item: inner }) => write!(f, "[{inner}]"),
      Type::Opt(Opt { inner }) => write!(f, "{inner}?"),
      Type::Var(var) => Display::fmt(var, f),
    }
  }
}

pub mod print;

#[cfg(test)]
mod tests;
