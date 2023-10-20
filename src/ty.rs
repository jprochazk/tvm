use crate::ast::{self, Ast, Block, Ident};
use crate::error::Result;
use crate::lex::Span;
use crate::{HashMap, PreHash};

pub fn check(ast: Ast<'_>) -> Result<Hir<'_>> {
  Checker::new().check(ast)
}

pub struct Hir<'src> {
  pub src: &'src str,
  pub decls: DeclPool<'src>,
  pub types: TypePool<'src>,
  pub top_level: Block<'src>,
}

pub struct Checker<'src> {
  env: TypeEnv<'src>,
  vars: Vec<HashMap<&'src str, TypeId>>,
}

impl<'src> Checker<'src> {
  pub fn new() -> Self {
    Self {
      env: TypeEnv::new(),
      vars: vec![HashMap::default()],
    }
  }

  pub fn check(mut self, ast: Ast<'src>) -> Result<Hir<'src>> {
    let c = &mut self;

    let mut decls = DeclPool::new();

    // 1. check records (recursive)
    // TODO: check records

    // 2. check declarations
    for decl in ast.decls {
      // TODO: collect errors
      decls.insert(check_decl(c, decl)?);
    }

    // 3. check top-level (defines module vars)
    // TODO: define module vars
    let top_level = check_block(c, ast.top_level)?;

    // 4. check fn bodies
    for decl in decls.array.iter_mut() {
      // TODO: define params + check return type
      #[allow(irrefutable_let_patterns)]
      if let Decl::Fn(decl) = decl {
        infer_block(c, &mut decl.body)?;
      }
    }

    let types = self.env.types;

    assert_eq!(self.vars.len(), 1);

    Ok(Hir {
      src: ast.src,
      decls,
      types,
      top_level,
    })
  }

  fn enter_scope(&mut self) {
    self.vars.push(HashMap::default());
  }

  fn define(&mut self, name: Ident<'src>, ty: TypeId) -> Result<()> {
    if self
      .vars
      .last_mut()
      .unwrap()
      .insert(name.lexeme, ty)
      .is_some()
    {
      return Err(err!(@name.span, VarAlreadyDefined));
    }
    Ok(())
  }

  fn leave_scope(&mut self) {
    assert!(self.vars.pop().is_some());
  }

  fn intern_ty(&mut self, ty: Type) -> TypeId {
    self.env.types.intern_ty(ty)
  }

  fn intern_ast(&mut self, ty: &ast::Type<'src>) -> Result<TypeId> {
    self.env.types.intern_ast(ty)
  }
}

fn check_decl<'src>(c: &mut Checker<'src>, decl: ast::Decl<'src>) -> Result<Decl<'src>> {
  match decl.kind {
    ast::DeclKind::Fn(decl) => check_decl_fn(c, *decl),
  }
}

fn check_decl_fn<'src>(c: &mut Checker<'src>, decl: ast::FnDecl<'src>) -> Result<Decl<'src>> {
  let name = decl.name;
  let mut params = Vec::with_capacity(decl.params.len());
  for param in &decl.params {
    // TODO: collect errors
    params.push(FnParam {
      name: param.name,
      ty: c.env.types.intern_ast(&param.ty)?,
    });
  }
  let ret = match &decl.ret {
    Some(ty) => c.env.types.intern_ast(ty)?,
    None => TypeId::None,
  };
  Ok(Decl::Fn(Fn {
    name,
    params,
    ret,
    body: decl.body,
  }))
}

fn check_block<'src>(c: &mut Checker<'src>, mut block: Block<'src>) -> Result<Block<'src>> {
  c.enter_scope();

  for stmt in block.body.iter_mut() {
    check_stmt(c, stmt)?;
  }

  if let Some(tail) = block.tail.as_mut() {
    infer(c, tail)?;
  }

  c.leave_scope();

  Ok(block)
}

fn infer_block<'src>(c: &mut Checker<'src>, block: &mut Block<'src>) -> Result<TypeId> {
  *block = check_block(c, std::mem::take(block))?;
  let ty = match &block.tail {
    Some(tail) => tail.ty,
    None => TypeId::None,
  };
  Ok(ty)
}

fn check_stmt<'src>(c: &mut Checker<'src>, stmt: &mut ast::Stmt<'src>) -> Result<()> {
  match &mut stmt.kind {
    ast::StmtKind::Let(node) => {
      let ty_l = match &node.ty {
        Some(ty) => c.intern_ast(ty)?,
        None => TypeId::Unknown,
      };
      let ty_r = infer(c, &mut node.init)?;
      let ty = unify(node.init.span, c, ty_l, ty_r)?;
      c.define(node.name, ty)
    }
    ast::StmtKind::Loop(node) => infer_block(c, &mut node.body).map(|_| ()),
    ast::StmtKind::Expr(node) => infer(c, &mut node.inner).map(|_| ()),
  }
}

fn infer<'src>(c: &mut Checker<'src>, expr: &mut ast::Expr<'src>) -> Result<TypeId> {
  let ty = match &mut expr.kind {
    ast::ExprKind::Return(node) => match &mut node.value {
      Some(value) => infer(c, value)?,
      None => TypeId::Unknown,
    },
    ast::ExprKind::Yield(node) => match &mut node.value {
      Some(value) => infer(c, value)?,
      None => TypeId::Unknown,
    },
    ast::ExprKind::Break(_) => TypeId::None,
    ast::ExprKind::Continue(_) => TypeId::None,
    ast::ExprKind::Block(node) => {
      infer_block(c, &mut node.inner)?;
      match &node.inner.tail {
        Some(tail) => tail.ty,
        None => TypeId::None,
      }
    }
    ast::ExprKind::If(node) => {
      // TODO: non-expr if
      let mut branches = node.branches.iter_mut();
      let first = branches.next().unwrap();
      infer(c, &mut first.cond).and_then(|ty| unify(first.cond.span, c, ty, TypeId::Bool))?;
      let body_ty = infer_block(c, &mut first.body)?;
      for branch in branches {
        infer(c, &mut branch.cond).and_then(|ty| unify(first.cond.span, c, ty, TypeId::Bool))?;
        infer_block(c, &mut branch.body).and_then(|ty| unify(branch.body.span, c, ty, body_ty))?;
      }
      match node.tail.as_mut() {
        Some(tail) => infer_block(c, tail).and_then(|ty| unify(tail.span, c, ty, body_ty))?,
        None => unify(first.body.span, c, TypeId::None, body_ty)?,
      }
    }
    ast::ExprKind::Binary(node) => {
      let ty_l = infer(c, &mut node.left)?;
      let ty_r = infer(c, &mut node.right)?;
      unify_binary_op(expr.span, c, ty_l, node.op, ty_r)?
    }
    ast::ExprKind::Unary(node) => {
      let ty_r = infer(c, &mut node.right)?;
      unify_unary_op(expr.span, c, node.op, ty_r)?
    }
    ast::ExprKind::Literal(node) => infer_lit(c, &mut node.value)?,
    ast::ExprKind::Use(_) => todo!(),
    ast::ExprKind::Assign(_) => todo!(),
    ast::ExprKind::Call(_) => todo!(),
  };
  expr.ty = ty;
  Ok(ty)
}

fn infer_lit<'src>(c: &mut Checker<'src>, value: &mut ast::Literal<'src>) -> Result<TypeId> {
  Ok(match value {
    ast::Literal::None => TypeId::None,
    ast::Literal::Int(_) => TypeId::Int,
    ast::Literal::Float(_) => TypeId::Num,
    ast::Literal::Bool(_) => TypeId::Bool,
    ast::Literal::String(_) => TypeId::Str,
    ast::Literal::Array(node) => {
      let inner = match node {
        ast::Array::List(items) if items.is_empty() => TypeId::Unknown,
        ast::Array::List(items) => {
          let mut items = items.iter_mut();
          let item_ty = infer(c, items.next().unwrap())?;
          for item in items {
            infer(c, item).and_then(|ty| unify(item.span, c, ty, item_ty))?;
          }
          item_ty
        }
        // TODO: unify len with int
        ast::Array::Copy(item, _) => infer(c, item)?,
      };
      c.intern_ty(Type::Array(inner))
    }
    ast::Literal::Set(values) => {
      let inner = if values.is_empty() {
        TypeId::Unknown
      } else {
        let mut values = values.iter_mut();
        // TODO: check for valid key type
        let key_ty = infer(c, values.next().unwrap())?;
        for value in values {
          infer(c, value).and_then(|ty| unify(value.span, c, ty, key_ty))?;
        }
        key_ty
      };
      c.intern_ty(Type::Set(inner))
    }
    ast::Literal::Map(pairs) => {
      let (key, val) = if pairs.is_empty() {
        (TypeId::Unknown, TypeId::Unknown)
      } else {
        let mut pairs = pairs.iter_mut();
        let (first_key, first_val) = pairs.next().unwrap();
        // TODO: check for valid key type
        let key_ty = infer(c, first_key)?;
        let val_ty = infer(c, first_val)?;
        for (key, val) in pairs {
          infer(c, key).and_then(|ty| unify(key.span, c, ty, key_ty))?;
          infer(c, val).and_then(|ty| unify(val.span, c, ty, val_ty))?;
        }
        (key_ty, val_ty)
      };
      c.intern_ty(Type::Map(key, val))
    }
  })
}

fn unify_binary_op(
  span: Span,
  c: &mut Checker<'_>,
  ty_l: TypeId,
  op: ast::BinaryOp,
  ty_r: TypeId,
) -> Result<TypeId> {
  match op {
    ast::BinaryOp::Add
    | ast::BinaryOp::Sub
    | ast::BinaryOp::Mul
    | ast::BinaryOp::Div
    | ast::BinaryOp::Rem
    | ast::BinaryOp::Pow
    | ast::BinaryOp::Gt
    | ast::BinaryOp::Lt
    | ast::BinaryOp::Ge
    | ast::BinaryOp::Le => {
      let ty = unify(span, c, ty_l, ty_r)?;
      match ty {
        TypeId::Int | TypeId::Num => Ok(ty),
        _ => unify(span, c, ty, TypeId::Num),
      }
    }
    ast::BinaryOp::Eq | ast::BinaryOp::Ne => {
      unify(span, c, ty_l, ty_r)?;
      Ok(TypeId::Bool)
    }
    ast::BinaryOp::And | ast::BinaryOp::Or => {
      let ty = unify(span, c, ty_l, ty_r)?;
      unify(span, c, ty, TypeId::Bool)
    }
    ast::BinaryOp::Opt => match &c.env.types.by_id[ty_l.to_index()] {
      Type::Opt(inner) => unify(span, c, *inner, ty_r),
      _ => Err(err!(@span, TypeError)),
    },
  }
}

fn unify_unary_op(
  span: Span,
  c: &mut Checker<'_>,
  op: ast::UnaryOp,
  ty_r: TypeId,
) -> Result<TypeId> {
  match op {
    ast::UnaryOp::Minus => match ty_r {
      TypeId::Int | TypeId::Num => Ok(ty_r),
      _ => unify(span, c, ty_r, TypeId::Num),
    },
    ast::UnaryOp::Not => unify(span, c, ty_r, TypeId::Bool),
    ast::UnaryOp::Opt => todo!(),
  }
}

fn unify(span: Span, c: &mut Checker<'_>, ty_l: TypeId, ty_r: TypeId) -> Result<TypeId> {
  // TODO: unification must match on _types_, not _type ids_.

  let t = &mut c.env.types.by_id;
  let l = ty_l.to_index();
  let r = ty_r.to_index();
  match (t[l].clone(), t[r].clone()) {
    // `unknown -> T` coercion
    (_, Type::Unknown) => Ok(ty_l),
    (Type::Unknown, _) => Ok(ty_r),

    // `int -> num` coercion
    (Type::Int, Type::Num) | (Type::Num, Type::Int) => Ok(TypeId::Num),

    // `T == T` for primitives
    (Type::None, Type::None)
    | (Type::Int, Type::Int)
    | (Type::Num, Type::Num)
    | (Type::Bool, Type::Bool)
    | (Type::Str, Type::Str) => Ok(ty_l),

    (Type::Opt(l), Type::Opt(r)) => {
      let ty = Type::Opt(unify(span, c, l, r)?);
      Ok(c.intern_ty(ty))
    }
    (Type::Array(l), Type::Array(r)) => {
      let ty = Type::Array(unify(span, c, l, r)?);
      Ok(c.intern_ty(ty))
    }
    (Type::Set(l), Type::Set(r)) => {
      let ty = Type::Set(unify(span, c, l, r)?);
      Ok(c.intern_ty(ty))
    }
    (Type::Map(lk, lv), Type::Map(rk, rv)) => {
      let ty = Type::Map(unify(span, c, lk, rk)?, unify(span, c, lv, rv)?);
      Ok(c.intern_ty(ty))
    }
    (Type::Fn(lp, lret), Type::Fn(rp, rret)) => {
      if lp.len() != rp.len() {
        return Err(err!(@span, WrongParamCount));
      }

      let mut p = Vec::with_capacity(lp.len());
      for (l, r) in lp.into_iter().zip(rp.into_iter()) {
        p.push(unify(span, c, l, r)?);
      }
      let ret = unify(span, c, lret, rret)?;

      let ty = Type::Fn(p, ret);
      Ok(c.intern_ty(ty))
    }

    _ => Err(err!(@span, TypeError)),
  }
}

pub struct DeclPool<'src> {
  array: Vec<Decl<'src>>,
  map: HashMap<&'src str, DeclId>,
  next_decl_id: DeclId,
}

impl<'src> DeclPool<'src> {
  fn new() -> Self {
    Self {
      array: Vec::new(),
      map: HashMap::default(),
      next_decl_id: DeclId(0),
    }
  }

  fn insert(&mut self, decl: Decl<'src>) -> DeclId {
    let id = DeclId(self.next_decl_id.0 + 1);
    self.next_decl_id.0 += 1;
    self.map.insert(decl.name(), id);
    self.array.push(decl);
    id
  }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeclId(u32);

pub enum Decl<'src> {
  Fn(Fn<'src>),
}

impl<'src> Decl<'src> {
  fn name(&self) -> &'src str {
    match self {
      Decl::Fn(decl) => decl.name.lexeme,
    }
  }
}

pub struct Fn<'src> {
  pub name: Ident<'src>,
  pub params: Vec<FnParam<'src>>,
  pub ret: TypeId,
  pub body: Block<'src>,
}

pub struct FnParam<'src> {
  pub name: Ident<'src>,
  pub ty: TypeId,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TypeId {
  #[default]
  Unknown = 0,

  None = 1,
  Int = 2,
  Num = 3,
  Bool = 4,
  Str = 5,

  Other(u16) = 6,
}

#[derive(Clone, Hash)]
pub enum Type {
  Unknown,
  None,
  Int,
  Num,
  Bool,
  Str,
  Array(TypeId),
  Set(TypeId),
  Map(TypeId, TypeId),
  Fn(Vec<TypeId>, TypeId),
  Opt(TypeId),
}

pub struct TypeEnv<'src> {
  types: TypePool<'src>,
}

impl TypeEnv<'_> {
  fn new() -> Self {
    Self {
      types: TypePool::new(),
    }
  }
}

pub struct TypePool<'src> {
  by_id: Vec<Type>,
  by_name: HashMap<&'src str, TypeId>,
  by_ty_hash: PreHash<TypeId>,
  by_ast_hash: PreHash<TypeId>,
  next_type_id: u16,
}

impl<'src> TypePool<'src> {
  fn new() -> Self {
    let primitives = [
      (Type::Unknown, None),
      (Type::None, None),
      (Type::Int, Some((TypeId::Int, "int"))),
      (Type::Num, Some((TypeId::Num, "num"))),
      (Type::Bool, Some((TypeId::Bool, "bool"))),
      (Type::Str, Some((TypeId::Str, "str"))),
    ];

    let mut by_id = Vec::with_capacity(primitives.len() + 1);
    let mut by_name = HashMap::with_capacity_and_hasher(primitives.len(), Default::default());
    let mut by_ty_hash = PreHash::with_capacity_and_hasher(primitives.len(), Default::default());
    let mut by_ast_hash = PreHash::with_capacity_and_hasher(primitives.len(), Default::default());

    for (ty, info) in primitives {
      if let Some((id, name)) = info {
        by_ty_hash.insert(hash_of(&ty), id);
        by_ast_hash.insert(
          hash_of(&ast::Type::make_named(Span::empty(), Ident::raw(name))),
          id,
        );
        by_name.insert(name, id);
      }
      by_id.push(ty);
    }

    Self {
      by_id,
      by_name,
      by_ty_hash,
      by_ast_hash,
      next_type_id: 0,
    }
  }

  fn next_type_id(&mut self) -> TypeId {
    let id = TypeId::Other(self.next_type_id);
    self.next_type_id += 1;
    id
  }

  fn intern_ty(&mut self, ty: Type) -> TypeId {
    if let Some(id) = self.by_ty_hash.get(&hash_of(&ty)) {
      // interned
      return *id;
    }

    // does not exist yet
    let hash = hash_of(&ty);
    let id = self.next_type_id();
    self.by_ty_hash.insert(hash, id);
    self.by_id.push(ty);
    id
  }

  fn intern_ast(&mut self, ty: &ast::Type<'src>) -> Result<TypeId> {
    if let Some(id) = self.by_ast_hash.get(&hash_of(&ty)) {
      // interned
      return Ok(*id);
    }

    // does not exist yet
    let hash = hash_of(ty);
    let ty = match &ty.kind {
      ast::TypeKind::Empty(_) => return Ok(TypeId::Unknown),
      ast::TypeKind::Named(t) => match self.by_name.get(t.name.lexeme) {
        Some(id) => return Ok(*id),
        None => return Err(err!(@ty.span, NoSuchTypeInScope)),
      },
      ast::TypeKind::Array(t) => Type::Array(self.intern_ast(&t.item)?),
      ast::TypeKind::Set(t) => Type::Set(self.intern_ast(&t.value)?),
      ast::TypeKind::Map(t) => Type::Map(self.intern_ast(&t.key)?, self.intern_ast(&t.value)?),
      ast::TypeKind::Fn(t) => {
        let mut params = Vec::with_capacity(t.params.len());
        for param in &t.params {
          params.push(self.intern_ast(param)?);
        }
        let ret = self.intern_ast(&t.ret)?;
        Type::Fn(params, ret)
      }
      ast::TypeKind::Opt(t) => Type::Opt(self.intern_ast(&t.inner)?),
    };

    let id = self.next_type_id();
    self.by_ast_hash.insert(hash, id);
    self.by_id.push(ty);
    Ok(id)
  }
}

fn hash_of<T: std::hash::Hash>(v: &T) -> u64 {
  use std::hash::Hasher;
  let mut state = rustc_hash::FxHasher::default();
  v.hash(&mut state);
  state.finish()
}

impl TypeId {
  #[inline]
  fn to_index(&self) -> usize {
    // `TypeId` is `repr(u8)`, so we are allowed to read the first field
    let discriminant = unsafe { (self as *const TypeId).cast::<u8>().read() } as usize;
    match self {
      TypeId::Other(id) => discriminant + *id as usize,
      _ => discriminant,
    }
  }
}

mod print;

#[cfg(test)]
mod tests;
