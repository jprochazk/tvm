use std::hash::Hash;
use std::ops::{Index, IndexMut};

use crate::ast::{self, Ast, Ident};
use crate::error::{Error, Result};
use crate::lex::Span;
use crate::HashMap;

// TODO: pre-intern primitives

pub fn check(mut ast: Ast<'_>) -> Result<Hir<'_>, Vec<Error>> {
  let mut ctx = Ctx::new();

  check_top_level(&mut ctx, &mut ast.top_level);

  if !ctx.errors.is_empty() {
    return Err(ctx.errors);
  }

  Ok(Hir {
    src: ast.src,
    types: ctx.types,
    top_level: ast.top_level,
  })
}

fn check_top_level<'src>(ctx: &mut Ctx<'src>, top_level: &mut ast::Block<'src>) {
  ctx.enter_scope();
  for stmt in &mut top_level.body {
    check_stmt(ctx, stmt);
  }
  ctx.leave_scope();
}

fn check_stmt<'src>(ctx: &mut Ctx<'src>, stmt: &mut ast::Stmt<'src>) {
  match &mut stmt.kind {
    ast::StmtKind::Let(node) => {
      let ty = match &node.ty {
        Some(ty) => ctx.ann_to_ty(ty),
        None => ctx.fresh(),
      };
      let val_ty = infer(ctx, &mut node.init);
      unify(ctx, node.init.span, ty, val_ty);
      ctx.scope().bind(node.name, ty);
    }
    ast::StmtKind::Loop(_) => todo!(),
    ast::StmtKind::Expr(node) => {
      infer(ctx, &mut node.inner);
    }
  }
}

pub struct Hir<'src> {
  pub src: &'src str,
  // pub defs: IndexMap<DefId, Def<'src>>,
  types: Types,
  pub top_level: ast::Block<'src>,
}

pub struct Ctx<'src> {
  types: Types,
  scopes: Vec<Scope<'src>>,
  errors: Vec<Error>,
}

impl<'src> Ctx<'src> {
  fn new() -> Self {
    let mut types = Types::new();
    types.intern(Type::Error);
    Self {
      types,
      scopes: Vec::new(),
      errors: Vec::new(),
    }
  }

  fn fresh(&mut self) -> TypeId {
    self.types.fresh()
  }

  fn ann_to_ty(&mut self, ast: &ast::Type<'src>) -> TypeId {
    match &ast.kind {
      ast::TypeKind::Empty(_) => self.fresh(),
      ast::TypeKind::Named(_) => todo!(),
      ast::TypeKind::Array(inner) => {
        let item_ty = self.ann_to_ty(&inner.item);
        self.intern(Type::Array(item_ty))
      }
      ast::TypeKind::Fn(_) => todo!(),
      ast::TypeKind::Opt(_) => todo!(),
    }
  }

  fn resolve(&self, ty: TypeId) -> Type {
    self.types.resolve(ty)
  }

  fn enter_scope(&mut self) {
    self.scopes.push(Scope::new())
  }

  fn leave_scope(&mut self) {
    self.scopes.pop().unwrap();
  }

  fn scope(&mut self) -> &mut Scope<'src> {
    self.scopes.last_mut().unwrap()
  }

  fn binding(&mut self, name: Ident<'src>) -> TypeId {
    for scope in self.scopes.iter().rev() {
      if let Some(id) = scope.get(name) {
        return id;
      }
    }

    self.errors.push(err!(@name.span, UndefinedVar));
    TypeId::ERROR
  }

  fn intern(&mut self, ty: Type) -> TypeId {
    self.types.intern(ty)
  }
}

fn infer<'src>(ctx: &mut Ctx<'src>, e: &mut ast::Expr<'src>) -> TypeId {
  fn lit<'src>(ctx: &mut Ctx<'src>, e: &mut ast::Literal<'src>) -> TypeId {
    match e {
      ast::Literal::None => ctx.intern(Type::None),
      ast::Literal::Int(_) => ctx.intern(Type::Int),
      ast::Literal::Float(_) => ctx.intern(Type::Num),
      ast::Literal::Bool(_) => ctx.intern(Type::Bool),
      ast::Literal::String(_) => ctx.intern(Type::Str),
      ast::Literal::Array(node) => match node {
        ast::Array::List(items) => {
          let item_ty = match &mut items[..] {
            [] => ctx.fresh(),
            [f, ..] => {
              let item_ty = infer(ctx, f);
              for item in &mut items[1..] {
                let ty = infer(ctx, item);
                unify(ctx, item.span, item_ty, ty);
              }
              item_ty
            }
          };
          ctx.intern(Type::Array(item_ty))
        }
        ast::Array::Copy(item, len) => {
          let item_ty = infer(ctx, item);
          let len_ty = infer(ctx, len);
          let expected_len_ty = ctx.intern(Type::Int);
          unify(ctx, len.span, len_ty, expected_len_ty);
          ctx.intern(Type::Array(item_ty))
        }
      },
    }
  }

  let ty = match &mut e.kind {
    ast::ExprKind::Return(_) => todo!(),
    ast::ExprKind::Yield(_) => todo!(),
    ast::ExprKind::Break(_) => todo!(),
    ast::ExprKind::Continue(_) => todo!(),
    ast::ExprKind::Block(_) => todo!(),
    ast::ExprKind::If(_) => todo!(),
    ast::ExprKind::Binary(node) => {
      let left = infer(ctx, &mut node.left);
      let right = infer(ctx, &mut node.right);
      unify(ctx, e.span, left, right);
      left
    }
    ast::ExprKind::Unary(_) => todo!(),
    ast::ExprKind::Literal(node) => lit(ctx, &mut node.value),
    ast::ExprKind::Use(node) => match &mut node.place {
      ast::Place::Var { name } => ctx.binding(*name),
      ast::Place::Field { .. } => todo!(),
      ast::Place::Index { parent, key } => {
        let parent_ty = infer(ctx, parent);
        match ctx.resolve(parent_ty) {
          Type::Array(item) => {
            let key_ty = infer(ctx, key);
            let expected_ty = ctx.intern(Type::Int);
            unify(ctx, key.span, key_ty, expected_ty);
            item
          }
          _ => todo!(),
        }
      }
    },
    ast::ExprKind::Assign(_) => todo!(),
    ast::ExprKind::Call(_) => todo!(),
  };

  e.ty = ty;
  ty
}

fn unify(ctx: &mut Ctx<'_>, span: Span, l: TypeId, r: TypeId) {
  let lty = ctx.resolve(l);
  let rty = ctx.resolve(r);
  match (lty, rty) {
    (Type::Error, _) => (),
    (_, Type::Error) => (),

    (Type::Infer(var), _) => match ctx.types.vars[var] {
      Some(l) => unify(ctx, span, l, r),
      None => ctx.types.vars[var] = Some(r),
    },
    (_, Type::Infer(var)) => match ctx.types.vars[var] {
      Some(r) => unify(ctx, span, l, r),
      None => ctx.types.vars[var] = Some(l),
    },

    (Type::Int, Type::Int) => (),
    (Type::Num, Type::Num) => (),
    (Type::Bool, Type::Bool) => (),
    (Type::Str, Type::Str) => (),
    (Type::Array(l), Type::Array(r)) => unify(ctx, span, l, r),

    _ => ctx.errors.push(err!(@span, TypeError)),
  }
}

struct Scope<'src> {
  bindings: HashMap<&'src str, TypeId>,
}

impl<'src> Scope<'src> {
  fn new() -> Self {
    Self {
      bindings: HashMap::default(),
    }
  }

  fn bind(&mut self, name: Ident<'src>, ty: TypeId) -> Option<()> {
    self.bindings.insert(name.lexeme, ty).map(|_| ())
  }

  fn get(&self, name: Ident<'src>) -> Option<TypeId> {
    self.bindings.get(name.lexeme).copied()
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

impl TypeId {
  const ERROR: TypeId = TypeId(0);
}

impl Default for TypeId {
  fn default() -> Self {
    Self::ERROR
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
  #[default]
  Error,

  None,
  Int,
  Num,
  Bool,
  Str,
  Array(TypeId),

  Infer(VarId),
}

pub struct Types {
  pool: Pool<Type>,
  vars: TypeVars,
}

impl Types {
  fn new() -> Self {
    Self {
      pool: Pool::new(),
      vars: TypeVars::new(),
    }
  }

  fn fresh(&mut self) -> TypeId {
    let ty = Type::Infer(self.vars.fresh());
    self.intern(ty)
  }

  fn intern(&mut self, ty: Type) -> TypeId {
    TypeId(self.pool.insert(ty))
  }

  fn resolve(&self, ty: TypeId) -> Type {
    match &self.pool[ty] {
      ty @ Type::Infer(var) => match self.vars[*var] {
        Some(ty) => self.resolve(ty),
        None => ty.clone(),
      },
      ty => ty.clone(),
    }
  }
}

struct TypeVars {
  a: Vec<Option<TypeId>>,
}

impl TypeVars {
  fn new() -> Self {
    Self { a: Vec::new() }
  }

  fn fresh(&mut self) -> VarId {
    let id = VarId(self.a.len());
    self.a.push(None);
    id
  }
}

struct Pool<T: Hash> {
  a: Vec<T>,
  m: HashMap<u64, usize>,
}

impl<T: Hash> Pool<T> {
  fn new() -> Self {
    Self {
      a: Vec::new(),
      m: HashMap::default(),
    }
  }

  fn insert(&mut self, v: T) -> usize {
    let id = self.a.len();
    self.m.insert(hash(&v), id);
    self.a.push(v);
    id
  }
}

impl Index<TypeId> for Pool<Type> {
  type Output = Type;

  fn index(&self, index: TypeId) -> &Self::Output {
    &self.a[index.0]
  }
}

impl Index<VarId> for TypeVars {
  type Output = Option<TypeId>;

  fn index(&self, index: VarId) -> &Self::Output {
    &self.a[index.0]
  }
}

impl IndexMut<VarId> for TypeVars {
  fn index_mut(&mut self, index: VarId) -> &mut Self::Output {
    &mut self.a[index.0]
  }
}

impl Index<VarId> for Types {
  type Output = Option<TypeId>;

  fn index(&self, index: VarId) -> &Self::Output {
    &self.vars[index]
  }
}

impl IndexMut<VarId> for Types {
  fn index_mut(&mut self, index: VarId) -> &mut Self::Output {
    &mut self.vars[index]
  }
}

fn hash<T: Hash>(v: &T) -> u64 {
  use std::hash::Hasher;
  let mut s = rustc_hash::FxHasher::default();
  v.hash(&mut s);
  s.finish()
}

#[cfg(test)]
mod tests;

mod print;
