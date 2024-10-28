use std::collections::BTreeMap;
use std::fmt::Debug;

use crate::ast::{BinaryOp, Ident, UnaryOp};
use crate::lex::Span;
use crate::vm2::value::f64n;
use crate::Str;

pub struct Hir<'src> {
    pub src: &'src str,
    pub(crate) defs: Defs<'src>,
    pub(crate) fns: Fns<'src>,
    pub top_level: Block<'src>,
}

#[derive(Debug, Clone, Copy)]
pub enum Ty {
    /// Unit
    Unit,

    /// Type definition
    Def(DefId),

    /// Function
    Fn(FnId),

    /// Type error
    Error,

    /// Any type, only available in `extern fn` declarations
    Dynamic,

    /// Unreachable after this point
    Unreachable,
}

impl Ty {
    pub fn is_err(&self) -> bool {
        matches!(self, Ty::Error)
    }

    pub fn into_fn(self) -> Option<FnId> {
        match self {
            Self::Fn(id) => Some(id),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct TypeDef<'src> {
    pub id: DefId,
    pub name: Ident<'src>,
    pub fields: Fields<'src>,
}

#[derive(Debug)]
pub enum Fields<'src> {
    Extern,
    Named(FieldMap<'src>),
}

pub type FieldMap<'src> = BTreeMap<&'src str, Field<'src>>;

#[derive(Debug)]
pub struct Field<'src> {
    pub name: Ident<'src>,
    pub ty: Ty,
    pub offset: usize,
}

#[derive(Debug)]
pub(crate) struct Defs<'src> {
    next_id: DefId,
    id_map: DefIdMap<'src>,
    array: Vec<TypeDef<'src>>,
}

impl<'a, 'src> std::ops::Index<&'a str> for Defs<'src> {
    type Output = TypeDef<'src>;

    fn index(&self, index: &'a str) -> &Self::Output {
        self.get_by_name(index).unwrap()
    }
}

impl<'src> std::ops::Index<DefId> for Defs<'src> {
    type Output = TypeDef<'src>;

    fn index(&self, index: DefId) -> &Self::Output {
        self.get_by_id(index).unwrap()
    }
}

impl<'src> Defs<'src> {
    pub(crate) fn new() -> Self {
        Self {
            next_id: DefId(0),
            id_map: DefIdMap::new(),
            array: Vec::new(),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.array.is_empty()
    }

    #[inline]
    pub fn contains(&self, name: &'src str) -> bool {
        self.id_map.contains_key(name)
    }

    #[inline]
    pub fn get_by_id(&self, id: DefId) -> Option<&TypeDef<'src>> {
        self.array.get(id.0 as usize)
    }

    #[inline]
    pub fn id(&self, name: &str) -> Option<DefId> {
        self.id_map.get(name).copied()
    }

    #[inline]
    pub fn get_by_name(&self, name: &str) -> Option<&TypeDef<'src>> {
        self.id(name).and_then(|id| self.get_by_id(id))
    }

    #[inline]
    pub fn reserve(&mut self, name: &'src str) -> DefId {
        let id = self.next_id;
        self.next_id.0 += 1;

        self.id_map.insert(name, id);
        self.array.push(TypeDef {
            id,
            name: Ident::raw(""),
            fields: Fields::Extern,
        });

        id
    }

    #[inline]
    pub fn define(&mut self, id: DefId, def: TypeDef<'src>) {
        assert_eq!(def.id, id);
        self.array[id.0 as usize] = def;
    }

    pub fn iter(&self) -> impl Iterator<Item = &TypeDef<'src>> {
        self.array.iter()
    }
}

type DefIdMap<'src> = BTreeMap<&'src str, DefId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(pub(crate) u32);

#[derive(Debug)]
pub(crate) struct Fns<'src> {
    next_id: FnId,
    id_map: BTreeMap<&'src str, FnId>,
    array: Vec<Fn<'src>>,
}

impl<'a, 'src> std::ops::Index<&'a str> for Fns<'src> {
    type Output = Fn<'src>;

    fn index(&self, index: &'a str) -> &Self::Output {
        self.get_by_name(index).unwrap()
    }
}

impl<'src> std::ops::Index<FnId> for Fns<'src> {
    type Output = Fn<'src>;

    fn index(&self, index: FnId) -> &Self::Output {
        self.get_by_id(index).unwrap()
    }
}

impl<'src> Fns<'src> {
    pub fn new() -> Self {
        Self {
            next_id: FnId(0),
            id_map: BTreeMap::new(),
            array: Vec::new(),
        }
    }

    #[inline]
    pub fn insert(&mut self, fn_: Fn<'src>) -> FnId {
        let id = self.next_id;
        self.next_id.0 += 1;

        self.id_map.insert(fn_.name.as_str(), id);
        self.array.push(fn_);

        id
    }

    #[inline]
    pub(crate) fn get_by_id_mut(&mut self, id: FnId) -> Option<&mut Fn<'src>> {
        self.array.get_mut(id.0 as usize)
    }

    #[inline]
    pub fn get_by_id(&self, id: FnId) -> Option<&Fn<'src>> {
        self.array.get(id.0 as usize)
    }

    #[inline]
    pub fn id(&self, name: &str) -> Option<FnId> {
        self.id_map.get(name).copied()
    }

    #[inline]
    pub fn get_by_name(&self, name: &str) -> Option<&Fn<'src>> {
        self.id(name).and_then(|id| self.get_by_id(id))
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (FnId, &Fn<'src>)> {
        self.id_map
            .values()
            .map(|id| (*id, self.get_by_id(*id).unwrap()))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.id_map.len()
    }
}

#[derive(Debug)]
pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub kind: FnKind,
    pub sig: FnSig<'src>,
    pub body: FnBody<'src>,
}

impl<'src> Fn<'src> {
    pub fn is_extern_fn(&self) -> bool {
        matches!(self.kind, FnKind::Function) && matches!(self.body, FnBody::Extern)
    }

    pub fn is_extern_cons(&self) -> bool {
        matches!(self.kind, FnKind::Cons) && matches!(self.body, FnBody::Extern)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FnKind {
    /// Regular function declaration
    Function,

    /// Type constructor
    Cons,
}

#[derive(Debug, Clone)]
pub struct FnSig<'src> {
    pub params: Vec<Param<'src>>,
    pub ret: Ty,
    pub ret_span: Option<Span>,
}

#[derive(Debug)]
pub enum FnBody<'src> {
    /// Host-defined
    Extern,

    /// Compiler-defined
    Intrinsic,

    /// Script-defined
    Block(Block<'src>),
}

#[derive(Debug, Clone)]
pub struct Param<'src> {
    pub name: Ident<'src>,
    pub ty: Ty,
}

pub struct Block<'src> {
    pub span: Span,
    pub body: Vec<Stmt<'src>>,
    pub tail: Option<Expr<'src>>,
}

impl<'src> Block<'src> {
    pub fn ty(&self) -> Ty {
        match &self.tail {
            Some(tail) => tail.ty,
            None => Ty::Unit,
        }
    }
}

pub struct Stmt<'src> {
    pub span: Span,
    pub kind: StmtKind<'src>,
}

#[derive(Debug)]
pub enum StmtKind<'src> {
    Let(Box<Let<'src>>),
    Loop(Box<Loop<'src>>),
    Expr(Box<Expr<'src>>),
}

#[derive(Debug)]
pub struct Let<'src> {
    pub name: Ident<'src>,
    pub init: Expr<'src>,
}

#[derive(Debug)]
pub struct Loop<'src> {
    pub body: Block<'src>,
}

pub struct Expr<'src> {
    pub span: Span,
    pub ty: Ty,
    pub kind: ExprKind<'src>,
}

impl<'src> Expr<'src> {
    pub fn into_stmt(self) -> Stmt<'src> {
        Stmt {
            span: self.span,
            kind: StmtKind::Expr(Box::new(self)),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind<'src> {
    Return(Box<Return<'src>>),
    Break(Break),
    Continue(Continue),
    Block(Box<Block<'src>>),
    If(Box<If<'src>>),
    Binary(Box<Binary<'src>>),
    Unary(Box<Unary<'src>>),
    Primitive(Box<Primitive<'src>>),
    UseVar(Box<UseVar<'src>>),
    UseField(Box<UseField<'src>>),
    UseIndex(Box<UseIndex<'src>>),
    AssignVar(Box<AssignVar<'src>>),
    AssignField(Box<AssignField<'src>>),
    AssignIndex(Box<AssignIndex<'src>>),
    Call(Box<Call<'src>>),
}

#[derive(Debug)]
pub struct Return<'src> {
    pub value: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct Break;

#[derive(Debug)]
pub struct Continue;

#[derive(Debug)]
pub struct If<'src> {
    pub if_token: Span,
    pub branches: Vec<Branch<'src>>,
    pub tail: Option<Block<'src>>,
}

#[derive(Debug)]
pub struct Branch<'src> {
    pub cond: Expr<'src>,
    pub body: Block<'src>,
}

#[derive(Debug)]
pub struct Binary<'src> {
    pub lhs: Expr<'src>,
    pub op: BinaryOp,
    pub rhs: Expr<'src>,
}

impl<'src> Binary<'src> {
    pub fn is_int(&self) -> bool {
        self.lhs.ty.is_int()
    }

    pub fn is_num(&self) -> bool {
        self.lhs.ty.is_num()
    }
}

#[derive(Debug)]
pub struct Unary<'src> {
    pub op: UnaryOp,
    pub rhs: Expr<'src>,
}

#[derive(Debug)]
pub enum Primitive<'src> {
    Int(i64),
    Num(f64n),
    Bool(bool),
    Str(Str<'src>),
}

#[derive(Debug)]
pub struct UseVar<'src> {
    pub name: Ident<'src>,
}

#[derive(Debug)]
pub struct UseField<'src> {
    pub parent: Expr<'src>,
    pub def: DefId,
    pub name: Ident<'src>,
}

#[derive(Debug)]
pub struct UseIndex<'src> {
    pub parent: Expr<'src>,
    pub key: Expr<'src>,
}

#[derive(Debug)]
pub struct AssignVar<'src> {
    pub name: Ident<'src>,
    pub op: Option<BinaryOp>,
    pub value: Expr<'src>,
}

#[derive(Debug)]
pub struct AssignField<'src> {
    pub parent: Expr<'src>,
    pub name: Ident<'src>,
    pub def: DefId,
    pub op: Option<BinaryOp>,
    pub value: Expr<'src>,
}

#[derive(Debug)]
pub struct AssignIndex<'src> {
    pub parent: Expr<'src>,
    pub key: Expr<'src>,
    pub op: Option<BinaryOp>,
    pub value: Expr<'src>,
}

#[derive(Debug)]
pub struct Call<'src> {
    pub callee: Expr<'src>,
    pub args: Vec<Arg<'src>>,
}

#[derive(Debug)]
pub struct Arg<'src> {
    pub key: Option<Ident<'src>>,
    pub value: Expr<'src>,
}

impl<'src> Debug for Hir<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Hir")
            .field("top_level", &self.top_level)
            .finish()
    }
}

impl<'src> Debug for Block<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("body", &self.body)
            .field("tail", &self.tail)
            .finish()
    }
}

impl<'src> Debug for Stmt<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            StmtKind::Let(node) => Debug::fmt(node, f),
            StmtKind::Loop(node) => Debug::fmt(node, f),
            StmtKind::Expr(node) => Debug::fmt(node, f),
        }
    }
}

impl<'src> Debug for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::Return(node) => Debug::fmt(node, f),
            ExprKind::Break(node) => Debug::fmt(node, f),
            ExprKind::Continue(node) => Debug::fmt(node, f),
            ExprKind::Block(node) => Debug::fmt(node, f),
            ExprKind::If(node) => Debug::fmt(node, f),
            ExprKind::Binary(node) => Debug::fmt(node, f),
            ExprKind::Unary(node) => Debug::fmt(node, f),
            ExprKind::Primitive(node) => Debug::fmt(node, f),
            ExprKind::UseVar(node) => Debug::fmt(node, f),
            ExprKind::UseField(node) => Debug::fmt(node, f),
            ExprKind::UseIndex(node) => Debug::fmt(node, f),
            ExprKind::AssignVar(node) => Debug::fmt(node, f),
            ExprKind::AssignField(node) => Debug::fmt(node, f),
            ExprKind::AssignIndex(node) => Debug::fmt(node, f),
            ExprKind::Call(node) => Debug::fmt(node, f),
        }
    }
}

#[cfg(test)]
pub(crate) struct DisplayHir<'a>(pub(crate) &'a Hir<'a>);

#[cfg(test)]
impl std::fmt::Display for DisplayHir<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:#?}\n", self.0.top_level)?;

        for TypeDef { name, fields, id } in self.0.defs.iter() {
            if Ty::Def(*id).is_primitive() {
                continue;
            }

            match fields {
                Fields::Extern => {
                    writeln!(f, "extern type {name};")?;
                }
                Fields::Named(fields) => {
                    write!(f, "type {name}(")?;
                    let mut fields = fields.iter().peekable();
                    while let Some((name, field)) = fields.next() {
                        write!(f, "{name}: {}", p!(self.0, field.ty))?;
                        if fields.peek().is_some() {
                            write!(f, ", ")?;
                        }
                    }
                    writeln!(f, ")")?;
                }
            }
        }

        writeln!(f)?;
        if !self.0.fns.array.is_empty() {
            writeln!(f, "{:#?}", self.0.fns.array)?;
        }

        Ok(())
    }
}
