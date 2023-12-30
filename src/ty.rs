#[macro_use]
mod macros;

use std::collections::BTreeMap;

use crate::ast::{self, Ast, Block, Expr, Ident, Stmt};
use crate::error::{Error, ErrorCtx, Result};
use crate::lex::Span;

// TODO: don't ignore keyword args
// they should:
// - allow reordering args
// - parse only after positionals, to allow mixing
// - be unavailable in calls to functions without keyword params
//
// e.g. `(T) -> R` does not allow call with keyword params,
// because there's no information about how to re-order the args.
// this information is only available against calls where the
// callee is a function declaration

#[derive(Debug)]
pub struct Hir<'src> {
    pub defs: Defs<'src>,
    pub fns: Fns<'src>,
    pub top_level: Block<'src, Ty>,
}

pub fn check<'src>(v: &Ast<'src>) -> Result<Hir<'src>, Vec<Error>> {
    // 3. type check top-level code + all function bodies

    let mut tcx = TyCtx::new(v.src);
    register_primitive_types(&mut tcx);

    tcx.enter_scope();
    tcx.declare_types(&v.decls);
    let pending_fns = tcx.collect_fn_sigs(&v.decls);
    tcx.infer_fns(&v.decls, pending_fns);
    let top_level = tcx.infer_block(&v.top_level);
    tcx.leave_scope();

    tcx.finish(top_level)
}

struct TyCtx<'src> {
    ecx: ErrorCtx<'src>,
    defs: Defs<'src>,
    fns: Fns<'src>,

    scopes: Vec<Scope<'src>>,
}

type Scope<'src> = BTreeMap<&'src str, Symbol>;

#[derive(Clone, Copy)]
struct Symbol {
    ty: Ty,
    span: Span,
    kind: SymbolKind,
}

#[derive(Clone, Copy)]
enum SymbolKind {
    Var,
    Fn,
    Cons,
}

impl std::fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            SymbolKind::Var => "variable",
            SymbolKind::Fn => "function",
            SymbolKind::Cons => "type constructor",
        };
        f.write_str(s)
    }
}

impl<'src> TyCtx<'src> {
    fn new(src: &'src str) -> Self {
        Self {
            ecx: ErrorCtx::new(src),
            defs: Defs::new(),
            fns: Fns::new(),

            scopes: Vec::new(),
        }
    }

    fn finish(mut self, top_level: Block<'src, Ty>) -> Result<Hir<'src>, Vec<Error>> {
        self.ecx.finish()?;

        Ok(Hir {
            defs: self.defs,
            fns: self.fns,
            top_level,
        })
    }

    fn declare_types(&mut self, decls: &[ast::Decl<'src>]) {
        // 1. give each unique decl an ID
        let mut pending = Vec::new();
        for decl in decls {
            let ast::decl::DeclKind::Type(decl) = &decl.kind else {
                continue;
            };

            if self.defs.contains(decl.name.as_str()) {
                self.ecx.emit_shadowed_def(decl.name.span);
                continue;
            }

            let id = self.defs.reserve(decl.name.as_str());
            pending.push((id, &**decl));
        }

        // 2. define all types and their constructors
        for (id, decl) in pending {
            let name = decl.name.clone();
            let fields = match &decl.fields {
                ast::decl::Fields::Extern => Fields::Extern,
                ast::decl::Fields::Named(fields) => {
                    let mut out = BTreeMap::new();
                    for (offset, field) in fields.iter().enumerate() {
                        let (name, ty) = self.resolve_field(field);
                        out.insert(field.name.as_str(), Field { name, ty, offset });
                    }
                    Fields::Named(out)
                }
            };

            self.declare_cons(id, name.clone(), &fields);
            self.defs.define(id, TypeDef { id, name, fields });
        }
    }

    fn declare_cons(&mut self, def_id: DefId, name: Ident<'src>, fields: &Fields<'src>) {
        if let Err(e) = self.check_symbol(&name, SymbolKind::Cons) {
            self.ecx.push(e);
            return;
        }

        let (kind, sig) = match fields {
            Fields::Extern => (
                FnKind::ExternCons,
                FnSig {
                    params: vec![],
                    ret: Ty::Def(def_id),
                    ret_span: None,
                },
            ),
            Fields::Named(fields) => (
                FnKind::Cons,
                FnSig {
                    params: fields
                        .values()
                        .map(|field| Param {
                            name: field.name.clone(),
                            ty: field.ty,
                        })
                        .collect(),
                    ret: Ty::Def(def_id),
                    ret_span: None,
                },
            ),
        };

        let id = self.fns.insert(Fn {
            name: name.clone(),
            kind,
            sig,
            body: FnBody::Extern, // compiler-defined
        });
        self.declare_symbol(name, Ty::Fn(id), SymbolKind::Fn);
    }

    fn collect_fn_sigs(
        &mut self,
        fns: &[ast::Decl<'src>],
    ) -> BTreeMap<&'src str, (Ident<'src>, FnSig<'src>)> {
        let mut out = BTreeMap::<&str, (Ident, FnSig)>::new();
        for fn_ in fns {
            let ast::decl::DeclKind::Fn(fn_) = &fn_.kind else {
                continue;
            };
            let name = fn_.name.clone();

            if let Err(e) = self.check_symbol(&name, SymbolKind::Fn) {
                self.ecx.push(e);
                continue;
            }

            if let Some((existing, _)) = out.get(name.as_str()) {
                // let existing: &Ident<'_> = existing;
                self.ecx.emit_invalid_shadowing(
                    name.span,
                    name.as_str(),
                    SymbolKind::Fn,
                    SymbolKind::Fn,
                    existing.span,
                );
                continue;
            }

            let sig = FnSig {
                params: fn_
                    .params
                    .iter()
                    .map(|param| Param {
                        name: param.name.clone(),
                        ty: self.resolve_ast_ty(&param.ty),
                    })
                    .collect(),
                ret: fn_
                    .ret
                    .as_ref()
                    .map(|ty| self.resolve_ast_ty(ty))
                    .unwrap_or(Ty::Unit),
                ret_span: fn_.ret.as_ref().map(|ty| ty.span),
            };

            out.insert(name.as_str(), (name, sig));
        }
        out
    }

    fn enter_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn leave_scope(&mut self) {
        assert!(self.scopes.pop().is_some());
    }

    fn current_scope(&mut self) -> &mut Scope<'src> {
        self.scopes
            .last_mut()
            .expect("accessing current scope without any scope available")
    }

    fn check_symbol(&mut self, name: &Ident<'src>, kind: SymbolKind) -> Result<()> {
        use SymbolKind as S;
        let scope = self.current_scope();
        match scope.get(name.as_str()).copied() {
            Some(existing) if matches!((existing.kind, kind), (S::Var, S::Var)) => Ok(()),
            Some(existing) => Err(self.ecx.invalid_shadowing(
                name.span,
                name.as_str(),
                kind,
                existing.kind,
                existing.span,
            )),
            None => Ok(()),
        }
    }

    fn declare_symbol(&mut self, name: Ident<'src>, ty: Ty, kind: SymbolKind) {
        debug_assert!(self.check_symbol(&name, kind).is_ok());
        self.current_scope().insert(
            name.as_str(),
            Symbol {
                ty,
                span: name.span,
                kind,
            },
        );
    }

    fn resolve_var_ty(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name).copied() {
                return Some(symbol.ty);
            }
        }

        None
    }

    fn resolve_field(&mut self, field: &ast::decl::Field<'src>) -> (Ident<'src>, Ty) {
        let name = field.name.clone();
        let ty = self.resolve_ast_ty(&field.ty);
        (name, ty)
    }

    fn resolve_ast_ty(&mut self, ty: &ast::Ty<'src>) -> Ty {
        match &ty.kind {
            ast::ty::TyKind::Empty => {
                self.ecx.emit_unknown_type(ty.span);
                Ty::Error
            }
            ast::ty::TyKind::Named(ast::ty::Named { name }) => match self.defs.id(name.as_str()) {
                Some(id) => Ty::Def(id),
                None => {
                    self.ecx.emit_undefined_decl(name.span, name.as_str());
                    Ty::Error
                }
            },
        }
    }

    fn infer_fns(
        &mut self,
        fns: &[ast::Decl<'src>],
        mut pending_fns: BTreeMap<&'src str, (Ident<'src>, FnSig<'src>)>,
    ) {
        for fn_ in fns {
            let ast::decl::DeclKind::Fn(fn_) = &fn_.kind else {
                continue;
            };

            let (name, sig) = pending_fns.remove(fn_.name.as_str()).unwrap();

            let body = self.infer_fn_body(&fn_.name, &sig, &fn_.body);
            let id = self.fns.insert(Fn {
                name,
                kind: FnKind::Function,
                sig,
                body,
            });
            self.declare_symbol(fn_.name.clone(), Ty::Fn(id), SymbolKind::Fn);
        }
        assert!(pending_fns.is_empty());
    }

    fn infer_fn_body(
        &mut self,
        name: &Ident<'src>,
        sig: &FnSig<'src>,
        body: &ast::decl::Body<'src>,
    ) -> FnBody<'src> {
        match body {
            ast::decl::Body::Extern => FnBody::Extern,
            ast::decl::Body::Block(block) => {
                // TODO: add fn context so `return` can be type checked
                self.enter_scope();
                for param in &sig.params {
                    // definitely not shadowing anything else, we just entered a fresh scope
                    let _ = self.current_scope().insert(
                        param.name.as_str(),
                        Symbol {
                            ty: param.ty,
                            span: param.name.span,
                            kind: SymbolKind::Var,
                        },
                    );
                }
                let block = self.infer_block(block);
                self.leave_scope();

                let ret_ty = block.tail.as_ref().map(|tail| tail.ty).unwrap_or(Ty::Unit);

                if !self.type_eq(sig.ret, ret_ty) {
                    let p = ty_p!(self);
                    self.ecx
                        .bad_return_type()
                        .fn_name(name.span)
                        .fn_ret(p(sig.ret), sig.ret_span)
                        .ret_val(p(ret_ty), block.tail.as_ref().map(|v| v.span))
                        .emit();
                }

                FnBody::Block(block)
            }
        }
    }

    fn infer_block(&mut self, block: &Block<'src>) -> Block<'src, Ty> {
        self.enter_scope();
        let mut body = Vec::with_capacity(block.body.len());
        for stmt in &block.body {
            body.push(self.infer_stmt(stmt));
        }
        let tail = block.tail.as_ref().map(|tail| self.infer_expr(tail));
        self.leave_scope();

        Block {
            span: block.span,
            body,
            tail,
        }
    }

    fn infer_stmt(&mut self, stmt: &Stmt<'src>) -> Stmt<'src, Ty> {
        match &stmt.kind {
            ast::stmt::StmtKind::Let(v) => self.infer_let(stmt.span, v),
            ast::stmt::StmtKind::Loop(v) => self.infer_loop(stmt.span, v),
            ast::stmt::StmtKind::Expr(v) => self.infer_expr(v).into_stmt(),
        }
    }

    fn infer_let(&mut self, span: Span, v: &ast::stmt::Let<'src>) -> Stmt<'src, Ty> {
        let init = match v.ty.as_ref().map(|ty| self.resolve_ast_ty(ty)) {
            Some(ty) => self.check_expr(&v.init, ty),
            None => self.infer_expr(&v.init),
        };
        match self.check_symbol(&v.name, SymbolKind::Var) {
            Ok(_) => self.declare_symbol(v.name.clone(), init.ty, SymbolKind::Var),
            Err(e) => self.ecx.push(e),
        }
        ast::stmt::Let::new(span, v.name.clone(), v.ty.clone(), init)
    }

    fn infer_loop(&mut self, span: Span, v: &ast::stmt::Loop<'src>) -> Stmt<'src, Ty> {
        ast::stmt::Loop::new(span, self.infer_block(&v.body))
    }

    fn check_expr(&mut self, expr: &Expr<'src>, ty: Ty) -> Expr<'src, Ty> {
        let mut expr = self.infer_expr(expr);
        if !self.type_eq(expr.ty, ty) {
            let p = ty_p!(self);
            self.ecx.emit_type_mismatch(expr.span, p(expr.ty), p(ty));
            expr.ty = Ty::Error;
        }
        expr
    }

    fn infer_expr(&mut self, expr: &Expr<'src>) -> Expr<'src, Ty> {
        use ast::expr::ExprKind as E;
        match &expr.kind {
            E::Return(_) => todo!("infer:Return"),
            E::Break => todo!("infer:Break"),
            E::Continue => todo!("infer:Continue"),
            E::Block(_) => todo!("infer:Block"),
            E::If(_) => todo!("infer:If"),
            E::Binary(v) => self.infer_binary(expr.span, v),
            E::Unary(_) => todo!("infer:Unary"),
            E::Primitive(v) => self.infer_primitive(expr.span, v),
            E::Array(_) => todo!("infer:Array"),
            E::UseVar(v) => self.infer_var_expr(expr.span, v),
            E::UseField(_) => todo!("infer:UseField"),
            E::UseIndex(_) => todo!("infer:UseIndex"),
            E::AssignVar(_) => todo!("infer:AssignVar"),
            E::AssignField(_) => todo!("infer:AssignField"),
            E::AssignIndex(_) => todo!("infer:AssignIndex"),
            E::Call(v) => self.infer_call_expr(expr.span, v),
            E::MethodCall(_) => todo!("infer:MethodCall"),
        }
    }

    fn infer_binary(&mut self, span: Span, v: &ast::expr::Binary<'src>) -> Expr<'src, Ty> {
        fn get_binop_ret_ty<'src>(
            tcx: &mut TyCtx<'src>,
            span: Span,
            lhs: &Expr<'src, Ty>,
            rhs: &Expr<'src, Ty>,
            op: ast::BinaryOp,
        ) -> Ty {
            if !tcx.type_supports_binop(lhs.ty, op) {
                let p = ty_p!(tcx);
                tcx.ecx.emit_unsupported_op(span, p(lhs.ty), op);
                return Ty::Error;
            }

            if !tcx.type_supports_binop(rhs.ty, op) {
                let p = ty_p!(tcx);
                tcx.ecx.emit_unsupported_op(span, p(rhs.ty), op);
                return Ty::Error;
            }

            if !tcx.type_eq(lhs.ty, rhs.ty) {
                let p = ty_p!(tcx);
                tcx.ecx.emit_type_mismatch(span, p(lhs.ty), p(rhs.ty));
                return Ty::Error;
            }

            use ast::BinaryOp as Op;
            match op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Pow => lhs.ty,
                Op::Eq | Op::Ne | Op::Gt | Op::Lt | Op::Ge | Op::Le | Op::And | Op::Or => {
                    Ty::Def(BOOL)
                }
                Op::Opt => todo!("opt binop"),
            }
        }

        let lhs = self.infer_expr(&v.lhs);
        let rhs = self.infer_expr(&v.rhs);
        let ty = get_binop_ret_ty(self, span, &lhs, &rhs, v.op);
        ast::expr::Binary::with(ty, lhs, v.op, rhs)
    }

    fn infer_primitive(&mut self, span: Span, v: &ast::expr::Primitive<'src>) -> Expr<'src, Ty> {
        use ast::expr::Primitive as P;
        match v {
            P::Int(v) => P::int_with(span, Ty::Def(INT), *v),
            P::Num(v) => P::num_with(span, Ty::Def(NUM), *v),
            P::Bool(v) => P::bool_with(span, Ty::Def(BOOL), *v),
            P::Str(v) => P::str_with(span, Ty::Def(STR), v.clone()),
        }
    }

    fn infer_var_expr(&mut self, span: Span, v: &ast::expr::UseVar<'src>) -> Expr<'src, Ty> {
        let ty = match self.resolve_var_ty(v.name.as_str()) {
            Some(ty) => ty,
            None => {
                self.ecx.emit_undefined_var(span);
                Ty::Error
            }
        };

        ast::expr::UseVar::with(span, ty, v.name.clone())
    }

    fn infer_call_expr(&mut self, span: Span, v: &ast::expr::Call<'src>) -> Expr<'src, Ty> {
        let callee = self.infer_expr(&v.callee);
        let args = v
            .args
            .iter()
            .map(|arg| ast::Arg {
                key: arg.key.clone(),
                value: self.infer_expr(&arg.value),
            })
            .collect::<Vec<_>>();
        let ty = self.check_call(callee.span, callee.ty, &args);
        ast::expr::Call::with(span, ty, callee, args)
    }

    fn check_call(&mut self, span: Span, callee: Ty, args: &[ast::Arg<'src, Ty>]) -> Ty {
        match callee {
            Ty::Unit | Ty::Def(_) => {
                let p = ty_p!(self);
                self.ecx.emit_not_callable(span, p(callee));
                Ty::Error
            }
            Ty::Fn(id) => {
                let fn_ = &self.fns[id];
                if fn_.is_extern_cons() {
                    self.ecx
                        .emit_extern_cons_not_callable(span, fn_.name.as_str());
                    return Ty::Error;
                }

                let params = &fn_.sig.params;
                if params.len() != args.len() {
                    self.ecx.emit_param_mismatch(span, params.len(), args.len());
                    return Ty::Error;
                }

                for (i, arg) in args.iter().enumerate() {
                    let arg_ty = arg.value.ty;
                    let param_ty = params[i].ty;
                    if !self.type_eq(arg_ty, param_ty) {
                        let p = ty_p!(self);
                        self.ecx
                            .emit_type_mismatch(arg.value.span, p(arg_ty), p(param_ty));
                    }
                }

                fn_.sig.ret
            }
            Ty::Error => Ty::Error,
        }
    }

    fn type_eq(&self, a: Ty, b: Ty) -> bool {
        match (a, b) {
            (Ty::Unit, Ty::Unit) => true,
            (Ty::Def(a), Ty::Def(b)) => a == b,
            (Ty::Fn(a), Ty::Fn(b)) => self.fn_sig_match(&self.fns[a].sig, &self.fns[b].sig),
            (Ty::Error, _) | (_, Ty::Error) => true,
            _ => false,
        }
    }

    fn type_supports_binop(&self, ty: Ty, op: ast::BinaryOp) -> bool {
        use ast::BinaryOp as Op;
        match op {
            Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Rem
            | Op::Pow
            | Op::Gt
            | Op::Lt
            | Op::Ge
            | Op::Le => ty.is_int() || ty.is_num(),
            Op::Eq | Op::Ne => ty.is_primitive(),
            Op::And | Op::Or => ty.is_bool(),
            Op::Opt => todo!("opt binop"),
        }
    }

    fn fn_sig_match(&self, a: &FnSig<'src>, b: &FnSig<'src>) -> bool {
        if a.params.len() != b.params.len() {
            return false;
        }

        if !self.type_eq(a.ret, b.ret) {
            return false;
        }

        for (a, b) in a.params.iter().zip(b.params.iter()) {
            if !self.type_eq(a.ty, b.ty) {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(u32);

#[derive(Debug)]
pub struct Fns<'src> {
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
    fn new() -> Self {
        Self {
            next_id: FnId(0),
            id_map: BTreeMap::new(),
            array: Vec::new(),
        }
    }

    #[inline]
    fn insert(&mut self, fn_: Fn<'src>) -> FnId {
        let id = self.next_id;
        self.next_id.0 += 1;

        self.id_map.insert(fn_.name.as_str(), id);
        self.array.push(fn_);

        id
    }

    #[inline]
    pub fn contains(&self, name: &'src str) -> bool {
        self.id_map.contains_key(name)
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
}

#[derive(Debug)]
pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub kind: FnKind,
    pub sig: FnSig<'src>,
    pub body: FnBody<'src>,
}

impl<'src> Fn<'src> {
    fn is_extern_cons(&self) -> bool {
        use FnKind as F;
        matches!(self.kind, F::ExternCons)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FnKind {
    /// Regular function declaration
    Function,

    /// Type constructor
    Cons,

    /// Extern type constructor
    ///
    /// Functions of this kind may not be called,
    /// and exist only to provide better error messages.
    ExternCons,
}

#[derive(Debug)]
pub struct FnSig<'src> {
    pub params: Vec<Param<'src>>,
    pub ret: Ty,
    pub ret_span: Option<Span>,
}

#[derive(Debug)]
pub enum FnBody<'src> {
    Extern,
    Block(Block<'src, Ty>),
}

#[derive(Debug)]
pub struct Param<'src> {
    pub name: Ident<'src>,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Defs<'src> {
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
    fn new() -> Self {
        Self {
            next_id: DefId(0),
            id_map: DefIdMap::new(),
            array: Vec::new(),
        }
    }

    #[inline]
    fn is_empty(&self) -> bool {
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
    fn reserve(&mut self, name: &'src str) -> DefId {
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
    fn define(&mut self, id: DefId, def: TypeDef<'src>) {
        assert_eq!(def.id, id);
        self.array[id.0 as usize] = def;
    }
}

type DefIdMap<'src> = BTreeMap<&'src str, DefId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId(u32);

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
}

macro_rules! primitives {
    ($LIST:ident = [$($name:ident),*]) => {
        paste::paste! {
            #[allow(clippy::upper_case_acronyms)]
            #[repr(u32)]
            enum _Builtins {
                $($name),*
            }

            $(
                pub const $name: DefId = DefId(_Builtins::$name as u32);
            )*

            const $LIST: &[(&str, DefId)] = &[
                $((stringify!([<$name:lower>]), $name)),*
            ];

            impl Ty {
                $(
                    pub fn [<is_ $name:lower>](&self) -> bool {
                        match self {
                            Self::Def(id) => *id == $name,
                            _ => false,
                        }
                    }
                )*
            }
        }
    }
}

primitives!(PRIMITIVES = [INT, NUM, BOOL, STR]);

impl Ty {
    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Def(id) => {
                let first = PRIMITIVES.first().unwrap().1;
                let last = PRIMITIVES.last().unwrap().1;
                *id >= first && *id <= last
            }
            _ => false,
        }
    }
}

fn register_primitive_types(tcx: &mut TyCtx<'_>) {
    assert!(
        tcx.defs.is_empty(),
        "`register_builtin_types` called with some type defs already present"
    );

    for (name, id) in PRIMITIVES.iter().copied() {
        assert_eq!(tcx.defs.reserve(name), id);
        tcx.defs.define(
            id,
            TypeDef {
                id,
                name: Ident::raw(name),
                fields: Fields::Extern,
            },
        );
    }
}

#[derive(Clone, Copy)]
pub struct TyPrinter<'a, 'src> {
    defs: &'a Defs<'src>,
    fns: &'a Fns<'src>,
}

impl<'a, 'src> TyPrinter<'a, 'src> {
    pub fn print(self, ty: Ty) -> PrintTy<'a, 'src> {
        PrintTy { printer: self, ty }
    }
}

pub struct PrintTy<'a, 'src> {
    printer: TyPrinter<'a, 'src>,
    ty: Ty,
}

impl<'a, 'src> std::fmt::Display for PrintTy<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            Ty::Unit => f.write_str("()"),
            Ty::Def(id) => {
                let decl = self.printer.defs[id].name.as_str();
                write!(f, "{decl}")
            }
            Ty::Fn(id) => {
                let fn_ = self.printer.fns[id].name.as_str();
                write!(f, "[fn {fn_}]")
            }
            Ty::Error => f.write_str("?!"),
        }
    }
}

#[cfg(test)]
mod tests;
