use std::collections::BTreeMap;

use crate::ast::{self, Ast, Block, Expr, Ident, Stmt};
use crate::error::{Error, ErrorCtx, Result};
use crate::lex::Span;

pub struct Hir<'src> {
    pub decls: Decls<'src>,
    pub fns: Fns<'src>,
    pub top_level: Block<'src, Ty>,
}

pub fn check<'src>(v: &Ast<'src>) -> Result<Hir<'src>, Vec<Error>> {
    // 3. type check top-level code + all function bodies

    let mut tcx = TyCtx::new(v.src);

    tcx.declare_types(&v.decls);
    let pending_fns = tcx.declare_fns(&v.decls);
    tcx.infer_fns(&v.decls, pending_fns);
    let top_level = tcx.infer_block(&v.top_level);

    tcx.finish(top_level)
}

struct TyCtx<'src> {
    ecx: ErrorCtx<'src>,
    decls: Decls<'src>,
    fns: Fns<'src>,

    scopes: Vec<BTreeMap<&'src str, Ty>>,
}

type Scope<'src> = BTreeMap<&'src str, Ty>;

impl<'src> TyCtx<'src> {
    fn new(src: &'src str) -> Self {
        Self {
            ecx: ErrorCtx::new(src),
            decls: Decls::new(),
            fns: Fns::new(),

            scopes: Vec::new(),
        }
    }

    fn finish(mut self, top_level: Block<'src, Ty>) -> Result<Hir<'src>, Vec<Error>> {
        self.ecx.finish()?;

        Ok(Hir {
            decls: self.decls,
            fns: self.fns,
            top_level,
        })
    }

    fn declare_types(&mut self, decls: &[ast::Decl<'src>]) {
        // TODO: put a constructor in scope for each decl

        // 1. give each unique decl an ID
        let mut pending = Vec::new();
        for decl in decls {
            let ast::decl::DeclKind::Type(decl) = &decl.kind else {
                continue;
            };

            if self.decls.contains(decl.name.as_str()) {
                self.err(|ecx, _| ecx.duplicate_decl(decl.name.span, decl.name.as_str()));
                continue;
            }

            let id = self.decls.reserve(decl.name.as_str());
            pending.push((id, &**decl));
        }

        // 2. define all decls
        for (id, decl) in pending {
            let name = decl.name.clone();
            let fields = match &decl.fields {
                ast::decl::Fields::Extern => DeclFields::Extern,
                ast::decl::Fields::Named(fields) => {
                    let mut out = BTreeMap::new();
                    for (offset, field) in fields.iter().enumerate() {
                        let (name, ty) = self.resolve_field(field);
                        out.insert(field.name.as_str(), DeclField { name, ty, offset });
                    }
                    DeclFields::Named(out)
                }
            };

            self.decls.define(id, Decl { id, name, fields });
        }
    }

    fn declare_fns(
        &mut self,
        fns: &[ast::Decl<'src>],
    ) -> BTreeMap<&'src str, (Ident<'src>, FnSig<'src>)> {
        let mut out = BTreeMap::new();
        for fn_ in fns {
            let ast::decl::DeclKind::Fn(fn_) = &fn_.kind else {
                continue;
            };

            if self.fns.contains(fn_.name.as_str()) {
                self.err(|ecx, _| ecx.duplicate_fn(fn_.name.span, fn_.name.as_str()));
                continue;
            }

            let name = fn_.name.clone();
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
        self.scopes.last_mut().unwrap()
    }

    fn resolve_var_ty(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name).copied() {
                return Some(ty);
            }
        }

        None
    }

    fn resolve_field(&mut self, field: &ast::decl::Field<'src>) -> (Ident<'src>, Ty) {
        let name = field.name.clone();
        let ty = self.resolve_ast_ty(&field.ty);
        (name, ty)
    }

    fn resolve_ast_ty(&mut self, ty: &ast::TypeExpr<'src>) -> Ty {
        match &ty.kind {
            ast::ty::TypeExprKind::Empty => self.ty_err(|ecx, _| ecx.unknown_type(ty.span)),
            ast::ty::TypeExprKind::Named(ast::ty::Named { name }) => {
                match self.decls.id(name.as_str()) {
                    Some(id) => Ty::Decl(id),
                    None => self.ty_err(|ecx, _| ecx.undefined_decl(name.span, name.as_str())),
                }
            }
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
            if self.current_scope().contains_key(name.as_str()) {
                self.err(|ecx, _| ecx.duplicate_fn(name.span, name.as_str()));
                continue;
            }

            let body = self.infer_fn_body(&fn_.name, &sig, &fn_.body);
            let id = self.fns.insert(Fn { name, sig, body });
            let _ = self.current_scope().insert(fn_.name.as_str(), Ty::Fn(id));
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
                    let _ = self.current_scope().insert(param.name.as_str(), param.ty);
                }
                let block = self.infer_block(block);
                self.leave_scope();

                let ret_ty = block.tail.as_ref().map(|tail| tail.ty).unwrap_or(Ty::Unit);

                if sig.ret != ret_ty {
                    self.err(|ecx, p| {
                        let ret_ty = sig.ret_span.map(|span| (p.print(sig.ret), span));
                        let ret_val = block.tail.as_ref().map(|v| (p.print(v.ty), v.span));
                        ecx.bad_return_type()
                            .fn_name(name.span)
                            .ret_ty(ret_ty)
                            .ret_val(ret_val)
                            .finish()
                    });
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
        // shadow previous declaration
        let _ = self.current_scope().insert(v.name.as_str(), init.ty);
        ast::stmt::Let::new(span, v.name.clone(), v.ty.clone(), init)
    }

    fn infer_loop(&mut self, span: Span, v: &ast::stmt::Loop<'src>) -> Stmt<'src, Ty> {
        ast::stmt::Loop::new(span, self.infer_block(&v.body))
    }

    fn check_expr(&mut self, expr: &Expr<'src>, ty: Ty) -> Expr<'src, Ty> {
        let mut expr = self.infer_expr(expr);
        if expr.ty != ty {
            expr.ty = self.ty_err(|ecx, decls| {
                ecx.type_mismatch(expr.span, decls.print(expr.ty), decls.print(ty))
            });
        }
        expr
    }

    fn infer_expr(&mut self, expr: &Expr<'src>) -> Expr<'src, Ty> {
        match &expr.kind {
            ast::expr::ExprKind::Return(_) => todo!("infer:Return"),
            ast::expr::ExprKind::Break => todo!("infer:Break"),
            ast::expr::ExprKind::Continue => todo!("infer:Continue"),
            ast::expr::ExprKind::Block(_) => todo!("infer:Block"),
            ast::expr::ExprKind::If(_) => todo!("infer:If"),
            ast::expr::ExprKind::Binary(_) => todo!("infer:Binary"),
            ast::expr::ExprKind::Unary(_) => todo!("infer:Unary"),
            ast::expr::ExprKind::Primitive(_) => todo!("infer:Primitive"),
            ast::expr::ExprKind::Array(_) => todo!("infer:Array"),
            ast::expr::ExprKind::UseVar(v) => self.infer_var_expr(expr.span, v),
            ast::expr::ExprKind::UseField(_) => todo!("infer:UseField"),
            ast::expr::ExprKind::UseIndex(_) => todo!("infer:UseIndex"),
            ast::expr::ExprKind::AssignVar(_) => todo!("infer:AssignVar"),
            ast::expr::ExprKind::AssignField(_) => todo!("infer:AssignField"),
            ast::expr::ExprKind::AssignIndex(_) => todo!("infer:AssignIndex"),
            ast::expr::ExprKind::Call(v) => self.infer_call_expr(expr.span, v),
            ast::expr::ExprKind::MethodCall(_) => todo!("infer:MethodCall"),
        }
    }

    fn infer_var_expr(&mut self, span: Span, v: &ast::expr::UseVar<'src>) -> ast::Expr<'src, Ty> {
        let ty = match self.resolve_var_ty(v.name.as_str()) {
            Some(ty) => ty,
            None => self.ty_err(|ecx, _| ecx.undefined_var(span)),
        };

        ast::expr::UseVar::with(span, ty, v.name.clone())
    }

    fn infer_call_expr(&mut self, span: Span, v: &ast::expr::Call<'src>) -> ast::Expr<'src, Ty> {
        let callee = self.infer_expr(&v.callee);
        let args = v
            .args
            .iter()
            .map(|arg| ast::Arg {
                key: arg.key.clone(),
                value: self.infer_expr(&arg.value),
            })
            .collect::<Vec<_>>();

        todo!()
    }

    fn err(&mut self, f: impl FnOnce(&mut ErrorCtx<'src>, &TyPrinter<'_, 'src>) -> Error) {
        let err = f(
            &mut self.ecx,
            &TyPrinter {
                decls: &self.decls,
                fns: &self.fns,
            },
        );
        self.ecx.push(err);
    }

    fn ty_err(&mut self, f: impl FnOnce(&mut ErrorCtx<'src>, &TyPrinter<'_, 'src>) -> Error) -> Ty {
        self.err(f);
        Ty::Error
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(u32);

pub struct Fns<'src> {
    next_id: FnId,
    id_map: BTreeMap<&'src str, FnId>,
    array: Vec<Fn<'src>>,
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

pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub sig: FnSig<'src>,
    pub body: FnBody<'src>,
}

pub struct FnSig<'src> {
    pub params: Vec<Param<'src>>,
    pub ret: Ty,
    pub ret_span: Option<Span>,
}

pub enum FnBody<'src> {
    Extern,
    Block(Block<'src, Ty>),
}

pub struct Param<'src> {
    pub name: Ident<'src>,
    pub ty: Ty,
}

pub struct Decls<'src> {
    next_id: DeclId,
    id_map: DeclIdMap<'src>,
    array: Vec<Decl<'src>>,
}

impl<'src> Decls<'src> {
    fn new() -> Self {
        Self {
            next_id: DeclId(0),
            id_map: DeclIdMap::new(),
            array: Vec::new(),
        }
    }

    #[inline]
    pub fn contains(&self, name: &'src str) -> bool {
        self.id_map.contains_key(name)
    }

    #[inline]
    pub fn get_by_id(&self, id: DeclId) -> Option<&Decl<'src>> {
        self.array.get(id.0 as usize)
    }

    #[inline]
    pub fn id(&self, name: &str) -> Option<DeclId> {
        self.id_map.get(name).copied()
    }

    #[inline]
    pub fn get_by_name(&self, name: &str) -> Option<&Decl<'src>> {
        self.id(name).and_then(|id| self.get_by_id(id))
    }

    #[inline]
    fn reserve(&mut self, name: &'src str) -> DeclId {
        let id = self.next_id;
        self.next_id.0 += 1;

        self.id_map.insert(name, id);
        self.array.push(Decl {
            id,
            name: Ident::raw(""),
            fields: DeclFields::Extern,
        });

        id
    }

    #[inline]
    fn define(&mut self, id: DeclId, def: Decl<'src>) {
        assert_eq!(def.id, id);
        self.array[id.0 as usize] = def;
    }
}

type DeclIdMap<'src> = BTreeMap<&'src str, DeclId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeclId(u32);

pub struct Decl<'src> {
    pub id: DeclId,
    pub name: Ident<'src>,
    pub fields: DeclFields<'src>,
}

pub enum DeclFields<'src> {
    Extern,
    Named(DeclFieldMap<'src>),
}

pub type DeclFieldMap<'src> = BTreeMap<&'src str, DeclField<'src>>;

pub struct DeclField<'src> {
    pub name: Ident<'src>,
    pub ty: Ty,
    pub offset: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Ty {
    Infer,
    Unit,
    Decl(DeclId),
    Fn(FnId),
    Error,
}

pub struct TyPrinter<'a, 'src> {
    decls: &'a Decls<'src>,
    fns: &'a Fns<'src>,
}

impl<'a, 'src> TyPrinter<'a, 'src> {
    pub fn print<'b>(&'b self, ty: Ty) -> PrintTy<'a, 'b, 'src> {
        PrintTy { printer: self, ty }
    }
}

pub struct PrintTy<'a, 'b, 'src> {
    printer: &'b TyPrinter<'a, 'src>,
    ty: Ty,
}

impl<'a, 'b, 'src> std::fmt::Display for PrintTy<'a, 'b, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            Ty::Infer => f.write_str("_"),
            Ty::Unit => f.write_str("()"),
            Ty::Decl(id) => f.write_str(self.printer.decls.get_by_id(id).unwrap().name.as_str()),
            Ty::Fn(id) => f.write_str(self.printer.fns.get_by_id(id).unwrap().name.as_str()),
            Ty::Error => f.write_str("{unknown}"),
        }
    }
}
