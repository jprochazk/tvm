use std::collections::BTreeMap;

use crate::ast::{self, Ast, Ident};
use crate::error::{ErrorCtx, Report, Result};
use crate::hir::*;
use crate::lex::Span;
use crate::util::default;
use crate::HashSet;

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

pub fn check<'src>(ast: &Ast<'src>) -> Result<Hir<'src>, Report> {
    let mut tcx = TyCtx::new(ast.src);
    register_primitive_types(&mut tcx);

    tcx.enter_scope();
    tcx.declare_types(&ast.decls);
    tcx.infer_fns(&ast.decls);
    let top_level = tcx.infer_block(&ast.top_level);
    tcx.leave_scope();

    tcx.finish(top_level)
}

struct TyCtx<'src> {
    src: &'src str,
    ecx: ErrorCtx<'src>,
    defs: Defs<'src>,
    fns: Fns<'src>,
    current_fn: Option<FnCtx>,
    scopes: Vec<Scope<'src>>,
}

struct FnCtx {
    ret_ty: Ty,
}

type Scope<'src> = BTreeMap<&'src str, Symbol>;

#[derive(Clone, Copy)]
struct Symbol {
    ty: Ty,
    span: Span,
    kind: SymbolKind,
}

#[derive(Clone, Copy, PartialEq, Eq)]
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
            src,
            ecx: ErrorCtx::new(src),
            defs: Defs::new(),
            fns: Fns::new(),
            current_fn: None,
            scopes: Vec::new(),
        }
    }

    fn finish(mut self, top_level: Block<'src>) -> Result<Hir<'src>, Report> {
        self.ecx.finish()?;

        Ok(Hir {
            src: self.src,
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
            let name = decl.name;
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

            self.declare_cons(id, name, &fields);
            self.defs.define(id, TypeDef { id, name, fields });
        }
    }

    fn declare_cons(&mut self, def_id: DefId, name: Ident<'src>, fields: &Fields<'src>) {
        if let Err(e) = self.check_symbol(&name, SymbolKind::Cons) {
            self.ecx.push(e);
            return;
        }

        let (kind, sig, body) = match fields {
            Fields::Extern => (
                FnKind::Cons,
                FnSig {
                    params: vec![],
                    ret: Ty::Def(def_id),
                    ret_span: None,
                },
                FnBody::Extern,
            ),
            Fields::Named(fields) => (
                FnKind::Cons,
                FnSig {
                    params: fields
                        .values()
                        .map(|field| Param {
                            name: field.name,
                            ty: field.ty,
                        })
                        .collect(),
                    ret: Ty::Def(def_id),
                    ret_span: None,
                },
                FnBody::Intrinsic,
            ),
        };

        let id = self.fns.insert(Fn {
            name,
            kind,
            sig,
            body,
        });
        self.declare_symbol(name, Ty::Fn(id), SymbolKind::Fn);
    }

    fn infer_fns(&mut self, decls: &[ast::Decl<'src>]) {
        let mut fns: Vec<(FnId, Ident, FnSig, &ast::decl::Body)> = default();
        let mut seen: HashSet<Ident<'src>> = default();
        for decl in decls {
            let ast::decl::DeclKind::Fn(fn_) = &decl.kind else {
                continue;
            };
            let name = fn_.name;

            if let Err(e) = self.check_symbol(&name, SymbolKind::Fn) {
                self.ecx.push(e);
                continue;
            }

            if let Some(existing) = seen.get(&name) {
                self.ecx.emit_invalid_shadowing(
                    name.span,
                    name.as_str(),
                    SymbolKind::Fn,
                    SymbolKind::Fn,
                    existing.span,
                );
                continue;
            }
            seen.insert(name);

            let is_extern = fn_.body.is_extern();
            let sig = FnSig {
                params: fn_
                    .params
                    .iter()
                    .map(|param| Param {
                        name: param.name,
                        ty: self.resolve_ast_ty_in_fn_sig(&param.ty, is_extern),
                    })
                    .collect(),
                ret: fn_
                    .ret
                    .as_ref()
                    .map(|ty| self.resolve_ast_ty(ty))
                    .unwrap_or(Ty::Unit),
                ret_span: fn_.ret.as_ref().map(|ty| ty.span),
            };

            fns.push((FnId(u32::MAX), name, sig, &fn_.body));
        }

        for (id, name, sig, body) in &mut fns {
            *id = self.fns.insert(Fn {
                name: *name,
                kind: FnKind::Function,
                sig: sig.clone(),
                body: if body.is_extern() {
                    FnBody::Extern
                } else {
                    FnBody::Intrinsic
                },
            });
            self.declare_symbol(*name, Ty::Fn(*id), SymbolKind::Fn);
        }

        for (id, name, sig, body) in fns {
            let body = self.infer_fn_body(&name, &sig, body);
            let fn_ = self.fns.get_by_id_mut(id).unwrap();
            fn_.body = body;
        }
    }

    fn enter_fn(&mut self, ret_ty: Ty) {
        assert!(self.current_fn.is_none());
        self.current_fn = Some(FnCtx { ret_ty });
    }

    fn leave_fn(&mut self) {
        let _ = self.current_fn.take();
    }

    fn enter_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn leave_scope(&mut self) {
        assert!(self.scopes.pop().is_some());
    }

    fn current_scope(&mut self) -> &mut Scope<'src> {
        self.scopes.last_mut().expect("ICE: no open scope")
    }

    fn check_symbol(&mut self, name: &Ident<'src>, kind: SymbolKind) -> Result<()> {
        use SymbolKind as S;
        match self.current_scope().get(name.as_str()).copied() {
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

    fn resolve_var(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            let Some(symbol) = scope.get(name).copied() else {
                continue;
            };
            return Some(symbol);
        }

        None
    }

    fn resolve_field(&mut self, field: &ast::decl::Field<'src>) -> (Ident<'src>, Ty) {
        let name = field.name;
        let ty = self.resolve_ast_ty(&field.ty);
        (name, ty)
    }

    fn resolve_ast_ty_in_fn_sig(&mut self, ty: &ast::Ty<'src>, is_extern: bool) -> Ty {
        use ast::ty::{Named, TyKind as T};
        match &ty.kind {
            T::Named(Named { name }) if name.as_str() == "dynamic" => {
                if is_extern {
                    Ty::Dynamic
                } else {
                    self.ecx.emit_any_outside_extern_fn(ty.span);
                    Ty::Error
                }
            }
            _ => self.resolve_ast_ty(ty),
        }
    }

    fn resolve_ast_ty(&mut self, ty: &ast::Ty<'src>) -> Ty {
        use ast::ty::{Named, TyKind as T};
        match &ty.kind {
            T::Empty => {
                self.ecx.emit_unknown_type(ty.span);
                Ty::Error
            }
            T::Named(Named { name }) => match self.defs.id(name.as_str()) {
                Some(id) => Ty::Def(id),
                None => {
                    self.ecx.emit_undefined_decl(name.span, name.as_str());
                    Ty::Error
                }
            },
        }
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
                self.enter_fn(sig.ret);
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
                self.leave_fn();

                let ret_ty = block.ty();

                if !self.type_eq(sig.ret, ret_ty) {
                    self.ecx
                        .bad_return_type()
                        .fn_name(name.span)
                        .fn_ret(p!(self, sig.ret), sig.ret_span)
                        .ret_val(p!(self, ret_ty), block.tail.as_ref().map(|v| v.span))
                        .emit();
                }

                FnBody::Block(block)
            }
        }
    }

    fn infer_block(&mut self, block: &ast::Block<'src>) -> Block<'src> {
        self.enter_scope();
        let mut body = Vec::with_capacity(block.body.len());
        for stmt in &block.body {
            body.push(self.infer_stmt(stmt));
        }
        let tail = block
            .tail
            .as_ref()
            .map(|tail| self.infer_expr(tail, NoDiscard));
        self.leave_scope();

        Block {
            span: block.span,
            body,
            tail,
        }
    }

    fn infer_stmt(&mut self, stmt: &ast::Stmt<'src>) -> Stmt<'src> {
        match &stmt.kind {
            ast::stmt::StmtKind::Let(v) => self.infer_let(stmt.span, v),
            ast::stmt::StmtKind::Loop(v) => self.infer_loop(stmt.span, v),
            ast::stmt::StmtKind::Expr(v) => self.infer_expr(v, Discard).into_stmt(),
        }
    }

    fn infer_let(&mut self, span: Span, v: &ast::stmt::Let<'src>) -> Stmt<'src> {
        let init = match v.ty.as_ref().map(|ty| self.resolve_ast_ty(ty)) {
            Some(ty) => self.check_expr(&v.init, ty, NoDiscard),
            None => self.infer_expr(&v.init, NoDiscard),
        };
        match self.check_symbol(&v.name, SymbolKind::Var) {
            Ok(_) => self.declare_symbol(v.name, init.ty, SymbolKind::Var),
            Err(e) => self.ecx.push(e),
        }
        Stmt {
            span,
            kind: StmtKind::Let(Box::new(Let { name: v.name, init })),
        }
    }

    fn infer_loop(&mut self, span: Span, v: &ast::stmt::Loop<'src>) -> Stmt<'src> {
        Stmt {
            span,
            kind: StmtKind::Loop(Box::new(Loop {
                body: self.infer_block(&v.body),
            })),
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr<'src>, ty: Ty, intent: Intent) -> Expr<'src> {
        let mut expr = self.infer_expr(expr, intent);
        if !self.type_eq(expr.ty, ty) {
            self.ecx
                .emit_type_mismatch(expr.span, p!(self, expr.ty), p!(self, ty));
            expr.ty = Ty::Error;
        }
        expr
    }

    fn infer_expr(&mut self, expr: &ast::Expr<'src>, intent: Intent) -> Expr<'src> {
        use ast::expr::ExprKind as E;
        match &expr.kind {
            E::Return(v) => self.infer_return(expr.span, v),
            E::Break => self.infer_break(expr.span),
            E::Continue => self.infer_continue(expr.span),
            E::Block(v) => self.infer_block_expr(expr.span, v),
            E::If(v) => self.infer_if(expr.span, v, intent),
            E::Binary(v) => self.infer_binary(expr.span, v),
            E::Unary(v) => self.infer_unary(expr.span, v),
            E::Primitive(v) => self.infer_primitive(expr.span, v),
            E::Array(_) => todo!("infer:Array"),
            E::UseVar(v) => self.infer_var_expr(expr.span, v),
            E::UseField(_) => todo!("infer:UseField"),
            E::UseIndex(_) => todo!("infer:UseIndex"),
            E::AssignVar(v) => self.infer_var_assign(expr.span, v),
            E::AssignField(_) => todo!("infer:AssignField"),
            E::AssignIndex(_) => todo!("infer:AssignIndex"),
            E::Call(v) => self.infer_call_expr(expr.span, v),
            E::MethodCall(_) => todo!("infer:MethodCall"),
        }
    }

    fn infer_return(&mut self, span: Span, v: &ast::expr::Return<'src>) -> Expr<'src> {
        let ret_ty = match self.current_fn.as_ref().map(|ctx| ctx.ret_ty) {
            Some(ret_ty) => ret_ty,
            None => {
                self.ecx.emit_return_outside_fn(span);
                Ty::Error
            }
        };

        let value = v
            .value
            .as_ref()
            .map(|value| self.check_expr(value, ret_ty, NoDiscard));

        Expr {
            span,
            ty: Ty::Unreachable,
            kind: ExprKind::Return(Box::new(Return { value })),
        }
    }

    fn infer_break(&mut self, span: Span) -> Expr<'src> {
        Expr {
            span,
            ty: Ty::Unreachable,
            kind: ExprKind::Break(Break),
        }
    }

    fn infer_continue(&mut self, span: Span) -> Expr<'src> {
        Expr {
            span,
            ty: Ty::Unreachable,
            kind: ExprKind::Continue(Continue),
        }
    }

    fn infer_block_expr(&mut self, span: Span, v: &ast::Block<'src>) -> Expr<'src> {
        let block = self.infer_block(v);

        Expr {
            span,
            ty: block.ty(),
            kind: ExprKind::Block(Box::new(block)),
        }
    }

    fn infer_if(&mut self, span: Span, v: &ast::expr::If<'src>, intent: Intent) -> Expr<'src> {
        // TODO: branch/tail body type does not need to be the same
        // when if is in stmt position (therefore result is unused)

        if intent == NoDiscard && v.tail.is_none() {
            // TODO(syn): check that `if` has tail when used in expr context
            self.ecx.emit_missing_if_tail(span);
        }

        let mut branches = Vec::with_capacity(v.branches.len());
        let mut block_ty = None;
        for branch in &v.branches {
            let cond = self.check_expr(&branch.cond, Ty::BOOL, NoDiscard);
            let body = self.infer_block(&branch.body);

            if intent == NoDiscard {
                let block_ty = *block_ty.get_or_insert_with(|| body.ty());
                if !self.type_eq(block_ty, body.ty()) {
                    let span = body
                        .tail
                        .as_ref()
                        .map(|v| v.span)
                        .or_else(|| body.body.last().map(|v| v.span))
                        .unwrap_or(span);
                    self.ecx
                        .emit_type_mismatch(span, p!(self, block_ty), p!(self, body.ty()));
                    continue;
                }
            }

            branches.push(Branch { cond, body });
        }

        let block_ty = block_ty.unwrap_or(Ty::Unit);
        let tail = v.tail.as_ref().map(|tail| {
            let tail = self.infer_block(tail);
            if intent == NoDiscard && !self.type_eq(block_ty, tail.ty()) {
                self.ecx
                    .emit_type_mismatch(tail.span, p!(self, block_ty), p!(self, tail.ty()));
            }
            tail
        });

        Expr {
            span,
            ty: block_ty,
            kind: ExprKind::If(Box::new(If {
                if_token: v.if_token,
                branches,
                tail,
            })),
        }
    }

    fn infer_binary(&mut self, span: Span, v: &ast::expr::Binary<'src>) -> Expr<'src> {
        fn get_binop_ret_ty<'src>(
            tcx: &mut TyCtx<'src>,
            span: Span,
            lhs: &Expr<'src>,
            rhs: &Expr<'src>,
            op: ast::BinaryOp,
        ) -> Ty {
            if lhs.ty.is_err() || rhs.ty.is_err() {
                return Ty::Error;
            }

            if !tcx.type_supports_binop(lhs.ty, op) {
                tcx.ecx.emit_unsupported_op(span, p!(tcx, lhs.ty), op);
                return Ty::Error;
            }

            if !tcx.type_eq(lhs.ty, rhs.ty) {
                tcx.ecx
                    .emit_type_mismatch(span, p!(tcx, lhs.ty), p!(tcx, rhs.ty));
                return Ty::Error;
            }

            use ast::BinaryOp as Op;
            match op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Pow => lhs.ty,
                Op::Eq | Op::Ne | Op::Gt | Op::Lt | Op::Ge | Op::Le | Op::And | Op::Or => Ty::BOOL,
                Op::Opt => todo!("opt binop"),
            }
        }

        let lhs = self.infer_expr(&v.lhs, NoDiscard);
        let op = v.op;
        let rhs = self.infer_expr(&v.rhs, NoDiscard);
        let ty = get_binop_ret_ty(self, span, &lhs, &rhs, op);

        Expr {
            span,
            ty,
            kind: ExprKind::Binary(Box::new(Binary { lhs, op, rhs })),
        }
    }

    fn infer_unary(&mut self, span: Span, v: &ast::expr::Unary<'src>) -> Expr<'src> {
        fn get_unop_ret_ty<'src>(
            tcx: &mut TyCtx<'src>,
            span: Span,
            rhs: &Expr<'src>,
            op: ast::UnaryOp,
        ) -> Ty {
            if rhs.ty.is_err() {
                return Ty::Error;
            }

            if !tcx.type_supports_unop(rhs.ty, op) {
                tcx.ecx.emit_unsupported_op(span, p!(tcx, rhs.ty), op);
                return Ty::Error;
            }

            use ast::UnaryOp as Op;
            match op {
                Op::Minus => rhs.ty,
                Op::Not => rhs.ty,
                Op::Opt => todo!("opt unop"),
            }
        }

        let op = v.op;
        let rhs = self.infer_expr(&v.rhs, NoDiscard);
        let ty = get_unop_ret_ty(self, span, &rhs, op);

        Expr {
            span,
            ty,
            kind: ExprKind::Unary(Box::new(Unary { op, rhs })),
        }
    }

    fn infer_primitive(&mut self, span: Span, v: &ast::expr::Primitive<'src>) -> Expr<'src> {
        let (ty, prim) = match v {
            ast::expr::Primitive::Int(v) => (Ty::INT, Primitive::Int(*v)),
            ast::expr::Primitive::Num(v) => (Ty::NUM, Primitive::Num(*v)),
            ast::expr::Primitive::Bool(v) => (Ty::BOOL, Primitive::Bool(*v)),
            ast::expr::Primitive::Str(v) => (Ty::STR, Primitive::Str(v.clone())),
        };

        Expr {
            span,
            ty,
            kind: ExprKind::Primitive(Box::new(prim)),
        }
    }

    fn infer_var_expr(&mut self, span: Span, v: &ast::expr::UseVar<'src>) -> Expr<'src> {
        let ty = match self.resolve_var(v.name.as_str()) {
            Some(symbol) => symbol.ty,
            None => {
                self.ecx.emit_undefined_var(span);
                Ty::Error
            }
        };

        Expr {
            span,
            ty,
            kind: ExprKind::UseVar(Box::new(UseVar { name: v.name })),
        }
    }

    fn infer_var_assign(&mut self, span: Span, v: &ast::expr::AssignVar<'src>) -> Expr<'src> {
        let ty = match self.resolve_var(v.name.as_str()) {
            Some(symbol) => match symbol.kind {
                SymbolKind::Var => symbol.ty,
                _ => {
                    self.ecx.emit_invalid_assign_target(v.name.span);
                    Ty::Error
                }
            },
            None => {
                self.ecx.emit_undefined_var(span);
                Ty::Error
            }
        };

        let value = self.check_expr(&v.value, ty, NoDiscard);
        if let Some(op) = v.op {
            if !self.type_supports_binop(value.ty, op) {
                self.ecx
                    .emit_unsupported_op(value.span, p!(self, value.ty), op);
            }
        }

        Expr {
            span,
            ty: Ty::Unit,
            kind: ExprKind::AssignVar(Box::new(AssignVar {
                name: v.name,
                op: v.op,
                value,
            })),
        }
    }

    fn infer_call_expr(&mut self, span: Span, v: &ast::expr::Call<'src>) -> Expr<'src> {
        let callee = self.infer_expr(&v.callee, NoDiscard);
        let args = v
            .args
            .iter()
            .map(|arg| Arg {
                key: arg.key,
                value: self.infer_expr(&arg.value, NoDiscard),
            })
            .collect::<Vec<_>>();
        let ty = self.check_call(callee.span, callee.ty, &args);

        Expr {
            span,
            ty,
            kind: ExprKind::Call(Box::new(Call { callee, args })),
        }
    }

    fn check_call(&mut self, span: Span, callee: Ty, args: &[Arg<'src>]) -> Ty {
        match callee {
            Ty::Unit | Ty::Def(_) => {
                self.ecx.emit_not_callable(span, p!(self, callee));
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
                        self.ecx.emit_type_mismatch(
                            arg.value.span,
                            p!(self, arg_ty),
                            p!(self, param_ty),
                        );
                    }
                }

                fn_.sig.ret
            }
            Ty::Error => Ty::Error,
            Ty::Unreachable => Ty::Unreachable,
            Ty::Dynamic => unreachable!("ICE: `any` appears as callee"),
        }
    }

    fn type_eq(&self, a: Ty, b: Ty) -> bool {
        match (a, b) {
            (Ty::Unit, Ty::Unit) => true,
            (Ty::Def(a), Ty::Def(b)) => a == b,
            (Ty::Fn(a), Ty::Fn(b)) => self.fn_sig_match(&self.fns[a].sig, &self.fns[b].sig),
            (Ty::Dynamic | Ty::Error | Ty::Unreachable, _)
            | (_, Ty::Dynamic | Ty::Error | Ty::Unreachable) => true,
            _ => false,
        }
    }

    fn type_supports_binop(&self, ty: Ty, op: ast::BinaryOp) -> bool {
        use ast::BinaryOp as Op;
        if ty.is_err() {
            return true;
        }

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

    fn type_supports_unop(&self, ty: Ty, op: ast::UnaryOp) -> bool {
        use ast::UnaryOp as Op;
        if ty.is_err() {
            return true;
        }

        match op {
            Op::Minus => ty.is_int() || ty.is_num(),
            Op::Not => ty.is_bool(),
            Op::Opt => todo!("opt unop"),
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

macro_rules! primitives {
    ($prim:ident; $($name:ident),* $(,)?) => {
        paste::paste! {
            #[allow(non_camel_case_types)]
            #[repr(u32)]
            enum _Builtins {
                $($name),*
            }

            pub mod $prim {
                use super::*;
                $(
                    #[allow(non_upper_case_globals)]
                    pub const [<$name:camel>]: Prim = Prim {
                        name: stringify!($name),
                        id: DefId(_Builtins::$name as u32),
                    };
                )*
            }

            const _PRIMITIVES_LEN: usize = [$($prim::[<$name:camel>]),*].len();

            impl Prim {
                pub const LIST: [Prim; _PRIMITIVES_LEN] = [
                    $($prim::[<$name:camel>]),*
                ];
            }

            impl Ty {
                $(
                    pub const [<$name:upper>]: Ty = Ty::Def(DefId(_Builtins::$name as u32));
                )*

                $(
                    pub fn [<is_ $name:lower>](&self) -> bool {
                        match self {
                            Self::Def(id) => *id == $prim::[<$name:camel>].id,
                            _ => false,
                        }
                    }
                )*
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim {
    pub name: &'static str,
    pub id: DefId,
}

impl Prim {
    pub const fn ty(self) -> Ty {
        Ty::Def(self.id)
    }
}

primitives! {
    prim;
    int, num, bool, str
}

impl Ty {
    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Def(id) => {
                let [first, .., last] = Prim::LIST;
                *id >= first.id && *id <= last.id
            }
            _ => false,
        }
    }
}

fn register_primitive_types(tcx: &mut TyCtx<'_>) {
    assert!(
        tcx.defs.is_empty(),
        "`register_primitive_types` called with some type defs already present"
    );

    for Prim { name, id } in Prim::LIST.iter().copied() {
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum Intent {
    NoDiscard,
    Discard,
}
use Intent::*;

#[derive(Clone, Copy)]
pub struct TyPrinter<'a, 'src> {
    pub(crate) defs: &'a Defs<'src>,
    pub(crate) fns: &'a Fns<'src>,
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
            Ty::Dynamic => f.write_str("any"),
            Ty::Error => f.write_str("{error}"),
            Ty::Unreachable => f.write_str("!"),
        }
    }
}

#[cfg(test)]
mod tests;
