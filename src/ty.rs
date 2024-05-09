#[macro_use]
mod macros;

use std::collections::BTreeMap;

use crate::ast::{self, Ast, Ident};
use crate::error::{Error, ErrorCtx, Result};
use crate::hir::*;
use crate::lex::Span;
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

pub fn check<'src>(ast: &Ast<'src>) -> Result<Hir<'src>, Vec<Error>> {
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
            src,
            ecx: ErrorCtx::new(src),
            defs: Defs::new(),
            fns: Fns::new(),
            scopes: Vec::new(),
        }
    }

    fn finish(mut self, top_level: Block<'src>) -> Result<Hir<'src>, Vec<Error>> {
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
                            name: field.name,
                            ty: field.ty,
                        })
                        .collect(),
                    ret: Ty::Def(def_id),
                    ret_span: None,
                },
            ),
        };

        let id = self.fns.insert(Fn {
            name,
            kind,
            sig,
            body: FnBody::Extern, // compiler-defined
        });
        self.declare_symbol(name, Ty::Fn(id), SymbolKind::Fn);
    }

    fn infer_fns(&mut self, decls: &[ast::Decl<'src>]) {
        let mut fns = Vec::<(FnId, Ident<'_>, FnSig<'_>, &ast::decl::Body<'_>)>::new();
        let mut seen = HashSet::<Ident<'src>>::default();
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

            let sig = FnSig {
                params: fn_
                    .params
                    .iter()
                    .map(|param| Param {
                        name: param.name,
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

            fns.push((FnId(u32::MAX), name, sig, &fn_.body));
        }

        for (id, name, sig, _) in &mut fns {
            *id = self.fns.insert(Fn {
                name: *name,
                kind: FnKind::Function,
                sig: sig.clone(),
                body: FnBody::Extern,
            });
            self.declare_symbol(*name, Ty::Fn(*id), SymbolKind::Fn);
        }

        for (id, name, sig, body) in fns {
            let body = self.infer_fn_body(&name, &sig, body);
            let fn_ = self.fns.get_by_id_mut(id).unwrap();
            fn_.body = body;
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn leave_scope(&mut self) {
        assert!(self.scopes.pop().is_some());
    }

    fn current_scope(&mut self) -> &mut Scope<'src> {
        self.scopes.last_mut().expect("BUG: no open scope")
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

    fn resolve_var_ty(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name).copied() {
                return Some(symbol.ty);
            }
        }

        None
    }

    fn resolve_field(&mut self, field: &ast::decl::Field<'src>) -> (Ident<'src>, Ty) {
        let name = field.name;
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

                let ret_ty = block.ty();

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

    fn infer_block(&mut self, block: &ast::Block<'src>) -> Block<'src> {
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

    fn infer_stmt(&mut self, stmt: &ast::Stmt<'src>) -> Stmt<'src> {
        match &stmt.kind {
            ast::stmt::StmtKind::Let(v) => self.infer_let(stmt.span, v),
            ast::stmt::StmtKind::Loop(v) => self.infer_loop(stmt.span, v),
            ast::stmt::StmtKind::Expr(v) => self.infer_expr(v).into_stmt(),
        }
    }

    fn infer_let(&mut self, span: Span, v: &ast::stmt::Let<'src>) -> Stmt<'src> {
        let init = match v.ty.as_ref().map(|ty| self.resolve_ast_ty(ty)) {
            Some(ty) => self.check_expr(&v.init, ty),
            None => self.infer_expr(&v.init),
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

    fn check_expr(&mut self, expr: &ast::Expr<'src>, ty: Ty) -> Expr<'src> {
        let mut expr = self.infer_expr(expr);
        if !self.type_eq(expr.ty, ty) {
            let p = ty_p!(self);
            self.ecx.emit_type_mismatch(expr.span, p(expr.ty), p(ty));
            expr.ty = Ty::Error;
        }
        expr
    }

    fn infer_expr(&mut self, expr: &ast::Expr<'src>) -> Expr<'src> {
        use ast::expr::ExprKind as E;
        match &expr.kind {
            E::Return(_) => todo!("infer:Return"),
            E::Break => self.infer_break(expr.span),
            E::Continue => self.infer_continue(expr.span),
            E::Block(_) => todo!("infer:Block"),
            E::If(v) => self.infer_if(expr.span, v),
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

    fn infer_break(&mut self, span: Span) -> Expr<'src> {
        Expr {
            span,
            ty: Ty::Unit,
            kind: ExprKind::Break(Break),
        }
    }

    fn infer_continue(&mut self, span: Span) -> Expr<'src> {
        Expr {
            span,
            ty: Ty::Unit,
            kind: ExprKind::Continue(Continue),
        }
    }

    fn infer_if(&mut self, span: Span, v: &ast::expr::If<'src>) -> Expr<'src> {
        // TODO: branch/tail body type does not need to be the same
        // when if is in stmt position (therefore result is unused)

        let mut branches = Vec::with_capacity(v.branches.len());
        let mut block_ty = None;
        for branch in &v.branches {
            let cond = self.check_expr(&branch.cond, Bool.ty());
            let body = self.infer_block(&branch.body);

            if !v.is_stmt {
                let block_ty = *block_ty.get_or_insert_with(|| body.ty());
                if !self.type_eq(block_ty, body.ty()) {
                    let span = body
                        .tail
                        .as_ref()
                        .map(|v| v.span)
                        .or_else(|| body.body.last().map(|v| v.span))
                        .unwrap_or(span);
                    let p = ty_p!(self);
                    self.ecx.emit_type_mismatch(span, p(block_ty), p(body.ty()));
                    continue;
                }
            }

            branches.push(Branch { cond, body });
        }

        let block_ty = block_ty.unwrap_or(Ty::Unit);
        let tail = v.tail.as_ref().map(|tail| {
            let tail = self.infer_block(tail);
            if !v.is_stmt && !self.type_eq(block_ty, tail.ty()) {
                let p = ty_p!(self);
                self.ecx
                    .emit_type_mismatch(tail.span, p(block_ty), p(tail.ty()));
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
                Op::Eq | Op::Ne | Op::Gt | Op::Lt | Op::Ge | Op::Le | Op::And | Op::Or => Bool.ty(),
                Op::Opt => todo!("opt binop"),
            }
        }

        let lhs = self.infer_expr(&v.lhs);
        let op = v.op;
        let rhs = self.infer_expr(&v.rhs);
        let ty = get_binop_ret_ty(self, span, &lhs, &rhs, op);

        Expr {
            span,
            ty,
            kind: ExprKind::Binary(Box::new(Binary { lhs, op, rhs })),
        }
    }

    fn infer_primitive(&mut self, span: Span, v: &ast::expr::Primitive<'src>) -> Expr<'src> {
        let (ty, prim) = match v {
            ast::expr::Primitive::Int(v) => (Int.ty(), Primitive::Int(*v)),
            ast::expr::Primitive::Num(v) => (Num.ty(), Primitive::Num(*v)),
            ast::expr::Primitive::Bool(v) => (Bool.ty(), Primitive::Bool(*v)),
            ast::expr::Primitive::Str(v) => (Str.ty(), Primitive::Str(v.clone())),
        };

        Expr {
            span,
            ty,
            kind: ExprKind::Primitive(Box::new(prim)),
        }
    }

    fn infer_var_expr(&mut self, span: Span, v: &ast::expr::UseVar<'src>) -> Expr<'src> {
        let ty = match self.resolve_var_ty(v.name.as_str()) {
            Some(ty) => ty,
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

    fn infer_call_expr(&mut self, span: Span, v: &ast::expr::Call<'src>) -> Expr<'src> {
        let callee = self.infer_expr(&v.callee);
        let args = v
            .args
            .iter()
            .map(|arg| Arg {
                key: arg.key,
                value: self.infer_expr(&arg.value),
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

macro_rules! primitives {
    ($($name:ident),* $(,)?) => {
        paste::paste! {
            #[allow(non_camel_case_types)]
            #[repr(u32)]
            enum _Builtins {
                $($name),*
            }

            $(
                #[allow(non_upper_case_globals)]
                pub const [<$name:camel>]: Prim = Prim {
                    name: stringify!($name),
                    id: DefId(_Builtins::$name as u32),
                };
            )*

            const _PRIMITIVES_LEN: usize = [$([<$name:camel>]),*].len();

            impl Prim {
                pub const LIST: [Prim; _PRIMITIVES_LEN] = [
                    $([<$name:camel>]),*
                ];
            }

            impl Ty {
                $(
                    pub fn [<is_ $name:lower>](&self) -> bool {
                        match self {
                            Self::Def(id) => *id == [<$name:camel>].id,
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
    pub fn ty(self) -> Ty {
        Ty::Def(self.id)
    }
}

primitives! {
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
        "`register_builtin_types` called with some type defs already present"
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
