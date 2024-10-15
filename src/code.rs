use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::sync::Arc;

use crate::ast::{BinaryOp, Ident, UnaryOp};
use crate::error::{Error, ErrorCtx, Report, Result};
use crate::hir::Hir;
use crate::lex::Span;
use crate::util::{default, JoinIter};
use crate::vm2::value::{f64n, Literal, LiteralPool};
use crate::{hir, vm2, HashMap, Str};

pub mod op;
use op::*;

use self::asm::Callee;

pub fn compile(hir: Hir<'_>) -> Result<CodeUnit, Report> {
    Compiler {
        ecx: ErrorCtx::new(hir.src),
        module_state: ModuleState {
            vars: default(),
            fn_table: FnTableBuilder::new(),
        },
    }
    .compile(hir)
}

struct Compiler<'src> {
    ecx: ErrorCtx<'src>,
    module_state: ModuleState<'src>,
}

impl<'src> Compiler<'src> {
    fn compile(mut self, hir: Hir<'src>) -> Result<CodeUnit, Report> {
        // 1. reserve a slot in the function table for each function
        let mut pending_fns = Vec::<(FnId, &hir::Fn)>::with_capacity(hir.fns.len());
        for (_, fn_) in hir.fns.iter() {
            if fn_.is_extern_fn() {
                // for host functions, we just reserve them
                // they are "filled in" later by `CodeUnit::link`
                self.module_state.fn_table.reserve_host(
                    fn_.name.to_string(),
                    ExternFunctionSig {
                        params: fn_.sig.params.iter().map(|p| p.ty).collect(),
                        ret: fn_.sig.ret,
                    },
                )
            } else {
                // we need to codegen each script function
                pending_fns.push((self.module_state.fn_table.reserve_script(fn_.name), fn_));
            }
        }

        // 2. compile top-level code
        let top_level = generate_main_fn(hir.top_level);
        match function(&mut self, &top_level) {
            Ok(fn_) => self.module_state.fn_table.set_main(fn_),
            Err(e) => self.ecx.push(e),
        }

        // 3. compile all functions, place them into their respective slots
        for (id, fn_) in pending_fns {
            match function(&mut self, fn_) {
                Ok(fn_) => self.module_state.fn_table.set_script(id, fn_),
                Err(e) => self.ecx.push(e),
            }
        }

        let function_table = self.module_state.fn_table.finish();

        self.ecx.finish()?;

        Ok(CodeUnit {
            functions: Box::new(function_table),
        })
    }
}

struct ModuleState<'src> {
    vars: HashMap<Ident<'src>, MVar>,
    fn_table: FnTableBuilder,
}

struct FunctionState<'src, 'a> {
    compiler: &'a mut Compiler<'src>,
    hir: &'a hir::Fn<'src>,
    current_loop: Option<Loop>,
    asm: Assembler<'src>,
    regalloc: RegisterAllocator,
    locals: Vec<HashMap<Ident<'src>, TyReg>>,
}

fn generate_main_fn(block: hir::Block<'_>) -> hir::Fn<'_> {
    hir::Fn {
        name: Ident::raw("main"),
        kind: hir::FnKind::Function,
        sig: hir::FnSig {
            params: vec![],
            ret: block.ty(),
            ret_span: block.tail.as_ref().map(|tail| tail.span),
        },
        body: hir::FnBody::Block(block),
    }
}

fn function<'src, 'a>(
    compiler: &'a mut Compiler<'src>,
    hir: &'a hir::Fn<'src>,
) -> Result<Function> {
    FunctionState {
        compiler,
        hir,
        current_loop: None,
        asm: Assembler::new(),
        regalloc: RegisterAllocator::new(),
        locals: Vec::new(),
    }
    .compile()
}

impl<'src, 'a> FunctionState<'src, 'a> {
    fn compile(mut self) -> Result<Function> {
        let hir::FnBody::Block(body) = &self.hir.body else {
            return Ok(default());
        };

        self.scope(|f| -> Result<()> {
            f.with_reg(Span::empty(), None, |f, ret_r| {
                for param in &self.hir.sig.params {
                    let reg = f.alloc_reg(param.name.span)?;
                    f.declare_local(param.name, TyReg::new(param.ty, reg));
                }

                block_expr(f, body, Some(ret_r))?;

                use asm::*;
                f.asm.emit(Span::empty(), ret());

                Ok(())
            })
        })?;

        let (bytecode, spans, literals) = self.asm.finish();
        let registers = self.regalloc.total;
        Ok(Function {
            name: self.hir.name.to_string(),
            params: self.hir.sig.params.len() as u8,
            bytecode,
            spans,
            literals,
            registers,
        })
    }

    fn resolve_symbol(&self, name: Ident<'src>) -> Symbol {
        // resolution order:
        // 1. local variable
        // 2. module variable
        // 3. function
        // NOTE: all symbols have already been resolved during type checking
        // if we're generating code, then all symbol usages are guaranteed
        // to be valid.

        if let Some(reg) = self.resolve_local(&name) {
            return Symbol::Var(reg);
        }

        if let Some(idx) = self.resolve_module_var(&name) {
            return Symbol::Mvar(idx);
        }

        if let Some(id) = self.resolve_script_function(&name) {
            return Symbol::Fn(id);
        }

        if let Some(id) = self.resolve_host_function(&name) {
            return Symbol::Host(id);
        }

        unreachable!("BUG: unresolved variable: {name}@{}", name.span);
    }

    fn resolve_local(&self, name: &str) -> Option<TyReg> {
        for scope in self.locals.iter().rev() {
            if let Some(reg) = scope.get(name).copied() {
                return Some(reg);
            }
        }
        None
    }

    fn resolve_module_var(&self, name: &str) -> Option<MVar> {
        self.compiler.module_state.vars.get(name).copied()
    }

    fn resolve_script_function(&self, name: &str) -> Option<FnId> {
        self.compiler.module_state.fn_table.get_script_id(name)
    }

    fn resolve_host_function(&self, name: &str) -> Option<HostId> {
        self.compiler.module_state.fn_table.get_host_id(name)
    }

    fn resolve_local_in_current_scope(&self, name: &str) -> Option<TyReg> {
        self.locals
            .last()
            .and_then(|scope| scope.get(name))
            .copied()
    }

    fn declare_local(&mut self, name: Ident<'src>, reg: TyReg) {
        let scope = self.locals.last_mut().expect("BUG: no open scope");
        let _ = scope.insert(name, reg);
    }

    fn alloc_reg(&mut self, span: Span) -> Result<Reg> {
        match self.regalloc.alloc() {
            Some(reg) => Ok(reg),
            None => Err(self.compiler.ecx.too_many_registers(span)),
        }
    }

    fn free_reg(&mut self, reg: Reg) {
        self.regalloc.free(reg);
    }

    fn lit_i64(&mut self, span: Span, v: i64) -> Result<Lit> {
        self.asm
            .literal_pool
            .insert_int(v)
            .map(Lit)
            .ok_or_else(|| self.compiler.ecx.too_many_literals(span))
    }

    fn lit_f64(&mut self, span: Span, v: f64n) -> Result<Lit> {
        self.asm
            .literal_pool
            .insert_num(v)
            .map(Lit)
            .ok_or_else(|| self.compiler.ecx.too_many_literals(span))
    }

    fn lit_str(&mut self, span: Span, v: Str<'src>) -> Result<Lit> {
        self.asm
            .literal_pool
            .insert_str(v)
            .map(Lit)
            .ok_or_else(|| self.compiler.ecx.too_many_literals(span))
    }

    fn lit_jump_offset(&mut self, span: Span, v: isize) -> Result<Lit> {
        self.asm
            .literal_pool
            .insert_jump_offset(v)
            .map(Lit)
            .ok_or_else(|| self.compiler.ecx.too_many_literals(span))
    }

    fn scope<T>(&mut self, inner: impl FnOnce(&mut Self) -> T) -> T {
        self.locals.push(default());
        let r = inner(self);
        self.locals.pop().unwrap();
        r
    }

    #[inline]
    fn with_reg<T>(
        &mut self,
        span: Span,
        reg: Option<Reg>,
        inner: impl FnOnce(&mut Self, Reg) -> Result<T>,
    ) -> Result<T> {
        match reg {
            Some(reg) => inner(self, reg),
            None => {
                let reg = self.alloc_reg(span)?;
                let result = inner(self, reg);
                self.free_reg(reg);
                result
            }
        }
    }

    #[inline]
    fn in_loop<T>(&mut self, inner: impl FnOnce(&mut Self, Option<&Loop>) -> T) -> T {
        let state = self.current_loop.take();
        let result = inner(self, state.as_ref());
        self.current_loop = state;
        result
    }
}

fn stmt<'src, 'a>(f: &mut FunctionState<'src, 'a>, hir: &'a hir::Stmt<'src>) -> Result<()> {
    match &hir.kind {
        hir::StmtKind::Let(hir) => let_stmt(f, hir),
        hir::StmtKind::Loop(hir) => loop_stmt(f, hir),
        hir::StmtKind::Expr(hir) => expr_stmt(f, hir),
    }
}

fn let_stmt<'src, 'a>(f: &mut FunctionState<'src, 'a>, hir: &'a hir::Let<'src>) -> Result<()> {
    let dst = match f.resolve_local_in_current_scope(&hir.name) {
        Some(reg) => reg,
        None => f.alloc_reg(hir.name.span)?.ty(hir.init.ty),
    };
    assign_to(f, &hir.init, dst.reg)?;
    f.declare_local(hir.name, dst);
    Ok(())
}

fn loop_stmt<'src, 'a>(f: &mut FunctionState<'src, 'a>, hir: &'a hir::Loop<'src>) -> Result<()> {
    let state = Loop {
        entry: BackwardJumpLabel::bind(f)?,
        exit: ForwardJumpLabel::new(),
    };
    let state = loop_body(f, state, |f| {
        block_expr(f, &hir.body, None)?;
        Ok(())
    })?;

    state
        .entry
        .emit_jmp(f, (hir.body.span.end..hir.body.span.end + 1).into())?;
    state.exit.bind(f)?;

    Ok(())
}

fn loop_body<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    in_: Loop,
    body: impl FnOnce(&mut FunctionState<'src, 'a>) -> Result<()>,
) -> Result<Loop> {
    let prev_state = std::mem::replace(&mut f.current_loop, Some(in_));
    let result = body(f);
    let in_ = std::mem::replace(&mut f.current_loop, prev_state).unwrap();
    result?;
    Ok(in_)
}

fn expr_stmt<'src, 'a>(f: &mut FunctionState<'src, 'a>, hir: &'a hir::Expr<'src>) -> Result<()> {
    // discard the output
    let _ = expr(f, hir, None)?;
    Ok(())
}

fn expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    hir: &'a hir::Expr<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    let span = hir.span;
    match &hir.kind {
        hir::ExprKind::Return(hir) => return_expr(f, span, hir).map(|_| None),
        hir::ExprKind::Break(_) => break_expr(f, span).map(|_| None),
        hir::ExprKind::Continue(_) => continue_expr(f, span).map(|_| None),
        hir::ExprKind::Block(hir) => block_expr(f, hir, dst).map(|_| None),
        hir::ExprKind::If(hir) => if_expr(f, hir, dst).map(|_| None),
        hir::ExprKind::Binary(hir) => binary_expr(f, span, hir, dst),
        hir::ExprKind::Unary(hir) => unary_expr(f, span, hir, dst),
        hir::ExprKind::Primitive(hir) => primitive_expr(f, span, hir, dst),
        hir::ExprKind::UseVar(hir) => use_var_expr(f, span, hir, dst),
        hir::ExprKind::UseField(_hir) => todo!(),
        hir::ExprKind::UseIndex(_hir) => todo!(),
        hir::ExprKind::AssignVar(hir) => assign_var_expr(f, span, hir).map(|_| None),
        hir::ExprKind::AssignField(_hir) => todo!(),
        hir::ExprKind::AssignIndex(_hir) => todo!(),
        hir::ExprKind::Call(hir) => call_expr(f, span, hir, dst),
    }
}

fn return_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Return<'src>,
) -> Result<()> {
    todo!()
}

fn break_expr(f: &mut FunctionState, span: Span) -> Result<()> {
    // emit a `jmp` to the current loop exit
    f.in_loop(|f, state| match state {
        Some(state) => {
            state.exit.emit_jmp(f, span);
            Ok(())
        }
        None => Err(f.compiler.ecx.continue_outside_loop(span)),
    })
}

fn continue_expr(f: &mut FunctionState, span: Span) -> Result<()> {
    // emit a `jmp` to the current loop entry
    f.in_loop(|f, state| match state {
        Some(state) => state.entry.emit_jmp(f, span),
        None => Err(f.compiler.ecx.continue_outside_loop(span)),
    })
}

fn binary_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Binary<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    f.with_reg(span, dst, |f, dst| {
        let lhs = expr(f, &hir.lhs, Some(dst))?.unwrap_or(dst).ty(hir.lhs.ty);
        let fresh_rhs = f.alloc_reg(span)?;
        let rhs = expr(f, &hir.rhs, Some(fresh_rhs))?
            .unwrap_or(fresh_rhs)
            .ty(hir.rhs.ty);

        emit_binop(f, span, lhs, rhs, hir.op, dst);

        f.free_reg(fresh_rhs);

        Ok(None)
    })
}

fn unary_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Unary<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    f.with_reg(span, dst, |f, dst| {
        let rhs = expr(f, &hir.rhs, Some(dst))?.unwrap_or(dst).ty(hir.rhs.ty);

        emit_unop(f, span, rhs, hir.op, dst);

        Ok(None)
    })
}

#[derive(Debug, Clone, Copy)]
struct TyReg {
    ty: hir::Ty,
    reg: Reg,
}

impl Reg {
    fn ty(self, ty: hir::Ty) -> TyReg {
        TyReg::new(ty, self)
    }
}

impl TyReg {
    fn new(ty: hir::Ty, reg: Reg) -> Self {
        Self { ty, reg }
    }
}

fn emit_binop(f: &mut FunctionState, span: Span, lhs: TyReg, rhs: TyReg, op: BinaryOp, dst: Reg) {
    use asm::*;
    use BinaryOp as O;
    let op = match op {
        O::Add => add,
        O::Sub => sub,
        O::Mul => mul,
        O::Div => div,
        O::Rem => rem,
        // O::Pow => Pow,
        O::Eq => ceq,
        O::Ne => cne,
        O::Gt => cgt,
        O::Lt => clt,
        O::Ge => cge,
        O::Le => cle,
        _ => todo!(),
    };

    let ty = if lhs.ty.is_int() {
        I64
    } else if lhs.ty.is_num() {
        F64
    } else {
        unreachable!("BUG: binary expr consisting of non-numeric types at {span}");
    };

    f.asm.emit(span, op(ty, lhs.reg, rhs.reg, dst));
}

fn emit_unop(f: &mut FunctionState, span: Span, rhs: TyReg, op: UnaryOp, dst: Reg) {
    use asm::*;
    use UnaryOp as O;
    match op {
        O::Minus => {
            let ty = if rhs.ty.is_int() {
                I64
            } else if rhs.ty.is_num() {
                F64
            } else {
                unreachable!("BUG: unary minus with non-numeric type at {span}")
            };
            f.asm.emit(span, mns(ty, rhs.reg, dst));
        }
        O::Not => f.asm.emit(span, not(rhs.reg, dst)),
        O::Opt => todo!(),
    }
}

fn primitive_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Primitive<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    use asm::*;

    let Some(dst) = dst else { return Ok(None) };

    match hir {
        hir::Primitive::Int(v) => match i16::try_from(*v) {
            Ok(v) => f.asm.emit(span, load_i16(v, dst)),
            Err(_) => {
                let idx = f.lit_i64(span, *v)?;
                f.asm.emit(span, load_cst(idx, dst));
            }
        },
        hir::Primitive::Num(v) => {
            let idx = f.lit_f64(span, *v)?;
            f.asm.emit(span, load_cst(idx, dst));
        }
        hir::Primitive::Bool(v) => {
            f.asm.emit(span, load_bool(*v, dst));
        }
        hir::Primitive::Str(v) => {
            let idx = f.lit_str(span, v.clone())?;
            f.asm.emit(span, load_cst(idx, dst));
        }
    }

    Ok(None)
}

fn use_var_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::UseVar<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    let symbol = f.resolve_symbol(hir.name);

    use asm::*;
    let ret = match symbol {
        Symbol::Host(id) => {
            let Some(dst) = dst else { return Ok(None) };
            f.asm.emit(span, load_fn_host(id, dst));
            None
        }
        Symbol::Fn(id) => {
            let Some(dst) = dst else { return Ok(None) };
            f.asm.emit(span, load_fn(id, dst));
            None
        }
        Symbol::Mvar(_v) => todo!("module variables"),
        Symbol::Var(v) => Some(v.reg),
    };

    Ok(ret)
}

fn assign_var_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::AssignVar<'src>,
) -> Result<()> {
    let symbol = f.resolve_symbol(hir.name);

    let Symbol::Var(dst) = symbol else {
        unreachable!();
    };

    match hir.op {
        Some(op) => {
            let lhs = dst;
            let fresh_rhs = f.alloc_reg(span)?;
            let rhs = expr(f, &hir.value, Some(fresh_rhs))?
                .unwrap_or(fresh_rhs)
                .ty(hir.value.ty);

            emit_binop(f, hir.name.span.to(hir.value.span), lhs, rhs, op, dst.reg);

            f.free_reg(fresh_rhs);
        }
        None => {
            assign_to(f, &hir.value, dst.reg)?;
        }
    }

    Ok(())
}

fn call_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Call<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    'value_call: {
        if let hir::ExprKind::UseVar(var) = &hir.callee.kind {
            //   fn f() {}
            //   f(); // calling function directly
            let callee = match f.resolve_symbol(var.name) {
                Symbol::Fn(id) => Callee::Script(id),
                Symbol::Host(id) => Callee::Host(id),
                _ => break 'value_call,
            };
            return call_expr_direct(f, span, hir, callee, dst);
        }
    }

    //   fn f() {}
    //   let g = f;
    //   g(); // calling function in variable
    call_expr_value(f, span, hir, dst)
}

fn call_expr_direct<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Call<'src>,
    callee: Callee,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    // caller: [.., ret, a, b, c]
    // callee:     [ret, a, b, c, ..]

    f.with_reg(span, dst, |f, dst| {
        let is_dst_flat = dst.get() + 1 == f.regalloc.current;
        let ret = match is_dst_flat {
            true => dst,
            false => f.alloc_reg(span)?, // must emit move
        };

        let mut args = Vec::with_capacity(hir.args.len());
        for arg in &hir.args {
            args.push(f.alloc_reg(arg.value.span)?);
        }

        for (arg, reg) in hir.args.iter().zip(args.iter()) {
            assign_to(f, &arg.value, *reg)?;
        }

        use asm::*;
        f.asm.emit(span, call_id(callee, ret));

        if !is_dst_flat {
            f.asm.emit(span, mov(ret, dst));
            f.free_reg(ret); // implicitly frees arg regs
        } else if let Some(arg0) = args.first() {
            f.free_reg(*arg0);
        }

        Ok(None)
    })
}

fn call_expr_value<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Call<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    // caller: [.., fn, a, b, c]
    // callee:     [ret, a, b, c, ..]

    f.with_reg(span, dst, |f, dst| {
        let is_dst_flat = dst.get() + 1 == f.regalloc.current;
        let fn_ = match is_dst_flat {
            true => dst,
            false => f.alloc_reg(span)?, // must emit move
        };

        let mut args = Vec::with_capacity(hir.args.len());
        for arg in &hir.args {
            args.push(f.alloc_reg(arg.value.span)?);
        }

        assign_to(f, &hir.callee, fn_)?;
        for (arg, reg) in hir.args.iter().zip(args.iter()) {
            assign_to(f, &arg.value, *reg)?;
        }

        use asm::*;
        f.asm.emit(span, call_reg(fn_));

        let ret = fn_;
        if !is_dst_flat {
            f.asm.emit(span, mov(ret, dst));
            f.free_reg(ret); // implicitly frees arg regs
        } else if let Some(arg0) = args.first() {
            f.free_reg(*arg0);
        }

        Ok(None)
    })
}

/// emit a block expression
///
/// this emits `mov` as needed, no need to use `assign_to`
fn block_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    hir: &'a hir::Block<'src>,
    dst: Option<Reg>,
) -> Result<()> {
    f.scope(|f| {
        for v in &hir.body {
            stmt(f, v)?;
        }

        match (&hir.tail, dst) {
            (Some(tail), Some(dst)) => {
                assign_to(f, tail, dst)?;
            }
            (None, Some(dst)) => {
                f.asm.emit(Span::empty(), asm::load_unit(dst));
            }
            (Some(tail), None) => {
                expr(f, tail, None)?;
            }
            (None, None) => {}
        }

        Ok(())
    })
}

fn if_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    hir: &'a hir::If<'src>,
    dst: Option<Reg>,
) -> Result<()> {
    f.with_reg(hir.if_token, dst, |f, dst| {
        let exit = ForwardJumpLabel::new();

        let mut branches = hir.branches.iter().peekable();
        while let Some(branch) = branches.next() {
            let next = SingleJumpLabel::new();

            assign_to(f, &branch.cond, dst)?;
            next.emit_jmpf(f, branch.cond.span, dst);

            block_expr(f, &branch.body, Some(dst))?;
            if hir.tail.is_some() || branches.peek().is_some() {
                exit.emit_jmp(f, branch.cond.span);
            }

            next.bind(f)?;
        }

        if let Some(tail) = &hir.tail {
            block_expr(f, tail, Some(dst))?
        }

        exit.bind(f)?;

        Ok(())
    })
}

/// if `src` is Some and not `dst`, then this emits a `mov src, dst`
///
/// this exists to support moving out of variables
fn maybe_move(f: &mut FunctionState, src: Option<Reg>, dst: Reg, span: Span) -> Result<()> {
    use asm::*;

    if let Some(src) = src {
        // `expr` was written to `out`
        if src != dst {
            // `out` and `dst` are different registers
            f.asm.emit(span, mov(src, dst));
        }
    }

    Ok(())
}

/// emit `value` and ensure the result is placed in `dst`
///
/// if the emit of `value` yields a register other than `dst`,
/// then this also emits a `mov value, dst`
///
/// this exists to support moving out of variables
fn assign_to<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    value: &'a hir::Expr<'src>,
    dst: Reg,
) -> Result<()> {
    let out = expr(f, value, Some(dst))?;
    maybe_move(f, out, dst, value.span)
}

struct Loop {
    entry: BackwardJumpLabel,
    exit: ForwardJumpLabel,
}

#[derive(Default)]
struct Assembler<'src> {
    bytecode: Vec<Op>,
    spans: Vec<Span>,
    literal_pool: LiteralPool<'src>,
}

impl<'src> Assembler<'src> {
    fn new() -> Self {
        default()
    }

    fn emit(&mut self, span: Span, op: Op) {
        self.bytecode.push(op);
        self.spans.push(span);
    }

    fn patch_jump(&mut self, referrer: usize, offset: Offset) {
        use asm::*;

        let op = self.bytecode[referrer];
        let patched = match op {
            Op::Jump { .. } => jmp(offset),
            Op::JumpIfFalse { cond, .. } => jmpf(cond, offset),
            _ => {
                unreachable!("cannot patch {op:?}@{referrer} as a jump");
            }
        };
        self.bytecode[referrer] = patched;
    }

    fn finish(self) -> (Vec<Op>, Vec<Span>, Vec<Literal>) {
        (self.bytecode, self.spans, self.literal_pool.finish())
    }
}

struct BackwardJumpLabel {
    /// Position of the block entry.
    ///
    /// Used as the target of backward jumps, such as `jmp`.
    pos: isize,
}

impl BackwardJumpLabel {
    fn bind(f: &mut FunctionState) -> Result<Self> {
        Ok(Self {
            pos: f.asm.bytecode.len() as isize,
        })
    }

    /// calculates distance to this label and emits a `jmp` into `f`
    /// using the distance as the offset
    fn emit_jmp(&self, f: &mut FunctionState, span: Span) -> Result<()> {
        let from = f.asm.bytecode.len() as isize;
        let to = self.pos;
        let offset = Offset::get(f, from, to)?;
        f.asm.emit(span, asm::jmp(offset));
        Ok(())
    }

    /// calculates distance to this label and emits a `jmpf` into `f`
    /// using the distance as the offset
    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: Reg) -> Result<()> {
        let from = f.asm.bytecode.len() as isize;
        let to = self.pos;
        let offset = Offset::get(f, from, to)?;
        f.asm.emit(span, asm::jmpf(cond, offset));
        Ok(())
    }
}

struct ForwardJumpLabel {
    /// List of positions of jumps to this exit.
    ///
    /// Used as the target of forward jumps, such as `jmp` and `jmpf`.
    referrers: RefCell<Vec<usize>>,
}

impl ForwardJumpLabel {
    fn new() -> Self {
        Self {
            referrers: RefCell::new(Vec::new()),
        }
    }

    fn bind(self, f: &mut FunctionState) -> Result<()> {
        let to = f.asm.bytecode.len() as isize;
        for referrer in self.referrers.borrow().iter() {
            let from = *referrer as isize;
            let offset = Offset::get(f, from, to)?;
            f.asm.patch_jump(*referrer, offset);
        }

        Ok(())
    }

    /// emits a `jmp` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmp(&self, f: &mut FunctionState, span: Span) {
        let referrer = f.asm.bytecode.len();
        self.referrers.borrow_mut().push(referrer);
        f.asm.emit(span, asm::jmp(Offset::placeholder()));
    }

    /// emits a `jmpf` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: Reg) {
        let referrer = f.asm.bytecode.len();
        self.referrers.borrow_mut().push(referrer);
        f.asm.emit(span, asm::jmpf(cond, Offset::placeholder()));
    }
}

struct SingleJumpLabel {
    referrer: Cell<Option<usize>>,
}

impl SingleJumpLabel {
    fn new() -> Self {
        Self {
            referrer: Cell::new(None),
        }
    }

    fn bind(self, f: &mut FunctionState) -> Result<()> {
        let to = f.asm.bytecode.len() as isize;
        if let Some(referrer) = self.referrer.get() {
            let from = referrer as isize;
            let offset = Offset::get(f, from, to)?;
            f.asm.patch_jump(referrer, offset);
        }

        Ok(())
    }

    /// emits a `jmp` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmp(&self, f: &mut FunctionState, span: Span) {
        let referrer = f.asm.bytecode.len();
        self.referrer.set(Some(referrer));
        f.asm.emit(span, asm::jmp(Offset::placeholder()));
    }

    /// emits a `jmpf` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: Reg) {
        let referrer = f.asm.bytecode.len();
        self.referrer.set(Some(referrer));
        f.asm.emit(span, asm::jmpf(cond, Offset::placeholder()));
    }
}

impl Offset {
    fn get(f: &mut FunctionState, to: isize, from: isize) -> Result<Self> {
        let offset = from - to;
        let offset = if let Ok(offset) = i16::try_from(offset) {
            Offset::Rel(Rel(offset))
        } else {
            Offset::Cst(f.lit_jump_offset(Span::empty(), offset)?)
        };
        Ok(offset)
    }
}

struct RegisterAllocator {
    current: u8,
    total: u8,
}

impl RegisterAllocator {
    fn new() -> Self {
        Self {
            current: 0,
            total: 0,
        }
    }

    fn alloc(&mut self) -> Option<Reg> {
        if self.current == u8::MAX {
            return None;
        }

        let reg = self.current;
        self.current += 1;
        self.total = std::cmp::max(self.current, self.total);
        Some(Reg(reg))
    }

    fn free(&mut self, reg: Reg) {
        assert!(self.current >= reg.get(), "registers freed out of order");
        self.current = reg.get();
    }
}

#[derive(Clone, Copy, Debug)]
enum Symbol {
    Host(HostId),
    Fn(FnId),
    Mvar(MVar),
    Var(TyReg),
}

pub struct CodeUnit {
    functions: Box<FnTable>,
}

pub struct Library {
    functions: Cow<'static, [ExternFunctionDecl]>,
}

#[derive(Clone)]
pub struct ExternFunctionDecl {
    pub name: &'static str,
    pub sig: ExternFunctionSig,
    pub callback: ExternFunctionCallback,
}

impl Library {
    pub fn new() -> Self {
        Self {
            functions: default(),
        }
    }

    /// ### Safety
    /// - All host function signatures must match their callbacks' signatures
    pub const unsafe fn from_static(functions: &'static [ExternFunctionDecl]) -> Self {
        Self {
            functions: Cow::Borrowed(functions),
        }
    }

    /// ### Safety
    /// - The signature must match the callback's signature
    pub unsafe fn add(
        mut self,
        name: &'static str,
        signature: ExternFunctionSig,
        callback: ExternFunctionCallback,
    ) -> Self {
        let mut functions = self.functions.into_owned();
        functions.push(ExternFunctionDecl {
            name,
            sig: signature,
            callback,
        });
        self.functions = Cow::Owned(functions);
        self
    }
}

impl CodeUnit {
    pub fn link(&self) -> Result<vm::Module> {
        let host = Library::new();
        self.link_with(&host)
    }

    pub fn link_with(&self, host: &Library) -> Result<vm::Module> {
        let mut missing = vec![];
        let mut extra = vec![];

        for name in self.functions.host_map.keys() {
            if !host.functions.iter().any(|decl| decl.name == name) {
                missing.push(name.clone());
            }
        }

        for decl in host.functions.iter() {
            if !self.functions.host_map.contains_key(decl.name) {
                extra.push(decl.name);
            }
        }

        if !missing.is_empty() || !extra.is_empty() {
            return Err(Error::simple(format!(
                "invalid host functions:\nmissing: {}\nextra: {}",
                missing.into_iter().join(", "),
                extra.into_iter().join(", ")
            ))
            .into());
        }

        let empty_host_fn = ExternFunction {
            name: "",
            callback: |_| unreachable!(),
        };
        let mut host_fns = vec![empty_host_fn; self.functions.host.len()];
        for decl in host.functions.iter() {
            let id = self.functions.host_map.get(decl.name).unwrap();
            // TODO: check signature?
            host_fns[id.to_index()] = ExternFunction::new(decl.name, decl.callback);
        }

        Ok(vm::Module {
            main: self.functions.main.clone(),
            script_fns: self.functions.script.clone(),
            host_fns,
        })
    }
}

pub struct FnTable {
    main: Arc<Function>,

    script_map: BTreeMap<String, FnId>,
    script: Vec<Arc<Function>>,

    host_map: BTreeMap<String, HostId>,
    host: Vec<Arc<ExternFunctionSig>>,
}

struct FnTableBuilder {
    next_fnid: u16,
    next_host_id: u16,

    table: FnTable,
    empty_fn: Arc<Function>,
}

impl FnTableBuilder {
    fn new() -> Self {
        let empty_fn: Arc<Function> = Arc::new(Function {
            name: "<empty>".into(),
            params: 0,
            bytecode: default(),
            spans: default(),
            literals: default(),
            registers: 0,
        });
        Self {
            next_fnid: 0,
            next_host_id: 0,

            table: FnTable {
                main: empty_fn.clone(),

                script_map: default(),
                script: default(),

                host_map: default(),
                host: default(),
            },
            empty_fn,
        }
    }

    fn get_script_id(&self, name: &str) -> Option<FnId> {
        self.table.script_map.get(name).copied()
    }

    fn get_host_id(&self, name: &str) -> Option<HostId> {
        self.table.host_map.get(name).copied()
    }

    fn set_main(&mut self, main: Function) {
        self.table.main = Arc::new(main);
    }

    fn reserve_script(&mut self, name: Ident<'_>) -> FnId {
        let id = FnId(self.next_fnid);
        self.next_fnid += 1;

        self.table.script_map.insert(name.to_string(), id);
        self.table.script.push(Arc::clone(&self.empty_fn));

        id
    }

    fn set_script(&mut self, id: FnId, fn_: Function) {
        self.table.script[id.to_index()] = Arc::new(fn_);
    }

    fn reserve_host(&mut self, name: String, sig: ExternFunctionSig) {
        let id = HostId(self.next_host_id);
        self.next_host_id += 1;
        self.table.host_map.insert(name, id);
        self.table.host.push(Arc::new(sig));
    }

    fn finish(self) -> FnTable {
        self.table
    }
}

#[derive(Default)]
pub struct Function {
    pub name: String,
    pub params: u8,
    pub bytecode: Vec<Op>,
    pub spans: Vec<Span>,
    pub literals: Vec<Literal>,
    pub registers: u8,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("params", &self.params)
            .field("registers", &self.registers)
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Copy)]
pub struct ExternFunction {
    pub name: &'static str,
    pub callback: ExternFunctionCallback,
}

impl ExternFunction {
    pub fn new(name: &'static str, callback: ExternFunctionCallback) -> Self {
        Self { name, callback }
    }
}

#[derive(Clone)]
pub struct ExternFunctionSig {
    pub params: Cow<'static, [hir::Ty]>,
    pub ret: hir::Ty,
}

#[doc(hidden)]
#[cfg(any(test, feature = "__debug"))]
pub mod print;

#[cfg(test)]
mod tests;
