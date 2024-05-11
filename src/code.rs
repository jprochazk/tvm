use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::sync::Arc;

use crate::ast::{BinaryOp, Ident};
use crate::error::{ErrorCtx, Report, Result};
use crate::hir::Hir;
use crate::lex::Span;
use crate::value::{f64n, Literal, LiteralPool};
use crate::{hir, vm, HashMap, Str};

pub mod op;
use op::*;

pub fn compile(hir: Hir<'_>) -> Result<CodeUnit, Report> {
    Compiler {
        ecx: ErrorCtx::new(hir.src),
        module_state: ModuleState {
            vars: HashMap::default(),
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
        for (hir_id, fn_) in hir.fns.iter() {
            self.module_state.fn_table.reserve(fn_.name, hir_id);
        }

        // 2. compile top-level code
        let top_level = generate_main_fn(hir.top_level);
        match function(&mut self, &top_level) {
            Ok(fn_) => self.module_state.fn_table.set_main(fn_),
            Err(e) => self.ecx.push(e),
        }

        // 3. compile all functions, place them into their respective slots
        for (hir_id, fn_) in hir.fns.iter() {
            match function(&mut self, fn_) {
                Ok(fn_) => self.module_state.fn_table.relocate(hir_id, fn_),
                Err(e) => self.ecx.push(e),
            }
        }

        let function_table = self.module_state.fn_table.finish();

        self.ecx.finish()?;

        Ok(CodeUnit {
            functions: function_table,
        })
    }
}

struct ModuleState<'src> {
    vars: HashMap<Ident<'src>, Mvar>,
    fn_table: FnTableBuilder,
}

struct FunctionState<'src, 'a> {
    compiler: &'a mut Compiler<'src>,
    hir: &'a hir::Fn<'src>,
    current_loop: Option<Loop>,
    asm: Assembler<'src>,
    regalloc: RegisterAllocator,
    locals: Vec<HashMap<Ident<'src>, Reg>>,
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
            return Ok(Function::default());
        };

        self.scope(|f| -> Result<()> {
            f.with_reg(Span::empty(), None, |f, ret_r| {
                for param in &self.hir.sig.params {
                    let reg = f.alloc_reg(param.name.span)?;
                    f.declare_local(param.name, reg);
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
            return Symbol::var(name.span, reg);
        }

        if let Some(idx) = self.resolve_module_var(&name) {
            return Symbol::mvar(name.span, idx);
        }

        if let Some(id) = self.resolve_function(&name) {
            return Symbol::fn_(name.span, id);
        }

        unreachable!("BUG: unresolved variable: {name}@{}", name.span);
    }

    fn resolve_local(&self, name: &str) -> Option<Reg> {
        for scope in self.locals.iter().rev() {
            if let Some(reg) = scope.get(name).copied() {
                return Some(reg);
            }
        }
        None
    }

    fn resolve_module_var(&self, name: &str) -> Option<Mvar> {
        self.compiler.module_state.vars.get(name).copied()
    }

    fn resolve_function(&self, name: &str) -> Option<Fnid> {
        self.compiler.module_state.fn_table.get_id(name)
    }

    fn resolve_local_in_current_scope(&self, name: &str) -> Option<Reg> {
        self.locals
            .last()
            .and_then(|scope| scope.get(name))
            .copied()
    }

    fn declare_local(&mut self, name: Ident<'src>, reg: Reg) {
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
        self.locals.push(HashMap::default());
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
        None => f.alloc_reg(hir.name.span)?,
    };
    assign_to(f, &hir.init, dst)?;
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
        hir::ExprKind::Unary(_hir) => todo!(),
        hir::ExprKind::Primitive(hir) => primitive_expr(f, span, hir, dst),
        hir::ExprKind::UseVar(hir) => use_var_expr(f, span, hir, dst),
        hir::ExprKind::UseField(_hir) => todo!(),
        hir::ExprKind::UseIndex(_hir) => todo!(),
        hir::ExprKind::AssignVar(_hir) => todo!(),
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
        let lhs = expr(f, &hir.lhs, Some(dst))?.unwrap_or(dst);
        let fresh_rhs = f.alloc_reg(span)?;
        let rhs = expr(f, &hir.rhs, Some(fresh_rhs))?.unwrap_or(fresh_rhs);

        use asm::*;

        macro_rules! emit_binop {
            ($kind:ident) => {
                if hir.lhs.ty.is_int() {
                    f.asm.emit(span, $kind(I64, lhs, rhs, dst));
                } else if hir.lhs.ty.is_num() {
                    f.asm.emit(span, $kind(F64, lhs, rhs, dst));
                } else {
                    unreachable!("BUG: binary expr consisting of non-numeric types: {hir:#?}");
                }
            };
        }

        use BinaryOp as O;
        match hir.op {
            O::Add => emit_binop!(add),
            O::Sub => emit_binop!(sub),
            O::Mul => emit_binop!(mul),
            O::Div => emit_binop!(div),
            O::Rem => emit_binop!(rem),
            // O::Pow => emit_binop!(Pow),
            O::Eq => emit_binop!(ceq),
            O::Ne => emit_binop!(cne),
            O::Gt => emit_binop!(cgt),
            O::Lt => emit_binop!(clt),
            O::Ge => emit_binop!(cge),
            O::Le => emit_binop!(cle),
            _ => {}
        }

        f.free_reg(fresh_rhs);

        Ok(None)
    })
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
    use asm::*;

    let symbol = f.resolve_symbol(hir.name);
    let ret = match symbol.kind {
        SymbolKind::Fn(id) => {
            let Some(dst) = dst else { return Ok(None) };
            f.asm.emit(span, load_fn(id, dst));
            None
        }
        SymbolKind::Mvar(_v) => todo!("module variables"),
        SymbolKind::Var(reg) => Some(reg),
    };

    Ok(ret)
}

fn call_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Call<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    if let hir::ExprKind::UseVar(var) = &hir.callee.kind {
        if let SymbolKind::Fn(fnid) = f.resolve_symbol(var.name).kind {
            //   fn f() {}
            //   f(); // calling function directly
            return call_expr_direct(f, span, hir, fnid, dst);
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
    fnid: op::Fnid,
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
        f.asm.emit(span, call_id(fnid, ret));

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
        Self::default()
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
struct Symbol {
    span: Span,
    kind: SymbolKind,
}

#[derive(Clone, Copy, Debug)]
enum SymbolKind {
    Fn(Fnid),
    Mvar(Mvar),
    Var(Reg),
}

impl Symbol {
    fn fn_(span: Span, id: Fnid) -> Self {
        Self {
            span,
            kind: SymbolKind::Fn(id),
        }
    }

    fn mvar(span: Span, idx: Mvar) -> Self {
        Self {
            span,
            kind: SymbolKind::Mvar(idx),
        }
    }

    fn var(span: Span, reg: Reg) -> Self {
        Self {
            span,
            kind: SymbolKind::Var(reg),
        }
    }
}

pub struct CodeUnit {
    functions: FnTable,
}

impl CodeUnit {
    pub fn link(&self) -> vm::Module {
        vm::Module {
            main: self.functions.main.clone(),
            functions: self.functions.script.clone(),
        }
    }
}

pub struct FnTable {
    name_to_id: BTreeMap<String, Fnid>,
    reloc_table: BTreeMap<hir::FnId, Fnid>,
    main: Arc<Function>,
    script: Vec<Arc<Function>>,
}

struct FnTableBuilder {
    next_id: u16,
    table: FnTable,
    empty_fn: Arc<Function>,
}

impl FnTableBuilder {
    fn new() -> Self {
        let empty_fn: Arc<Function> = Default::default();
        Self {
            next_id: 0,
            table: FnTable {
                name_to_id: BTreeMap::new(),
                reloc_table: BTreeMap::new(),
                main: empty_fn.clone(),
                script: Vec::new(),
            },
            empty_fn,
        }
    }

    fn get_id(&self, name: &str) -> Option<Fnid> {
        self.table.name_to_id.get(name).copied()
    }

    fn set_main(&mut self, main: Function) {
        self.table.main = Arc::new(main);
    }

    fn reserve(&mut self, name: Ident<'_>, hir_id: hir::FnId) {
        let table_id = Fnid(self.next_id);
        self.next_id += 1;

        self.table.name_to_id.insert(name.to_string(), table_id);
        self.table.reloc_table.insert(hir_id, table_id);
        self.table.script.push(Arc::clone(&self.empty_fn));
    }

    fn relocate(&mut self, hir_id: hir::FnId, fn_: Function) {
        let index = self.table.reloc_table[&hir_id].to_index();
        self.table.script[index] = Arc::new(fn_);
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

#[cfg(test)]
pub(crate) mod print;

#[cfg(test)]
mod tests;
