use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};

use crate::ast::{BinaryOp, Ident};
use crate::error::{Error, ErrorCtx, Location, Result};
use crate::hir::Hir;
use crate::lex::Span;
use crate::value::{f64n, Constant, ConstantPoolBuilder};
use crate::{hir, HashMap, Str};

pub mod op;
use op::*;

pub fn compile(hir: Hir<'_>) -> Result<Module, Vec<Error>> {
    Compiler {
        ecx: ErrorCtx::new(hir.src),
        module_state: ModuleState {
            vars: HashMap::default(),
            functions: FunctionTableBuilder::new(),
        },
    }
    .compile(hir)
}

struct Compiler<'src> {
    ecx: ErrorCtx<'src>,
    module_state: ModuleState<'src>,
}

impl<'src> Compiler<'src> {
    fn compile(mut self, hir: Hir<'src>) -> Result<Module, Vec<Error>> {
        // 1. reserve a slot in the function table for each function
        for (hir_id, fn_) in hir.fns.iter() {
            self.module_state.functions.reserve(fn_.name, hir_id);
        }

        // 2. compile top-level code
        let top_level = generate_main_fn(hir.top_level);
        match function(&mut self, &top_level) {
            Ok(fn_) => self.module_state.functions.main = fn_,
            Err(e) => self.ecx.push(e),
        }

        // 3. compile all functions, place them into their respective slots
        for (hir_id, fn_) in hir.fns.iter() {
            match function(&mut self, fn_) {
                Ok(fn_) => self.module_state.functions.relocate(hir_id, fn_),
                Err(e) => self.ecx.push(e),
            }
        }

        let function_table = self.module_state.functions.finish();

        self.ecx.finish()?;

        Ok(Module {
            functions: function_table,
        })
    }
}

struct ModuleState<'src> {
    vars: HashMap<Ident<'src>, Mvar>,
    functions: FunctionTableBuilder,
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
            use asm::*;
            match body.tail.is_some() {
                true => {
                    let ret_r = f.alloc_reg(Span::empty())?;

                    for param in &self.hir.sig.params {
                        let reg = f.alloc_reg(param.name.span)?;
                        f.declare_local(param.name, reg);
                    }

                    let ret_r = block_expr(f, body, Some(ret_r))?.unwrap_or(ret_r);
                    f.asm.emit(Span::empty(), retv(ret_r));
                }
                false => {
                    for param in &self.hir.sig.params {
                        let reg = f.alloc_reg(param.name.span)?;
                        f.declare_local(param.name, reg);
                    }

                    block_expr(f, body, None)?;
                    f.asm.emit(Span::empty(), ret());
                }
            }

            Ok(())
        })?;

        let (bytecode, spans, constants) = self.asm.finish();
        let registers = self.regalloc.total;
        Ok(Function {
            name: self.hir.name.to_string(),
            params: self.hir.sig.params.len() as u8,
            bytecode,
            spans,
            constants,
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
        self.compiler.module_state.functions.get_id(name)
    }

    fn current_scope(&self) -> &HashMap<Ident<'_>, Reg> {
        self.locals.last().unwrap()
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

    fn alloc_const_int(&mut self, span: Span, v: i64) -> Result<Cst> {
        self.asm
            .constants
            .insert_int(v)
            .map(Cst)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
    }

    fn alloc_const_num(&mut self, span: Span, v: f64n) -> Result<Cst> {
        self.asm
            .constants
            .insert_num(v)
            .map(Cst)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
    }

    fn alloc_const_str(&mut self, span: Span, v: Str<'src>) -> Result<Cst> {
        self.asm
            .constants
            .insert_str(v)
            .map(Cst)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
    }

    fn alloc_const_jump_offset(&mut self, span: Span, v: isize) -> Result<Cst> {
        self.asm
            .constants
            .insert_jump_offset(v)
            .map(Cst)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
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
        let (free, reg) = match reg {
            Some(reg) => (false, reg),
            None => (true, self.alloc_reg(span)?),
        };
        let result = inner(self, reg);
        if free {
            self.free_reg(reg);
        }
        result
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
        hir::ExprKind::Return(_hir) => todo!(),
        hir::ExprKind::Break(_hir) => break_expr(f, span).map(|_| None),
        hir::ExprKind::Continue(_hir) => continue_expr(f, span).map(|_| None),
        hir::ExprKind::Block(_hir) => break_expr(f, span).map(|_| None),
        hir::ExprKind::If(_hir) => todo!(),
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
            // O::Eq => emit_binop!(Eq),
            // O::Ne => emit_binop!(Ne),
            // O::Gt => emit_binop!(Gt),
            // O::Lt => emit_binop!(Lt),
            // O::Ge => emit_binop!(Ge),
            // O::Le => emit_binop!(Le),
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
        hir::Primitive::Int(v) => match Smi::try_new(*v) {
            Some(v) => f.asm.emit(span, load_smi(v, dst)),
            None => {
                let idx = f.alloc_const_int(span, *v)?;
                f.asm.emit(span, load_cst(idx, dst));
            }
        },
        hir::Primitive::Num(v) => {
            let idx = f.alloc_const_num(span, *v)?;
            f.asm.emit(span, load_cst(idx, dst));
        }
        hir::Primitive::Bool(v) => {
            f.asm.emit(span, load_bool(*v, dst));
        }
        hir::Primitive::Str(v) => {
            let idx = f.alloc_const_str(span, v.clone())?;
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
            // can do direct function call for code like this:
            //   fn f() {}
            //   f(); // directly references a function
            return call_expr_direct(f, span, hir, fnid, dst);
        }
    }

    // boring value call:
    //   fn f() {}
    //   let indirection = f;
    //   indirection();
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

fn block_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    hir: &'a hir::Block<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    use asm::*;

    f.scope(|f| {
        for v in &hir.body {
            stmt(f, v)?;
        }

        match (&hir.tail, dst) {
            (Some(tail), Some(dst)) => {
                if let Some(out) = expr(f, tail, Some(dst))? {
                    // if `dst` refers to variable in an outer scope
                    // then we don't need to emit a `mov`, and can instead
                    // just return the register directly
                    //
                    // if `dst` refers to a variable in an inner scope,
                    // we must emit a `mov`, because the variable will
                    // no longer be live after the end of this block

                    if out != dst && f.current_scope().values().any(|v| *v == out) {
                        // `dst` is in current scope
                        f.asm.emit(tail.span, mov(out, dst));
                        Ok(None)
                    } else {
                        // `dst` is in outer scope
                        Ok(Some(out))
                    }
                } else {
                    Ok(None)
                }
            }
            (Some(tail), None) => {
                expr(f, tail, None)?;
                Ok(None)
            }
            (None, Some(dst)) => {
                f.asm.emit(Span::empty(), load_unit(dst));
                Ok(None)
            }
            (None, None) => Ok(None),
        }
    })
}

fn assign_to<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    value: &'a hir::Expr<'src>,
    dst: Reg,
) -> Result<()> {
    use asm::*;

    if let Some(out) = expr(f, value, Some(dst))? {
        // `expr` was written to `out`
        if out != dst {
            // `out` and `dst` are different registers
            f.asm.emit(value.span, mov(out, dst));
        }
    }

    Ok(())
}

struct Loop {
    entry: BackwardJumpLabel,
    exit: ForwardJumpLabel,
}

#[derive(Default)]
struct Assembler<'src> {
    bytecode: Vec<Op>,
    spans: Vec<Span>,
    constants: ConstantPoolBuilder<'src>,
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

    fn finish(mut self) -> (Vec<Op>, Vec<Span>, Vec<Constant>) {
        if !self.bytecode.ends_with(&[Op::Stop]) {
            self.emit(Span::empty(), asm::stop());
        }
        (self.bytecode, self.spans, self.constants.finish())
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

    fn emit_jmp(&self, f: &mut FunctionState, span: Span) -> Result<()> {
        let from = f.asm.bytecode.len() as isize;
        let to = self.pos;
        let offset = Offset::get(f, from, to)?;
        f.asm.emit(span, asm::jmp(offset));
        Ok(())
    }

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

    fn emit_jmp(&self, f: &mut FunctionState, span: Span) {
        let referrer = f.asm.bytecode.len();
        self.referrers.borrow_mut().push(referrer);
        f.asm.emit(span, asm::jmp(Offset::placeholder()));
    }

    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: Reg) {
        let referrer = f.asm.bytecode.len();
        self.referrers.borrow_mut().push(referrer);
        f.asm.emit(span, asm::jmpf(cond, Offset::placeholder()));
    }
}

impl Offset {
    fn get(f: &mut FunctionState, to: isize, from: isize) -> Result<Self> {
        let offset = from - to;
        let offset = if let Ok(offset) = i16::try_from(offset) {
            Offset::Rel(Rel(offset))
        } else {
            Offset::Cst(f.alloc_const_jump_offset(Span::empty(), offset)?)
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

pub struct Module {
    functions: FunctionTable,
}

impl Module {
    fn functions(&self) -> impl Iterator<Item = (op::Fnid, &FnTableEntry)> {
        self.functions
            .entries
            .iter()
            .enumerate()
            .map(|(i, fn_)| (op::Fnid(i as u16), fn_))
    }
}

struct FunctionTable {
    name_to_id: BTreeMap<String, Fnid>,
    reloc_table: BTreeMap<hir::FnId, Fnid>,
    main: Function,
    entries: Vec<FnTableEntry>,
}

struct FunctionTableBuilder {
    next_id: u16,
    name_to_id: BTreeMap<String, Fnid>,
    reloc_table: BTreeMap<hir::FnId, Fnid>,
    main: Function,
    fns: Vec<FnTableEntry>,
}

impl FunctionTableBuilder {
    fn new() -> Self {
        Self {
            next_id: 0,
            name_to_id: BTreeMap::new(),
            reloc_table: BTreeMap::new(),
            main: Function::default(),
            fns: Vec::new(),
        }
    }

    fn get_id(&self, name: &str) -> Option<Fnid> {
        self.name_to_id.get(name).copied()
    }

    fn reserve(&mut self, name: Ident<'_>, hir_id: hir::FnId) {
        let table_id = Fnid(self.next_id);
        self.next_id += 1;

        self.name_to_id.insert(name.to_string(), table_id);
        self.reloc_table.insert(hir_id, table_id);
        self.fns.push(FnTableEntry::Script(Box::default()));
    }

    fn relocate(&mut self, hir_id: hir::FnId, fn_: Function) {
        self.fns[self.reloc_table[&hir_id].to_index()] = FnTableEntry::Script(Box::new(fn_));
    }

    fn finish(self) -> FunctionTable {
        FunctionTable {
            name_to_id: self.name_to_id,
            reloc_table: self.reloc_table,
            main: self.main,
            entries: self.fns,
        }
    }
}

pub enum FnTableEntry {
    Script(Box<Function>),
    Host(Box<ExternFunction>),
}

#[derive(Default)]
pub struct Function {
    // captures: Vec<Capture>,
    name: String,
    params: u8,
    bytecode: Vec<Op>,
    spans: Vec<Span>,
    constants: Vec<Constant>,
    registers: u8,
}

pub struct ExternFunction {
    // TODO: type info
    name: String,
}

/* enum Capture {
    NonLocal(Reg),
    Parent(Cap),
} */

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&DisplayModule(self, None), f)
    }
}

impl Module {
    pub fn display<'a, 'src>(&'a self, src: &'src str) -> DisplayModule<'a, 'src> {
        DisplayModule(self, Some(src))
    }
}

pub struct DisplayModule<'a, 'src>(&'a Module, Option<&'src str>);
impl<'a, 'src> Display for DisplayModule<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisplayModule(m, src) = self;

        writeln!(
            f,
            "{}",
            DisplayScriptFunction(None, &m.functions.main, *src)
        )?;
        for (id, fn_) in m.functions() {
            match fn_ {
                FnTableEntry::Script(fn_) => {
                    writeln!(f, "{}", DisplayScriptFunction(Some(id), fn_, *src))?
                }
                FnTableEntry::Host(fn_) => writeln!(f, "{}", DisplayExternFunction(Some(id), fn_))?,
            }
        }
        Ok(())
    }
}

struct DisplayExternFunction<'a>(Option<op::Fnid>, &'a ExternFunction);
impl<'a> Display for DisplayExternFunction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(id, fn_) = self;

        write!(f, "function {:?}", fn_.name)?;
        if let Some(id) = id {
            write!(f, " #{id}")?;
        }
        writeln!(f, " {{")?;
        writeln!(f, "  extern")?;
        writeln!(f, "}}")
    }
}

struct DisplayScriptFunction<'a, 'src>(Option<op::Fnid>, &'a Function, Option<&'src str>);
impl<'a, 'src> Display for DisplayScriptFunction<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(id, fn_, src) = self;

        write!(f, "function {:?}", fn_.name)?;
        if let Some(id) = id {
            write!(f, " #{}", id.0)?;
        }
        writeln!(f, " {{")?;
        writeln!(f, "  registers: {} ({} params)", fn_.registers, fn_.params)?;
        write!(f, "  constants: ")?;
        if fn_.constants.is_empty() {
            writeln!(f, "[]")?;
        } else {
            writeln!(f, "[")?;
            for c in &fn_.constants {
                writeln!(f, "    {c:?}")?;
            }
            writeln!(f, "  ]")?;
        }
        writeln!(f, "  bytecode: ({} ops) [", fn_.bytecode.len())?;

        let mut max_len = 0;
        let mut instructions = Vec::with_capacity(fn_.bytecode.len());
        for op in fn_.bytecode.iter() {
            let s = op.to_string();
            max_len = std::cmp::max(max_len, s.len());
            instructions.push(s);
        }

        let mut prev_line_span = Span::empty();
        let mut instructions = instructions.into_iter().zip(fn_.spans.iter()).peekable();
        while let Some((instruction, span)) = instructions.next() {
            f.write_str("    ")?;
            'next: {
                if let Some(src) = src {
                    write!(f, "{instruction:max_len$}  // ")?;

                    if span.is_empty() {
                        break 'next;
                    }

                    let loc = Location::from_source_span(src, span);
                    if prev_line_span == loc.line_span() {
                        break 'next;
                    }
                    prev_line_span = loc.line_span();

                    f.write_str(src[loc.line_span()].trim())?;
                } else {
                    write!(f, "{instruction}")?;
                }
            }
            if instructions.peek().is_some() {
                f.write_str("\n")?;
            }
        }
        writeln!(f, "  ")?;
        writeln!(f, "  ]")?;
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod tests;
