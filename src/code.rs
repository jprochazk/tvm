use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::fmt::Debug;

use crate::ast::{BinaryOp, Ident, UnaryOp};
use crate::error::{Error, ErrorCtx, Report, Result};
use crate::hir::Hir;
use crate::lex::Span;
use crate::util::{default, JoinIter};
use crate::vm::operands::{ExternFunctionId, FunctionId, LiteralId, PcRelativeOffset, Register};
use crate::vm::value::pool::LiteralPool;
use crate::vm::value::Literal;
use crate::vm::{
    self, asm, operands, DecodedInstruction, ExternFunction, ExternFunctionAbi, Instruction,
};
use crate::{hir, HashMap};

pub fn compile(hir: Hir<'_>, library: &Library) -> Result<vm::Module, Report> {
    Compiler {
        ecx: ErrorCtx::new(hir.src),
        module_state: ModuleState {
            fn_table: FnTableBuilder::new(),
        },
    }
    .compile(hir, library)
}

struct Compiler<'src> {
    ecx: ErrorCtx<'src>,
    module_state: ModuleState,
}

impl<'src> Compiler<'src> {
    fn compile(mut self, hir: Hir<'src>, library: &Library) -> Result<vm::Module, Report> {
        // 1. Generate and store entrypoint
        let main_fn = generate_main_fn(hir.top_level);
        let entry = self.module_state.fn_table.reserve_function(main_fn.name);
        self.module_state.fn_table.table.entry = entry;

        // 2. Reserve a slot in the function table for each function
        let mut pending_fns =
            Vec::<(operands::FunctionId, &hir::Fn)>::with_capacity(hir.fns.len() + 1);
        pending_fns.push((entry, &main_fn));
        for (_, fn_) in hir.fns.iter() {
            if fn_.is_extern_fn() {
                // for host functions, we just reserve them
                // they are filled in later by `CodeUnit::link`
                self.module_state.fn_table.reserve_external_function(
                    fn_.name.to_string(),
                    ExternFunctionAbi {
                        params: fn_.sig.params.iter().map(|p| p.ty).collect(),
                        ret: fn_.sig.ret,
                    },
                )
            } else {
                // we need to codegen each script function
                pending_fns.push((self.module_state.fn_table.reserve_function(fn_.name), fn_));
            }
        }

        // 3. Compile all functions (including the entrypoint), place them into their
        //    respective slots
        for (id, fn_) in pending_fns {
            match function(&mut self, fn_, &hir.defs, &hir.fns) {
                Ok(fn_) => self.module_state.fn_table.set_function(id, fn_),
                Err(e) => self.ecx.push(e),
            }
        }

        let function_table = self.module_state.fn_table.finish();
        let external_functions = match link_external_functions(&function_table, library) {
            Ok(external_functions) => external_functions,
            Err(e) => {
                self.ecx.push(e);
                vec![]
            }
        };
        let functions = function_table
            .functions
            .into_iter()
            .map(Function::finish)
            .collect();

        self.ecx.finish()?;

        Ok(vm::Module {
            entry,
            functions,
            external_functions,
        })
    }
}

fn link_external_functions(
    function_table: &FnTable,
    library: &Library,
) -> Result<Vec<ExternFunction>> {
    let mut missing = vec![];
    for name in function_table.external_function_map.keys() {
        if !library.functions.iter().any(|decl| decl.name == name) {
            missing.push(name.clone());
        }
    }

    let mut extra = vec![];
    for decl in library.functions.iter() {
        if !function_table.external_function_map.contains_key(decl.name) {
            extra.push(decl.name);
        }
    }

    if !missing.is_empty() || !extra.is_empty() {
        return Err(Error::simple(format!(
            "invalid external functions:\nmissing: {}\nextra: {}",
            missing.into_iter().join(", "),
            extra.into_iter().join(", ")
        ))
        .into());
    }

    fn unreachable(_: vm::Scope) {
        unreachable!("ICE: unreachable");
    }

    let empty_external_function = vm::function!(unreachable);
    let mut external_functions =
        vec![empty_external_function; function_table.external_functions.len()];
    for decl in library.functions.iter() {
        let id = function_table.external_function_map.get(decl.name).unwrap();
        // TODO: check signature?
        external_functions[*id as usize] = decl.clone();
    }

    Ok(external_functions)
}

struct ModuleState {
    fn_table: FnTableBuilder,
}

struct FunctionState<'src, 'a> {
    compiler: &'a mut Compiler<'src>,
    hir: &'a hir::Fn<'src>,
    defs: &'a hir::Defs<'src>,
    fns: &'a hir::Fns<'src>,
    current_loop: Option<Loop>,
    asm: Assembler,
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
    defs: &hir::Defs<'src>,
    fns: &hir::Fns<'src>,
) -> Result<Function> {
    FunctionState {
        compiler,
        hir,
        defs,
        fns,
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

        if let Some(id) = self.resolve_script_function(&name) {
            return Symbol::Fn(id);
        }

        if let Some(id) = self.resolve_host_function(&name) {
            return Symbol::Extern(id);
        }

        unreachable!("ICE: unresolved variable: {name}@{}", name.span);
    }

    fn resolve_local(&self, name: &str) -> Option<TyReg> {
        for scope in self.locals.iter().rev() {
            if let Some(reg) = scope.get(name).copied() {
                return Some(reg);
            }
        }
        None
    }

    fn resolve_script_function(&self, name: &str) -> Option<operands::FunctionId> {
        self.compiler.module_state.fn_table.get_function_id(name)
    }

    fn resolve_host_function(&self, name: &str) -> Option<operands::ExternFunctionId> {
        self.compiler
            .module_state
            .fn_table
            .get_external_function_id(name)
    }

    fn resolve_local_in_current_scope(&self, name: &str) -> Option<TyReg> {
        self.locals
            .last()
            .and_then(|scope| scope.get(name))
            .copied()
    }

    fn declare_local(&mut self, name: Ident<'src>, reg: TyReg) {
        let scope = self.locals.last_mut().expect("ICE: no open scope");
        let _ = scope.insert(name, reg);
    }

    fn alloc_reg(&mut self, span: Span) -> Result<operands::Register> {
        match self.regalloc.alloc() {
            Some(reg) => Ok(reg),
            None => Err(self.compiler.ecx.too_many_registers(span)),
        }
    }

    fn free_reg(&mut self, reg: operands::Register) {
        self.regalloc.free(reg);
    }

    fn literal(&mut self, span: Span, v: impl Into<Literal>) -> Result<operands::LiteralId> {
        self.asm
            .literal_pool
            .insert(v)
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
        reg: Option<operands::Register>,
        inner: impl FnOnce(&mut Self, operands::Register) -> Result<T>,
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

fn _asdf() {
    let mut v = 1;
    let mut v = 2;
    let u = v;
}

fn expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    hir: &'a hir::Expr<'src>,
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
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
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
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
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
    f.with_reg(span, dst, |f, dst| {
        let rhs = expr(f, &hir.rhs, Some(dst))?.unwrap_or(dst).ty(hir.rhs.ty);

        emit_unop(f, span, rhs, hir.op, dst);

        Ok(None)
    })
}

#[derive(Debug, Clone, Copy)]
struct TyReg {
    ty: hir::Ty,
    reg: operands::Register,
}

impl TyReg {
    fn new(ty: hir::Ty, reg: operands::Register) -> Self {
        Self { ty, reg }
    }
}

trait TyRegExt {
    fn ty(self, ty: hir::Ty) -> TyReg;
}

impl TyRegExt for Register {
    fn ty(self, ty: hir::Ty) -> TyReg {
        TyReg::new(ty, self)
    }
}

fn emit_binop(
    f: &mut FunctionState,
    span: Span,
    lhs: TyReg,
    rhs: TyReg,
    op: BinaryOp,
    dst: operands::Register,
) {
    use asm::*;
    use BinaryOp as O;
    let op = match (op, lhs.ty) {
        (O::Add, ty) if ty.is_int() => add_i64,
        (O::Add, ty) if ty.is_num() => add_f64,
        (O::Sub, ty) if ty.is_int() => sub_i64,
        (O::Sub, ty) if ty.is_num() => sub_f64,
        (O::Mul, ty) if ty.is_int() => mul_i64,
        (O::Mul, ty) if ty.is_num() => mul_f64,
        (O::Div, ty) if ty.is_int() => div_i64,
        (O::Div, ty) if ty.is_num() => div_f64,
        (O::Rem, ty) if ty.is_int() => rem_i64,
        (O::Rem, ty) if ty.is_num() => rem_f64,
        (O::Eq, ty) if ty.is_int() => cmp_eq_i64,
        (O::Eq, ty) if ty.is_num() => cmp_eq_f64,
        (O::Eq, ty) if ty.is_bool() => cmp_eq_bool,
        (O::Ne, ty) if ty.is_int() => cmp_ne_i64,
        (O::Ne, ty) if ty.is_num() => cmp_ne_f64,
        (O::Ne, ty) if ty.is_bool() => cmp_ne_bool,
        (O::Gt, ty) if ty.is_int() => cmp_gt_i64,
        (O::Gt, ty) if ty.is_num() => cmp_gt_f64,
        (O::Lt, ty) if ty.is_int() => cmp_lt_i64,
        (O::Lt, ty) if ty.is_num() => cmp_lt_f64,
        (O::Ge, ty) if ty.is_int() => cmp_ge_i64,
        (O::Ge, ty) if ty.is_num() => cmp_ge_f64,
        (O::Le, ty) if ty.is_int() => cmp_le_i64,
        (O::Le, ty) if ty.is_num() => cmp_le_f64,
        _ => todo!(),
    };

    if !lhs.ty.is_int() && !lhs.ty.is_num() {
        unreachable!("ICE: binary expr consisting of non-numeric types at {span}");
    };

    f.asm.emit(span, op(dst, lhs.reg, rhs.reg));
}

fn emit_unop(f: &mut FunctionState, span: Span, rhs: TyReg, op: UnaryOp, dst: operands::Register) {
    use asm::*;
    use UnaryOp as O;
    match op {
        O::Minus => {
            let op = if rhs.ty.is_int() {
                neg_i64
            } else if rhs.ty.is_num() {
                neg_f64
            } else {
                unreachable!("ICE: unary minus with non-numeric type at {span}")
            };
            f.asm.emit(span, op(dst, rhs.reg));
        }
        O::Not => f.asm.emit(span, not_bool(dst, rhs.reg)),
        O::Opt => todo!(),
    }
}

fn primitive_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Primitive<'src>,
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
    use asm::*;

    let Some(dst) = dst else { return Ok(None) };

    match hir {
        hir::Primitive::Int(v) => match i16::try_from(*v) {
            Ok(v) => f.asm.emit(span, load_i16(dst, v)),
            Err(_) => {
                let idx = f.literal(span, *v)?;
                f.asm.emit(span, load_literal(dst, idx));
            }
        },
        hir::Primitive::Num(v) => {
            let idx = f.literal(span, *v)?;
            f.asm.emit(span, load_literal(dst, idx));
        }
        hir::Primitive::Bool(v) => match v {
            true => f.asm.emit(span, load_true(dst)),
            false => f.asm.emit(span, load_false(dst)),
        },
        hir::Primitive::Str(v) => {
            todo!("strings")
            // let idx = f.literal(span, v.clone())?;
            // f.asm.emit(span, load_literal(dst, idx));
        }
    }

    Ok(None)
}

fn use_var_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::UseVar<'src>,
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
    let symbol = f.resolve_symbol(hir.name);

    use asm::*;
    let ret = match symbol {
        Symbol::Fn(id) => {
            let Some(dst) = dst else { return Ok(None) };
            f.asm.emit(span, load_fn(dst, id));
            None
        }
        Symbol::Extern(id) => {
            let Some(dst) = dst else { return Ok(None) };
            f.asm.emit(span, load_fn_extern(dst, id));
            None
        }
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

enum Callee {
    Script(FunctionId),
    Extern(ExternFunctionId),
}

fn call_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Call<'src>,
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
    'value_call: {
        if let hir::ExprKind::UseVar(var) = &hir.callee.kind {
            //   fn f() {}
            //   f(); // calling function directly
            let callee = match f.resolve_symbol(var.name) {
                Symbol::Fn(id) => Callee::Script(id),
                Symbol::Extern(id) => Callee::Extern(id),
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
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
    // caller: [.., ret, a, b, c]
    // callee:     [ret, a, b, c, ..]

    f.with_reg(span, dst, |f, dst| {
        let is_dst_flat = dst + 1 == f.regalloc.current;
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
        match callee {
            Callee::Script(id) => f.asm.emit(span, call(ret, id)),
            Callee::Extern(id) => f.asm.emit(span, call_extern(ret, id)),
        }

        if !is_dst_flat {
            f.asm.emit(span, mov(dst, ret));
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
    dst: Option<operands::Register>,
) -> Result<Option<operands::Register>> {
    // caller: [.., fn, a, b, c]
    // callee:     [ret, a, b, c, ..]

    f.with_reg(span, dst, |f, dst| {
        let is_dst_flat = dst + 1 == f.regalloc.current;
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
        let resolved_fn = f
            .fns
            .get_by_id(
                hir.callee
                    .ty
                    .into_fn()
                    .expect("ICE: callee is not a function"),
            )
            .expect("ICE: failed to get callee");
        if resolved_fn.is_extern_fn() {
            f.asm.emit(span, call_extern_reg(fn_));
        } else {
            f.asm.emit(span, call_reg(fn_));
        }

        let ret = fn_;
        if !is_dst_flat {
            f.asm.emit(span, mov(dst, ret));
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
    dst: Option<operands::Register>,
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
    dst: Option<operands::Register>,
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
fn maybe_move(
    f: &mut FunctionState,
    src: Option<operands::Register>,
    dst: operands::Register,
    span: Span,
) -> Result<()> {
    use asm::*;

    if let Some(src) = src {
        // `expr` was written to `out`
        if src != dst {
            // `out` and `dst` are different registers
            f.asm.emit(span, mov(dst, src));
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
    dst: Register,
) -> Result<()> {
    let out = expr(f, value, Some(dst))?;
    maybe_move(f, out, dst, value.span)
}

struct Loop {
    entry: BackwardJumpLabel,
    exit: ForwardJumpLabel,
}

#[derive(Default)]
struct Assembler {
    bytecode: Vec<Instruction>,
    spans: Vec<Span>,
    literal_pool: LiteralPool,
}

impl Assembler {
    fn new() -> Self {
        default()
    }

    fn emit(&mut self, span: Span, op: Instruction) {
        self.bytecode.push(op);
        self.spans.push(span);
    }

    fn patch_jump(&mut self, referrer: usize, offset: JumpOffset) {
        self.bytecode[referrer] = match self.bytecode[referrer].decode() {
            DecodedInstruction::Jump { .. } => offset.jump(),
            DecodedInstruction::JumpIfFalse { condition, .. } => offset.jump_if_false(condition),
            op => {
                let op = op.name();
                unreachable!("cannot patch {op}@{referrer} as a jump");
            }
        };
    }

    fn finish(self) -> (Vec<Instruction>, Vec<Span>, Vec<Literal>) {
        (self.bytecode, self.spans, self.literal_pool.finish())
    }
}

enum JumpOffset {
    Immediate(PcRelativeOffset),
    Constant(LiteralId),
}

impl JumpOffset {
    fn placeholder() -> Self {
        Self::Immediate(i16::MIN)
    }

    fn get(f: &mut FunctionState, to: isize, from: isize) -> Result<Self> {
        let offset = from - to;
        match i16::try_from(offset) {
            Ok(offset) => Ok(Self::Immediate(offset)),
            Err(_) => Ok(Self::Constant(
                f.literal(Span::empty(), Literal::jmp(offset))?,
            )),
        }
    }

    fn jump(self) -> Instruction {
        use asm::*;
        match self {
            Self::Immediate(offset) => jump(offset),
            Self::Constant(offset) => jump_long(offset),
        }
    }

    fn jump_if_false(self, condition: Register) -> Instruction {
        use asm::*;
        match self {
            Self::Immediate(offset) => jump_if_false(condition, offset),
            Self::Constant(offset) => jump_if_false_long(condition, offset),
        }
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
        let offset = JumpOffset::get(f, from, to)?;
        f.asm.emit(span, offset.jump());
        Ok(())
    }

    /// calculates distance to this label and emits a `jmpf` into `f`
    /// using the distance as the offset
    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: operands::Register) -> Result<()> {
        let from = f.asm.bytecode.len() as isize;
        let to = self.pos;
        let offset = JumpOffset::get(f, from, to)?;
        f.asm.emit(span, offset.jump_if_false(cond));
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
            let offset = JumpOffset::get(f, from, to)?;
            f.asm.patch_jump(*referrer, offset);
        }

        Ok(())
    }

    /// emits a `jmp` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmp(&self, f: &mut FunctionState, span: Span) {
        let referrer = f.asm.bytecode.len();
        self.referrers.borrow_mut().push(referrer);
        f.asm.emit(span, JumpOffset::placeholder().jump());
    }

    /// emits a `jmpf` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: operands::Register) {
        let referrer = f.asm.bytecode.len();
        self.referrers.borrow_mut().push(referrer);
        f.asm
            .emit(span, JumpOffset::placeholder().jump_if_false(cond));
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
            let offset = JumpOffset::get(f, from, to)?;
            f.asm.patch_jump(referrer, offset);
        }

        Ok(())
    }

    /// emits a `jmp` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmp(&self, f: &mut FunctionState, span: Span) {
        let referrer = f.asm.bytecode.len();
        self.referrer.set(Some(referrer));
        f.asm.emit(span, JumpOffset::placeholder().jump());
    }

    /// emits a `jmpf` which will be patched with the real offset
    /// when this label is bound
    fn emit_jmpf(&self, f: &mut FunctionState, span: Span, cond: operands::Register) {
        let referrer = f.asm.bytecode.len();
        self.referrer.set(Some(referrer));
        f.asm
            .emit(span, JumpOffset::placeholder().jump_if_false(cond));
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

    fn alloc(&mut self) -> Option<operands::Register> {
        if self.current == u8::MAX {
            return None;
        }

        let reg = self.current;
        self.current += 1;
        self.total = std::cmp::max(self.current, self.total);
        Some(reg)
    }

    fn free(&mut self, reg: operands::Register) {
        assert!(self.current >= reg, "registers freed out of order");
        self.current = reg;
    }
}

#[derive(Clone, Copy, Debug)]
enum Symbol {
    Extern(operands::ExternFunctionId),
    Fn(operands::FunctionId),
    Var(TyReg),
}

pub struct Library {
    functions: Cow<'static, [ExternFunction]>,
}

impl Library {
    pub fn new() -> Self {
        Self {
            functions: Cow::Owned(Vec::new()),
        }
    }

    /// ### Safety
    /// - All host function signatures must match their callbacks' signatures
    pub const unsafe fn from_static(functions: &'static [ExternFunction]) -> Self {
        Self {
            functions: Cow::Borrowed(functions),
        }
    }

    /// ### Safety
    /// - The signature must match the callback's signature
    pub unsafe fn add(mut self, function: ExternFunction) -> Self {
        let mut functions = self.functions.into_owned();
        functions.push(function);
        self.functions = Cow::Owned(functions);
        self
    }
}

pub struct FnTable {
    entry: FunctionId,

    function_map: BTreeMap<String, FunctionId>,
    functions: Vec<Function>,

    external_function_map: BTreeMap<String, ExternFunctionId>,
    external_functions: Vec<ExternFunctionAbi>,
}

struct FnTableBuilder {
    next_function_id: u16,
    next_external_function_id: u16,

    table: FnTable,
    empty_fn: Function,
}

impl FnTableBuilder {
    fn new() -> Self {
        let empty_fn = Function {
            name: "<empty>".into(),
            params: 0,
            bytecode: default(),
            spans: default(),
            literals: default(),
            registers: 0,
        };
        Self {
            next_function_id: 0,
            next_external_function_id: 0,

            table: FnTable {
                entry: u16::MAX,

                function_map: default(),
                functions: default(),

                external_function_map: default(),
                external_functions: default(),
            },
            empty_fn,
        }
    }

    fn get_function(&self, id: FunctionId) -> Option<&Function> {
        self.table.functions.get(id as usize)
    }

    fn get_function_id(&self, name: &str) -> Option<FunctionId> {
        self.table.function_map.get(name).copied()
    }

    fn get_external_function_abi(&self, id: ExternFunctionId) -> Option<&ExternFunctionAbi> {
        self.table.external_functions.get(id as usize)
    }

    fn get_external_function_id(&self, name: &str) -> Option<ExternFunctionId> {
        self.table.external_function_map.get(name).copied()
    }

    fn reserve_function(&mut self, name: Ident<'_>) -> FunctionId {
        let id = self.next_function_id;
        self.next_function_id += 1;

        self.table.function_map.insert(name.to_string(), id);
        self.table.functions.push(self.empty_fn.clone());

        id
    }

    fn set_function(&mut self, id: FunctionId, fn_: Function) {
        self.table.functions[id as usize] = fn_;
    }

    fn reserve_external_function(&mut self, name: String, abi: ExternFunctionAbi) {
        let id = self.next_external_function_id;
        self.next_external_function_id += 1;
        self.table.external_function_map.insert(name, id);
        self.table.external_functions.push(abi);
    }

    fn finish(self) -> FnTable {
        self.table
    }
}

#[derive(Default, Clone)]
pub struct Function {
    pub name: String,
    pub params: u8,
    pub bytecode: Vec<Instruction>,
    pub spans: Vec<Span>,
    // TODO: literals should be stored per-module
    pub literals: Vec<Literal>,
    pub registers: u8,
}

impl Function {
    pub(crate) fn finish(self) -> vm::Function {
        // TODO: retain more of those fields
        vm::Function::new(self.name, self.bytecode, self.literals, self.registers)
    }

    #[cfg(test)]
    #[doc(hidden)]
    pub fn test(
        name: impl Into<String>,
        bytecode: Vec<Instruction>,
        literals: Vec<Literal>,
        registers: u8,
    ) -> Self {
        Self {
            name: name.into(),
            params: 0,
            bytecode,
            spans: vec![],
            literals,
            registers,
        }
    }
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
mod tests;
