use std::collections::BTreeMap;
use std::fmt::{Debug, Display};

use crate::ast::Ident;
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
            fn_table: FunctionTable::new(),
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
        for (_, hir_id, _) in hir.fns.iter() {
            self.module_state.fn_table.reserve(hir_id)
        }

        // 2. compile top-level code
        let top_level = generate_main_fn(hir.top_level);
        match FunctionState::new(&mut self, &top_level).compile() {
            Ok(fn_) => self.module_state.fn_table.main = fn_,
            Err(e) => self.ecx.push(e),
        }

        // 3. compile all functions, place them into their respective slots
        for (_, hir_id, fn_) in hir.fns.iter() {
            match FunctionState::new(&mut self, fn_).compile() {
                Ok(fn_) => self.module_state.fn_table.relocate(hir_id, fn_),
                Err(e) => self.ecx.push(e),
            }
        }

        self.ecx.finish()?;

        Ok(Module {
            fn_table: self.module_state.fn_table,
        })
    }
}

struct ModuleState<'src> {
    vars: HashMap<Ident<'src>, Mvar>,
    fn_table: FunctionTable,
}

struct FunctionState<'src, 'a> {
    compiler: &'a mut Compiler<'src>,
    hir: &'a hir::Fn<'src>,
    current_loop: Option<LoopState>,
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

impl<'src, 'a> FunctionState<'src, 'a> {
    fn new(compiler: &'a mut Compiler<'src>, hir: &'a hir::Fn<'src>) -> Self {
        Self {
            compiler,
            hir,
            current_loop: None,
            asm: Assembler::new(),
            regalloc: RegisterAllocator::new(),
            locals: Vec::new(),
        }
    }

    fn compile(mut self) -> Result<Function> {
        use asm::*;

        let hir::FnBody::Block(body) = &self.hir.body else {
            return Ok(Function::default());
        };

        let val = self.alloc_reg(Span::empty())?;
        block(&mut self, body, Some(val))?;

        self.asm.emit(Span::empty(), ret(val));

        let (bytecode, spans, constants) = self.asm.finish();
        let registers = self.regalloc.total;
        Ok(Function {
            name: self.hir.name.to_string(),
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

    fn resolve_function(&self, name: &str) -> Option<FnId> {
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

    fn alloc_const_int(&mut self, span: Span, v: i64) -> Result<Cst> {
        self.asm
            .constants
            .insert_int(v)
            .map(Cst::new)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
    }

    fn alloc_const_num(&mut self, span: Span, v: f64n) -> Result<Cst> {
        self.asm
            .constants
            .insert_num(v)
            .map(Cst::new)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
    }

    fn alloc_const_str(&mut self, span: Span, v: Str<'src>) -> Result<Cst> {
        self.asm
            .constants
            .insert_str(v)
            .map(Cst::new)
            .ok_or_else(|| self.compiler.ecx.too_many_constants(span))
    }
}

fn scope<'src, 'a, T>(
    f: &mut FunctionState<'src, 'a>,
    inner: impl FnOnce(&mut FunctionState<'src, 'a>) -> T,
) -> T {
    f.locals.push(HashMap::default());
    let r = inner(f);
    f.locals.pop().unwrap();
    r
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
    todo!("loop_stmt")
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
        hir::ExprKind::Return(hir) => todo!(),
        hir::ExprKind::Break(hir) => todo!(),
        hir::ExprKind::Continue(hir) => todo!(),
        hir::ExprKind::Block(hir) => todo!(),
        hir::ExprKind::If(hir) => todo!(),
        hir::ExprKind::Binary(hir) => binary_expr(f, span, hir, dst),
        hir::ExprKind::Unary(hir) => todo!(),
        hir::ExprKind::Primitive(hir) => primitive_expr(f, span, hir, dst),
        hir::ExprKind::UseVar(hir) => use_var_expr(f, span, hir, dst),
        hir::ExprKind::UseField(hir) => todo!(),
        hir::ExprKind::UseIndex(hir) => todo!(),
        hir::ExprKind::AssignVar(hir) => todo!(),
        hir::ExprKind::AssignField(hir) => todo!(),
        hir::ExprKind::AssignIndex(hir) => todo!(),
        hir::ExprKind::Call(hir) => call_expr(f, span, hir, dst),
    }
}

fn binary_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Binary<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    todo!()
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
        hir::Primitive::Bool(v) => match v {
            true => f.asm.emit(span, load_true(dst)),
            false => f.asm.emit(span, load_false(dst)),
        },
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
    let symbol = f.resolve_symbol(hir.name);
    match symbol.kind {
        SymbolKind::Fn(v) => todo!("use function as value"),
        SymbolKind::Mvar(v) => todo!("module variables"),
        SymbolKind::Var(v) => Ok(Some(v)),
    }
}

fn call_expr<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    span: Span,
    hir: &'a hir::Call<'src>,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    todo!()
}

fn block<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    hir: &'a hir::Block<'src>,
    dst: Option<Reg>,
) -> Result<()> {
    use asm::*;

    scope(f, |f| {
        for v in &hir.body {
            stmt(f, v)?;
        }

        match (&hir.tail, dst) {
            (Some(tail), Some(dst)) => {
                assign_to(f, tail, dst)?;
            }
            (Some(tail), None) => {
                expr(f, tail, None)?;
            }
            (None, Some(dst)) => {
                f.asm.emit(Span::empty(), load_unit(dst));
            }
            (None, None) => {}
        }

        Ok(())
    })
}

fn assign_to<'src, 'a>(
    f: &mut FunctionState<'src, 'a>,
    value: &'a hir::Expr<'src>,
    dst: Reg,
) -> Result<()> {
    use asm::*;

    match expr(f, value, Some(dst))? {
        Some(out) => {
            // `expr` was written to `out`
            f.asm.emit(value.span, mov(out, dst));
        }
        _ => {
            // `expr` was written to `dst`
        }
    }

    Ok(())
}

struct LoopState {
    start: MultiLabel,
    end: MultiLabel,
}

struct Assembler<'src> {
    bytecode: Vec<u8>,
    spans: Vec<Span>,
    constants: ConstantPoolBuilder<'src>,
}

impl<'src> Assembler<'src> {
    fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            spans: Vec::new(),
            constants: ConstantPoolBuilder::new(),
        }
    }

    fn emit(&mut self, span: Span, e: impl Encode) {
        e.encode(&mut self.bytecode);
        self.spans.push(span);
    }

    fn finish(mut self) -> (Vec<u8>, Vec<Span>, Vec<Constant>) {
        if !self.bytecode.ends_with(&[Op::Stop as u8]) {
            self.emit(Span::empty(), asm::stop());
        }

        (self.bytecode, self.spans, self.constants.finish())
    }
}

struct Label {
    referrer: Option<usize>,
}

struct MultiLabel {
    referrers: Vec<usize>,
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
        Some(Reg::new(reg))
    }

    fn free(&mut self, reg: Reg) {
        assert!(
            self.current == reg.get() + 1,
            "registers freed out of order"
        );
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
    Fn(FnId),
    Mvar(Mvar),
    Var(Reg),
}

impl Symbol {
    fn fn_(span: Span, id: FnId) -> Self {
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
    fn_table: FunctionTable,
}

struct FunctionTable {
    next_id: u16,
    name_to_id: BTreeMap<String, FnId>,
    reloc_table: BTreeMap<hir::FnId, FnId>,
    main: Function,
    fns: Vec<FnTableEntry>,
}

impl FunctionTable {
    fn new() -> Self {
        Self {
            next_id: 0,
            name_to_id: BTreeMap::new(),
            reloc_table: BTreeMap::new(),
            main: Function::default(),
            fns: Vec::new(),
        }
    }

    fn get_id(&self, name: &str) -> Option<FnId> {
        self.name_to_id.get(name).copied()
    }

    fn get(&self, id: FnId) -> Option<&FnTableEntry> {
        self.fns.get(id.to_index())
    }

    fn reserve(&mut self, hir_id: hir::FnId) {
        let table_id = FnId::new(self.next_id);
        self.next_id += 1;

        self.reloc_table.insert(hir_id, table_id);
        self.fns.push(FnTableEntry::Script(Function::default()));
    }

    fn relocate(&mut self, hir_id: hir::FnId, fn_: Function) {
        self.fns[self.reloc_table[&hir_id].to_index()] = FnTableEntry::Script(fn_);
    }
}

enum FnTableEntry {
    Script(Function),
    Host(ExternFunction),
}

#[derive(Default)]
struct Function {
    // captures: Vec<Capture>,
    name: String,
    bytecode: Vec<u8>,
    spans: Vec<Span>,
    constants: Vec<Constant>,
    registers: u8,
}

struct ExternFunction {
    // TODO: type info
}

/* enum Capture {
    NonLocal(Reg),
    Parent(Cap),
} */

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", PrintFunction(&self.fn_table.main, None))?;
        for fn_ in &self.fn_table.fns {
            let FnTableEntry::Script(fn_) = fn_ else {
                continue;
            };
            writeln!(f, "{}", PrintFunction(fn_, None))?;
        }
        Ok(())
    }
}

impl Module {
    pub fn with_src<'a, 'src>(&'a self, src: &'src str) -> ModuleWithSrc<'a, 'src> {
        ModuleWithSrc(self, src)
    }
}

pub struct ModuleWithSrc<'a, 'src>(&'a Module, &'src str);
impl<'a, 'src> Display for ModuleWithSrc<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ModuleWithSrc(m, src) = self;

        writeln!(f, "{}", PrintFunction(&m.fn_table.main, Some(src)))?;
        for fn_ in &m.fn_table.fns {
            let FnTableEntry::Script(fn_) = fn_ else {
                continue;
            };
            writeln!(f, "{}", PrintFunction(fn_, Some(src)))?;
        }
        Ok(())
    }
}

struct PrintFunction<'a, 'src>(&'a Function, Option<&'src str>);
impl<'a, 'src> Display for PrintFunction<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use op::Decode as _;

        let fn_ = self.0;

        writeln!(f, "function {:?} {{", fn_.name)?;
        write!(f, "  constants: ")?;
        if fn_.constants.is_empty() {
            writeln!(f, "[]")?;
        } else {
            write!(f, "[")?;
            for c in &fn_.constants {
                writeln!(f, "    {c:?}")?;
            }
            writeln!(f, "  ]")?;
        }

        writeln!(f, "  bytecode: [")?;

        // collect instructions so that we know the length of each line
        // we _could_ calculate this on the fly, but that would be way
        // more code...
        let offset_width = crate::util::num_digits(fn_.bytecode.len());
        let mut cursor = std::io::Cursor::new(&fn_.bytecode[..]);
        let mut spans = fn_.spans.iter();
        let mut instructions = Vec::new();
        while cursor.position() < cursor.get_ref().len() as u64 {
            let offset = cursor.position();
            let instruction = unsafe { symbolic::Instruction::decode_unchecked(&mut cursor) };
            let span = spans.next().unwrap();
            instructions.push((*span, format!("    {offset:offset_width$}  {instruction}")));
        }

        let max_len = instructions.iter().map(|(_, s)| s.len()).max().unwrap_or(0);
        let mut prev_span = Span::empty();
        for (span, instruction) in instructions {
            'next: {
                if let Some(src) = self.1 {
                    write!(f, "{instruction:max_len$}  // ")?;

                    let temp = prev_span;
                    prev_span = span;
                    if temp == span || span.is_empty() {
                        break 'next;
                    }

                    let loc = Location::from_source_span(src, &span);
                    f.write_str(src[loc.line_span()].trim())?;
                } else {
                    write!(f, "{instruction}")?;
                }
            }
            f.write_str("\n")?;
        }

        writeln!(f, "  ")?;
        writeln!(f, "  ]")?;
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod tests;
