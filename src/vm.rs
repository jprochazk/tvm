mod dyn_array;
pub mod operands;
pub mod value;

use core::ptr::addr_of_mut as mut_;
use std::borrow::Cow;
use std::boxed::Box;
use std::marker::PhantomData;

use dyn_array::{DynArray, DynList};
use operands::{
    u24, ExternFunctionId, FunctionId, JoinOperands, LiteralId, PcRelativeOffset, Register,
    SplitOperands,
};
use value::{Literal, Value};

use crate::hir;

pub fn dispatch(context: &mut Vm, module: &Module) -> Result<(), Error> {
    let mut current_frame = CallFrame {
        callee: &mut Function::new(
            "[entry]",
            vec![
                operands::encode(Opcode::Call, (0u8, module.entry)),
                operands::encode(Opcode::End, ()),
            ],
            vec![],
            1,
        ),
        stack_base: 0,
        return_addr: 1,
    };

    let mut error = None;
    let mut local_context = LocalContext {
        functions: module.functions.as_ptr(),
        external_functions: module.external_functions.as_ptr(),
        vm: context,
        current_frame: &mut current_frame,
        error: &mut error,
    };

    #[cfg(debug_assertions)]
    unsafe {
        let callee = &*current_frame.callee;

        let ops = OPS.as_ptr().cast::<()>();
        let mut code = Function::code_ptr(callee);
        let mut stack = context.stack.offset(0);
        let mut literals = Function::literals_ptr(callee);
        let context = &mut local_context as *mut _;

        let (mut opcode, mut operands) = operands::decode(code.read());
        loop {
            let op = ops.cast::<Op>().add(opcode as usize).read();
            match (op)(operands, ops, code, stack, literals, context) {
                Control::End => return Ok(()),
                Control::Error => return Err(error.unwrap()),
                Control::Continue(
                    next_opcode,
                    next_operands,
                    new_code_ptr,
                    new_stack_ptr,
                    new_literals_ptr,
                ) => {
                    opcode = next_opcode;
                    operands = next_operands;
                    code = new_code_ptr;
                    stack = new_stack_ptr;
                    literals = new_literals_ptr;
                }
            }
        }
    }

    #[cfg(not(debug_assertions))]
    unsafe {
        let callee = &*current_frame.callee;

        match dispatch_current(
            (&OPS) as *const [Op; 256] as *const (),
            Function::code_ptr(callee),
            context.stack.offset(0),
            Function::literals_ptr(callee),
            &mut local_context,
        ) {
            // Control::Pause => todo!(),
            Control::End => Ok(()),
            Control::Error => Err(error.unwrap()),
        }
    }
}

#[repr(usize)]
#[derive(Clone, Copy)]
enum Control {
    // Pause,
    End,
    Error,

    #[cfg(debug_assertions)]
    Continue(
        RawOpcode,
        u24,
        *const Instruction,
        *mut StackSlot,
        *const Literal,
    ),
}

#[repr(transparent)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instruction(pub(crate) u32);

impl Instruction {
    pub fn encode(opcode: Opcode, operands: impl JoinOperands) -> Self {
        operands::encode(opcode, operands)
    }

    pub fn decode(self) -> DecodedInstruction {
        let (opcode, operands) = operands::decode(self);
        DecodedInstruction::from((opcode, operands))
    }

    pub fn opcode(self) -> RawOpcode {
        operands::decode(self).0
    }
}

impl core::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(&Disasm(*self), f)
    }
}

pub struct Function {
    name: Cow<'static, str>,
    code: *const [Instruction],
    literals: *const [Literal],
    // TODO: store this as pointer tag
    registers: u8,
}

impl Function {
    #[inline]
    pub fn new(
        name: impl Into<Cow<'static, str>>,
        code: Vec<Instruction>,
        literals: Vec<Literal>,
        registers: u8,
    ) -> Self {
        Self {
            name: name.into(),
            code: Box::into_raw(code.into_boxed_slice()),
            literals: Box::into_raw(literals.into_boxed_slice()),
            registers,
        }
    }

    #[inline]
    unsafe fn code_ptr(this: *const Function) -> *const Instruction {
        (*this).code as *const Instruction
    }

    #[inline]
    unsafe fn literals_ptr(this: *const Function) -> *const Literal {
        (*this).literals as *const Literal
    }
}

unsafe impl Send for Function {}
unsafe impl Sync for Function {}

impl Drop for Function {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.code.cast_mut()));
            drop(Box::from_raw(self.literals.cast_mut()));
        }
    }
}

pub trait TryIntoValue: Sized {
    fn try_into_value(self) -> std::result::Result<Value, ExternFunctionError>;
}

pub trait TryFromValue: Sized {
    fn try_from_value(value: Value) -> std::result::Result<Self, ExternFunctionError>;
}

impl<T: Into<Value>> TryIntoValue for Result<T, ExternFunctionError> {
    #[inline]
    fn try_into_value(self) -> std::result::Result<Value, ExternFunctionError> {
        match self {
            Ok(value) => Ok(value.into()),
            Err(e) => Err(e),
        }
    }
}

impl<T: Into<Value>> TryIntoValue for T {
    #[inline]
    fn try_into_value(self) -> std::result::Result<Value, ExternFunctionError> {
        Ok(self.into())
    }
}

pub trait ValueAbi {
    const TYPE: hir::Ty;
}

pub trait ExternFunctionCallback<Ret, Args> {
    unsafe fn call(&self, scope: Scope<'_>) -> std::result::Result<Value, ExternFunctionError>;
}

pub trait ExternFunctionMetadata<Ret, Args> {
    const ABI: ExternFunctionAbi;
}

macro_rules! impl_extern_function_traits {
    ($($arg:ident),* $(,)?) => {
        impl<Func, Ret, $($arg),*> ExternFunctionCallback<Ret, ($($arg,)*)> for Func
        where
            Func: for<'a> Fn(Scope<'a>, $($arg),*) -> Ret,
            $(
                $arg: TryFromValue,
            )*
            Ret: TryIntoValue,
        {
            #[inline]
            unsafe fn call(&self, scope: Scope<'_>)  -> std::result::Result<Value, ExternFunctionError> {
                #![allow(unused_variables, unused_mut, non_snake_case, unused_assignments)]

                let mut index = 0;
                $(
                    let $arg = <_>::try_from_value(scope.get_arg(index))?;
                    index += 1;
                )*

                (self)(scope.clone(), $($arg),*).try_into_value()
            }
        }

        impl<Func, Ret, $($arg),*> ExternFunctionMetadata<Ret, ($($arg,)*)> for Func
        where
            Func: for<'a> Fn(Scope<'a>, $($arg),*) -> Ret,
            $(
                $arg: ValueAbi,
            )*
            Ret: ValueAbi,
        {
            const ABI: ExternFunctionAbi = ExternFunctionAbi {
                params: std::borrow::Cow::Borrowed(&[$(<$arg as ValueAbi>::TYPE),*]),
                ret: <Ret as ValueAbi>::TYPE,
            };
        }
    };
}

impl_extern_function_traits!();
impl_extern_function_traits!(A);
impl_extern_function_traits!(A, B);
impl_extern_function_traits!(A, B, C);
impl_extern_function_traits!(A, B, C, D);
impl_extern_function_traits!(A, B, C, D, E);
impl_extern_function_traits!(A, B, C, D, E, F);
impl_extern_function_traits!(A, B, C, D, E, F, G);
impl_extern_function_traits!(A, B, C, D, E, F, G, H);

pub struct Scope<'a> {
    ops: *const (),
    stack: *mut StackSlot,
    context: *mut LocalContext,

    lifetime: PhantomData<fn(&'a ()) -> &'a ()>,
}

impl<'a> Scope<'a> {
    unsafe fn get_arg(&self, index: usize) -> Value {
        // first arg at `stack_base+1`
        self.stack.add(index + 1).read()
    }

    unsafe fn set_ret<T: Into<Value>>(&self, value: T) {
        // ret at `stack_base`
        self.stack.write(value.into());
    }

    unsafe fn set_error(&self, error: Error) {
        (*self.context).error.write(Some(error));
    }

    fn clone(&self) -> Self {
        Self {
            ops: self.ops,
            stack: self.stack,
            context: self.context,
            lifetime: PhantomData,
        }
    }
}

// TODO: proper error
pub type ExternFunctionError = ();

pub type ExternFunctionCallbackWrapper =
    for<'a> unsafe fn(Scope<'a>) -> std::result::Result<Value, ExternFunctionError>;

#[derive(Clone)]
pub struct ExternFunctionAbi {
    pub params: Cow<'static, [hir::Ty]>,
    pub ret: hir::Ty,
}

#[derive(Clone)]
#[non_exhaustive]
pub struct ExternFunction {
    pub name: &'static str,
    pub abi: ExternFunctionAbi,
    wrapper: ExternFunctionCallbackWrapper,
}

impl ExternFunction {
    #[doc(hidden)]
    pub const fn __internal_new<
        F: ExternFunctionCallback<Ret, Args> + ExternFunctionMetadata<Ret, Args> + 'static,
        Ret,
        Args,
    >(
        _: &F,
        name: &'static str,
        wrapper: ExternFunctionCallbackWrapper,
    ) -> Self {
        Self {
            name,
            abi: <F>::ABI,
            wrapper,
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __extern_function {
    ($inner:ident) => {{
        $crate::__extern_function!(stringify!($inner), $inner)
    }};
    ($name:expr, $inner:expr) => {{
        unsafe fn _wrapper(
            scope: $crate::vm::Scope<'_>,
        ) -> std::result::Result<$crate::vm::value::Value, $crate::vm::ExternFunctionError>
        {
            $crate::vm::ExternFunctionCallback::call(&$inner, scope)
        }

        const { $crate::vm::ExternFunction::__internal_new(&$inner, $name, _wrapper) }
    }};
}

pub use crate::__extern_function as function;
use crate::util::JoinIter;

#[derive(Clone, Copy)]
#[repr(C)]
struct CallFrame {
    callee: *const Function,
    stack_base: u32,
    return_addr: u32,
}

impl Default for CallFrame {
    fn default() -> Self {
        static INVALID_FUNCTION: Function = Function {
            name: Cow::Borrowed("invalid"),
            code: &[asm::invalid_op()],
            literals: &[],
            registers: 1,
        };

        Self {
            callee: &INVALID_FUNCTION,
            stack_base: 0,
            return_addr: 0,
        }
    }
}

impl core::fmt::Debug for CallFrame {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("CallFrame")
            .field("stack_base", &self.stack_base)
            .field("return_addr", &self.return_addr)
            .finish()
    }
}

pub type StackSlot = Value;

pub struct Module {
    // TODO: make `Function` and `ExternFunction` smaller structs
    pub(crate) entry: FunctionId,
    pub(crate) functions: Vec<Function>,
    pub(crate) external_functions: Vec<ExternFunction>,
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for function in &self.functions {
            writeln!(f, "fn {}", function.name)?;
            writeln!(f, "  registers: {}", function.registers)?;
            writeln!(f, "  literals: {:?}", unsafe { &*function.literals })?;
            writeln!(f, "  bytecode:")?;
            unsafe {
                for instruction in &*function.code {
                    writeln!(f, "    {}", Disasm(*instruction))?;
                }
            }
        }

        for function in &self.external_functions {
            writeln!(f, "extern fn {};", function.name)?;
        }

        Ok(())
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __extern_library {
    ($name:ident: $($function:ident),* $(,)?) => {
        const FUNCTIONS: &'static [$crate::vm::ExternFunction] = &[$($crate::__extern_function!($function)),*];
        const $name: $crate::code::Library = const { unsafe { $crate::code::Library::from_static(FUNCTIONS) } };
    };
}

pub use crate::__extern_library as library;

pub struct Vm {
    stack: DynArray<StackSlot>,
    frames: DynList<CallFrame>,
}

impl Vm {
    pub fn new() -> Self {
        Self::builder().build()
    }

    // 16 pages worth of `Value`
    pub const DEFAULT_INITIAL_STACK_SIZE: usize = 16 * 4096 / core::mem::size_of::<Value>();

    pub fn builder() -> VmBuilder {
        VmBuilder {
            initial_stack_size: Self::DEFAULT_INITIAL_STACK_SIZE,
        }
    }

    pub fn run(&mut self, module: &Module) -> Result<Value, Error> {
        dispatch(self, module).map(|_| self.ret())
    }

    pub fn ret(&self) -> Value {
        unsafe { self.stack.offset(0).read() }
    }

    #[doc(hidden)]
    pub fn stack_size(&self) -> usize {
        self.stack.capacity()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::builder().build()
    }
}

pub struct VmBuilder {
    initial_stack_size: usize,
}

impl VmBuilder {
    /// `initial_stack_size` will be rounded to the nearest power of two,
    /// and it may be no smaller than `2`.
    pub fn initial_stack_size(mut self, initial_stack_size: usize) -> Self {
        self.initial_stack_size = std::cmp::max(2, initial_stack_size.next_power_of_two());
        self
    }

    pub fn build(self) -> Vm {
        let stack = DynArray::new(self.initial_stack_size);
        // ~8 registers per function call,
        // and at least 2 for the trampoline + entrypoint.
        let frames = DynList::new(std::cmp::max(2, self.initial_stack_size / 8));

        Vm { stack, frames }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    InvalidOp,
    // TODO: proper error
    Extern(ExternFunctionError),
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Error::InvalidOp => write!(f, "invalid op"),
            // TODO: proper error
            Error::Extern(e) => write!(f, "extern function error"),
        }
    }
}

impl core::error::Error for Error {}

struct LocalContext {
    functions: *const Function,
    external_functions: *const ExternFunction,
    vm: *mut Vm,
    current_frame: *mut CallFrame,
    error: *mut Option<Error>,
}

unsafe fn get_function_unchecked(context: *mut LocalContext, index: FunctionId) -> *const Function {
    (*context).functions.add(index as usize)
}

unsafe fn get_extern_function_unchecked(
    context: *mut LocalContext,
    index: ExternFunctionId,
) -> *const ExternFunction {
    (*context).external_functions.add(index as usize)
}

#[cfg(not(windows))]
type Op = unsafe extern "C" fn(
    operands: u24,
    ops: *const (),
    code: *const Instruction,
    stack: *mut StackSlot,
    literals: *const Literal,
    context: *mut LocalContext,
) -> Control;

#[cfg(not(windows))]
macro_rules! wrap_abi {
    (unsafe fn $name:ident(
        $($i:ident : $ty:ty),* $(,)?
    ) -> $ret:ty $body:block) => {
        unsafe extern "C" fn $name(
            $($i: $ty),*
        ) -> $ret $body
    };
}

#[cfg(windows)]
pub type Op = unsafe extern "sysv64" fn(
    operands: u24,
    ops: *const (),
    code: *const Instruction,
    stack: *mut Stack,
    literals: *const Literal,
    context: *mut LocalContext,
) -> Control;

#[cfg(windows)]
macro_rules! wrap_abi {
    (unsafe fn $name:ident(
        $($i:ident : $ty:ty),* $(,)?
    ) -> $ret:ty $body:block) => {
        unsafe extern "sysv64" fn $name(
            $($i: $ty),*
        ) -> $ret $body
    };
}

/// Dispatch the current instruction.
#[inline(always)]
unsafe fn dispatch_current(
    ops: *const (),
    code: *const Instruction,
    stack: *mut StackSlot,
    literals: *const Literal,
    context: *mut LocalContext,
) -> Control {
    let (opcode, operands) = operands::decode(code.read());

    #[cfg(debug_assertions)]
    {
        let _ = (ops, context);
        Control::Continue(opcode, operands, code, stack, literals)
    }

    #[cfg(not(debug_assertions))]
    {
        let op = ops.cast::<Op>().add(opcode as usize).read();
        (op)(operands, ops, code, stack, literals, context)
    }
}

/// Increment `code` by one instruction and dispatch.
#[inline(always)]
unsafe fn dispatch_next(
    ops: *const (),
    code: *const Instruction,
    stack: *mut StackSlot,
    literals: *const Literal,
    context: *mut LocalContext,
) -> Control {
    let code = code.add(1);
    dispatch_current(ops, code, stack, literals, context)
}

wrap_abi! {
    unsafe fn invalid_op(
        _operands: u24,
        _ops: *const (),
        _code: *const Instruction,
        _stack: *mut StackSlot,
        _literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        mut_!(*(*context).error).write(Some(Error::InvalidOp));
        Control::Error
    }
}

wrap_abi! {
    unsafe fn mov(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst, src): info::mov = operands.split();

        let value = stack.add(src as usize).read();
        stack.add(dst as usize).write(value);

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_unit(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst,): info::load_unit = operands.split();

        stack.add(dst as usize).write(Value::Unit);

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_literal(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst, src): info::load_literal = operands.split();

        let literal: Literal = literals.add(src as usize).read();
        stack.add(dst as usize).write(literal.into_value());

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_i16(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst, value): info::load_i16 = operands.split();

        // println!("load_i64 dst={dst} src={src}");

        stack.add(dst as usize).write(Value::I64(value as i64));

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_true(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst,): info::load_true = operands.split();

        stack.add(dst as usize).write(Value::Bool(true));

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_false(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst,): info::load_false = operands.split();

        stack.add(dst as usize).write(Value::Bool(false));

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_fn(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (_dst, _id): info::load_fn = operands.split();

        todo!();
    }
}

wrap_abi! {
    unsafe fn load_fn_extern(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (_dst, _id): info::load_fn_extern = operands.split();

        todo!();
    }
}

wrap_abi! {
    unsafe fn jump(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (offset,): info::jump = operands.split();

        let code = code.offset(offset as isize);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump_long(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (offset,): info::jump_long = operands.split();
        let offset = literals.add(offset as usize).read().jump_offset_unchecked();

        let code = code.offset(offset);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump_if_false(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (condition, offset): info::jump_if_false = operands.split();
        let condition = stack.add(condition as usize).read();

        let code = if !condition.bool_unchecked() {
            code.offset(offset as isize)
        } else {
            code.add(1)
        };

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump_if_false_long(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (condition, offset): info::jump_if_false_long = operands.split();
        let condition = stack.add(condition as usize).read();
        let offset = literals.add(offset as usize).read().jump_offset_unchecked();

        let code = if !condition.bool_unchecked() {
            code.offset(offset)
        } else {
            code.add(1)
        };

        dispatch_current(ops, code, stack, literals, context)
    }
}

macro_rules! arithmetic {
    ($name:ident, $ty:ident, $op:tt) => {
        wrap_abi! {
            unsafe fn $name(
                operands: u24,
                ops: *const (),
                code: *const Instruction,
                stack: *mut StackSlot,
                literals: *const Literal,
                context: *mut LocalContext,
            ) -> Control {
                let (dst, lhs, rhs): info::$name = operands.split();

                let lhs = stack.add(lhs as usize).read().$ty();
                let rhs = stack.add(rhs as usize).read().$ty();
                let result = lhs $op rhs;

                stack.add(dst as usize).write(Value::from(result));

                dispatch_next(ops, code, stack, literals, context)
            }
        }
    };
}

arithmetic!(add_i64, i64_unchecked, +);
arithmetic!(add_f64, f64_unchecked, +);
arithmetic!(sub_i64, i64_unchecked, -);
arithmetic!(sub_f64, f64_unchecked, -);
arithmetic!(mul_i64, i64_unchecked, *);
arithmetic!(mul_f64, f64_unchecked, *);
arithmetic!(div_i64, i64_unchecked, /);
arithmetic!(div_f64, f64_unchecked, /);
arithmetic!(rem_i64, i64_unchecked, %);
arithmetic!(rem_f64, f64_unchecked, %);

macro_rules! comparison {
    ($name:ident, $ty:ident, $op:tt) => {
        wrap_abi! {
            unsafe fn $name(
                operands: u24,
                ops: *const (),
                code: *const Instruction,
                stack: *mut StackSlot,
                literals: *const Literal,
                context: *mut LocalContext,
            ) -> Control {
                let (dst, lhs, rhs): info::$name = operands.split();

                let lhs = stack.add(lhs as usize).read().$ty();
                let rhs = stack.add(rhs as usize).read().$ty();
                let result = lhs $op rhs;

                stack.add(dst as usize).write(Value::Bool(result));

                dispatch_next(ops, code, stack, literals, context)
            }
        }
    };
}

comparison!(cmp_eq_i64, i64_unchecked, ==);
comparison!(cmp_eq_f64, f64_unchecked, ==);
comparison!(cmp_eq_bool, bool_unchecked, ==);
comparison!(cmp_ne_i64, i64_unchecked, !=);
comparison!(cmp_ne_f64, f64_unchecked, !=);
comparison!(cmp_ne_bool, bool_unchecked, !=);
comparison!(cmp_gt_i64, i64_unchecked, >);
comparison!(cmp_gt_f64, f64_unchecked, >);
comparison!(cmp_lt_i64, i64_unchecked, <);
comparison!(cmp_lt_f64, f64_unchecked, <);
comparison!(cmp_ge_i64, i64_unchecked, >=);
comparison!(cmp_ge_f64, f64_unchecked, >=);
comparison!(cmp_le_i64, i64_unchecked, <=);
comparison!(cmp_le_f64, f64_unchecked, <=);

macro_rules! negate {
    ($name:ident, $ty:ident) => {
        wrap_abi! {
            unsafe fn $name(
                operands: u24,
                ops: *const (),
                code: *const Instruction,
                stack: *mut StackSlot,
                literals: *const Literal,
                context: *mut LocalContext,
            ) -> Control {
                let (dst, rhs): info::$name = operands.split();

                let rhs = stack.add(rhs as usize).read().$ty();
                let result = -rhs;
                stack.add(dst as usize).write(Value::from(result));

                dispatch_next(ops, code, stack, literals, context)
            }
        }
    };
}

negate!(neg_i64, i64_unchecked);
negate!(neg_f64, f64_unchecked);

wrap_abi! {
    unsafe fn not_bool(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst, rhs): info::not_bool = operands.split();

        let rhs = stack.add(rhs as usize).read().bool_unchecked();
        let result = !rhs;
        stack.add(dst as usize).write(Value::Bool(result));

        dispatch_next(ops, code, stack, literals, context)
    }
}

// call procedure:
// 1. grow stack if needed
// 2. allocate new call frame
// 3. jump to start of callee
//
// new call frame's stack overlaps with the current frame's stack
// example: assuming 3 args, with ret at r6:
//   frame N:   [ 0 1 2 3 4 5 6 7 8 9 ]
//                            ^ ^
//                            | args
//                            ret
//   frame N+1: [ 6 7 8 9 ... ]
//                ^ ^
//                | args
//                ret
// `r0` in the new frame will be in the same location as `r6`
// in the previous frame.
unsafe fn prepare_call(
    callee: *const Function,
    ret: Register,
    code_ptr: *const Instruction,
    context_ptr: *mut LocalContext,
) -> (*const Instruction, *mut StackSlot, *const Literal) {
    let context = &mut *context_ptr;
    let current_frame = &mut *context.current_frame;
    let vm = &mut *context.vm;

    let current_code_addr = code_ptr.offset_from(Function::code_ptr(current_frame.callee)) as u32;

    let new_frame = CallFrame {
        callee,
        stack_base: current_frame.stack_base + ret as u32,
        return_addr: current_code_addr + 1,
    };

    // grow the stack if needed
    let stack_ptr = grow_stack(
        vm,
        new_frame.stack_base as usize,
        (*callee).registers as usize,
    );

    // allocate new call frame
    vm.frames.push(core::mem::replace(current_frame, new_frame));

    // jump to start of callee
    // -> replacing the code ptr is equivalent to a jump
    let code_ptr = Function::code_ptr(callee);
    let literals_ptr = Function::literals_ptr(callee);

    (code_ptr, stack_ptr, literals_ptr)
}

#[inline]
unsafe fn grow_stack(
    vm: &mut Vm,
    new_stack_base: usize,
    required_stack_space: usize,
) -> *mut StackSlot {
    let remaining_stack_space = vm.stack.remaining(new_stack_base);
    if remaining_stack_space < required_stack_space {
        vm.stack.grow(required_stack_space - remaining_stack_space);
        vm.stack.offset(new_stack_base)
    } else {
        vm.stack.offset(new_stack_base)
        // stack_ptr.add(required_stack_space)
    }
}

// return procedure:
// 1. pop call frame
// 2. restore stack and literal pointers
// 3. jump to return address
unsafe fn return_from_call(
    context_ptr: *mut LocalContext,
) -> (*const Instruction, *mut StackSlot, *const Literal) {
    let context = &mut *context_ptr;
    let current_frame = &mut *context.current_frame;
    let outer_context = &mut *context.vm;

    // pop call frame
    let prev_frame = outer_context.frames.pop_unchecked();
    let current_frame = core::mem::replace(current_frame, prev_frame);

    // restore stack and literal pointers
    let prev_stack_ptr = outer_context.stack.offset(prev_frame.stack_base as usize);
    let prev_literals_ptr = Function::literals_ptr(prev_frame.callee);

    // jump to return address
    let return_addr = current_frame.return_addr as usize;
    let prev_code_ptr = Function::code_ptr(prev_frame.callee).add(return_addr);

    (prev_code_ptr, prev_stack_ptr, prev_literals_ptr)
}

wrap_abi! {
    unsafe fn call(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        _stack: *mut StackSlot,
        _literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (ret, callee): info::call = operands.split();

        let callee = get_function_unchecked(context, callee);

        let (code, stack, literals) = prepare_call(callee, ret, code, context);

        dispatch_current(ops, code, stack, literals, context)
    }
}

unsafe fn do_extern_call(
    callee: *const ExternFunction,
    ret: Register,

    ops: *const (),
    context: *mut LocalContext,
) -> Result<Value, ExternFunctionError> {
    let context = &mut *context;
    let current_frame = &mut *context.current_frame;
    let vm = &mut *context.vm;
    let stack = grow_stack(
        vm,
        current_frame.stack_base as usize + ret as usize,
        0, // no additional stack space required
    );

    ((*callee).wrapper)(Scope {
        ops,
        stack,
        context,
        lifetime: PhantomData,
    })
}

wrap_abi! {
    unsafe fn call_extern(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (ret, callee): info::call_extern = operands.split();

        let callee = get_extern_function_unchecked(context, callee);
        let result = do_extern_call(callee, ret, ops,  context);
        match result {
            Ok(value) => stack.add(ret as usize).write(value),
            Err(e) => {
                (*context).error.write(Some(Error::Extern(e)));
                return Control::Error
            }
        }

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn call_reg(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let ( _callee, ): info::call_reg = operands.split();

        // TODO

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn call_extern_reg(
        operands: u24,
        ops: *const (),
        code: *const Instruction,
        stack: *mut StackSlot,
        literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let ( _callee, ): info::call_extern_reg = operands.split();

        // TODO

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn ret(
        _operands: u24,
        ops: *const (),
        _code: *const Instruction,
        _stack: *mut StackSlot,
        _literals: *const Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (code, stack, literals) = return_from_call(context);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn end(
        _operands: u24,
        _ops: *const (),
        _code: *const Instruction,
        _stack: *mut StackSlot,
        _literals: *const Literal,
        _context: *mut LocalContext,
    ) -> Control {
        Control::End
    }
}

macro_rules! ops {
    (
        mod $info:ident;
        mod $asm:ident;
        struct $Disasm:ident;
        struct $RawOpcode:ident;
        enum $DecodedInstruction:ident;
        invalid = $invalid_op:ident;
        $table:ident: [$Opcode:ident] = [
            $($index:literal = $op:ident $(($($operand:ident : $operand_ty:ty),+))?),* $(,)?
        ]
    ) => {
        static $table: [Op; 256] = const {
            let mut table: [Op; 256] = [$invalid_op; 256];

            $(
                table[$index as usize] = unsafe{
                    core::mem::transmute::<*const (), Op>($op as *const ())
                };
            )*

            table
        };

        paste::paste! {
            #[repr(u8)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum $Opcode {
                [<$invalid_op:camel>] = 0,
                $([<$op:camel>] = $index),*
            }

            #[repr(u8)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum $DecodedInstruction {
                [<$invalid_op:camel>] = 0,
                $([<$op:camel>] $({ $($operand: $operand_ty),* })? = $index),*
            }

            impl $DecodedInstruction {
                pub fn name(&self) -> &'static str {
                    #![allow(unused_variables)]

                    match self {
                        Self::[<$invalid_op:camel>] => "???",
                        $(
                            Self::[<$op:camel>] $({ $($operand),* })? => stringify!($op),
                        )*
                    }
                }
            }

            impl From<(u8, u24)> for $DecodedInstruction {
                fn from((opcode, operands): (u8, u24)) -> Self {
                    match opcode {
                        $(
                            $index => {
                                let ($($($operand,)*)?) = operands.split();
                                Self::[<$op:camel>] $({ $($operand),* })?
                            }
                        )*
                        _ => Self::[<$invalid_op:camel>],
                    }
                }
            }

            pub mod $asm {
                use super::*;

                pub const fn $invalid_op() -> Instruction {
                    Instruction(0u32)
                }

                $(
                    pub fn $op(
                        $(
                            $($operand : $operand_ty),*
                        )?
                    ) -> Instruction {
                        $crate::vm::operands::encode(
                            $Opcode::[<$op:camel>],
                            ($($($operand,)*)?)
                        )
                    }
                )*
            }


            struct $Disasm(pub Instruction);

            impl core::fmt::Display for Disasm {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    let Disasm(instruction) = self;

                    match instruction.decode() {
                        $(
                            $DecodedInstruction::[<$op:camel>] $({ $($operand,)* })? => {
                                write!(f,
                                    concat!(
                                        stringify!($op),
                                        $($(
                                            " ",
                                            stringify!($operand),
                                            "={}",
                                        )*)?
                                    ),

                                    $($($operand),*)?
                                )
                            }
                        )*
                        _ => {
                            write!(f, "invalid")
                        }
                    }
                }
            }
        }

        pub type $RawOpcode = u8;

        pub mod $info {
            #![allow(non_camel_case_types)]

            use $crate::vm::operands::*;

            $(
                pub type $op = ($($($operand_ty,)*)?);
            )*
        }
    }
}

ops! {
    mod info;
    mod asm;
    struct Disasm;
    struct RawOpcode;
    enum DecodedInstruction;
    invalid = invalid_op;
    OPS: [Opcode] = [
        0x01 = mov(dst: Register, src: Register),

        0x02 = load_unit(dst: Register),
        0x03 = load_literal(dst: Register, src: LiteralId),
        0x04 = load_i16(dst: Register, value: i16),
        0x05 = load_true(dst: Register),
        0x06 = load_false(dst: Register),
        0x07 = load_fn(dst: Register, id: FunctionId),
        0x08 = load_fn_extern(dst: Register, id: ExternFunctionId),

        0x20 = jump(offset: PcRelativeOffset),
        0x21 = jump_long(offset: LiteralId),
        0x22 = jump_if_false(condition: Register, offset: PcRelativeOffset),
        0x23 = jump_if_false_long(condition: Register, offset: LiteralId),

        0x40 = add_i64(dst: Register, lhs: Register, rhs: Register),
        0x41 = add_f64(dst: Register, lhs: Register, rhs: Register),
        0x42 = sub_i64(dst: Register, lhs: Register, rhs: Register),
        0x43 = sub_f64(dst: Register, lhs: Register, rhs: Register),
        0x44 = mul_i64(dst: Register, lhs: Register, rhs: Register),
        0x45 = mul_f64(dst: Register, lhs: Register, rhs: Register),
        0x46 = div_i64(dst: Register, lhs: Register, rhs: Register),
        0x47 = div_f64(dst: Register, lhs: Register, rhs: Register),
        0x48 = rem_i64(dst: Register, lhs: Register, rhs: Register),
        0x49 = rem_f64(dst: Register, lhs: Register, rhs: Register),

        0x50 = cmp_eq_i64(dst: Register, lhs: Register, rhs: Register),
        0x51 = cmp_eq_f64(dst: Register, lhs: Register, rhs: Register),
        0x52 = cmp_eq_bool(dst: Register, lhs: Register, rhs: Register),
        // 0x53 = cmp_eq_str,
        0x54 = cmp_ne_i64(dst: Register, lhs: Register, rhs: Register),
        0x55 = cmp_ne_f64(dst: Register, lhs: Register, rhs: Register),
        0x56 = cmp_ne_bool(dst: Register, lhs: Register, rhs: Register),
        // 0x57 = cmp_ne_str,
        0x58 = cmp_gt_i64(dst: Register, lhs: Register, rhs: Register),
        0x59 = cmp_gt_f64(dst: Register, lhs: Register, rhs: Register),
        0x5A = cmp_lt_i64(dst: Register, lhs: Register, rhs: Register),
        0x5B = cmp_lt_f64(dst: Register, lhs: Register, rhs: Register),
        0x5C = cmp_ge_i64(dst: Register, lhs: Register, rhs: Register),
        0x5D = cmp_ge_f64(dst: Register, lhs: Register, rhs: Register),
        0x5E = cmp_le_i64(dst: Register, lhs: Register, rhs: Register),
        0x5F = cmp_le_f64(dst: Register, lhs: Register, rhs: Register),
        0x60 = neg_i64(dst: Register, rhs: Register),
        0x61 = neg_f64(dst: Register, rhs: Register),
        0x62 = not_bool(dst: Register, rhs: Register),

        0x80 = call(ret: Register, callee: FunctionId),
        0x81 = call_extern(ret: Register, callee: ExternFunctionId),
        0x82 = call_reg(callee: Register),
        0x83 = call_extern_reg(callee: Register),

        0x90 = ret,

        0xFF = end,
    ]
}

#[cfg(test)]
mod tests;
