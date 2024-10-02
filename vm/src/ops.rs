use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::fmt::Write as _;
use core::ptr::addr_of_mut as mut_;

use crate::dyn_array::DynArray;
use crate::operands::{
    self, u24, ExternFunctionId, FunctionId, LiteralId, Offset, Register, SplitOperands as _,
};
use crate::stdout;
use crate::value::Literal;
use crate::value::Value;

pub fn dispatch(context: &mut Context, main: FunctionId) -> Result<(), Error> {
    let mut current_frame = CallFrame {
        callee: &mut Function {
            code: vec![
                operands::encode(Opcode::call, (0u8, main)),
                operands::encode(Opcode::end, ()),
            ]
            .into(),
            literals: vec![].into(),
            registers: 1,
        },
        stack_base: 0,
        return_addr: 1,
    };

    let mut error = None;
    let mut local_context = LocalContext {
        outer: context,
        current_frame: &mut current_frame,
        error: &mut error,
    };

    #[cfg(debug_assertions)]
    unsafe {
        let callee = &mut *current_frame.callee;

        let ops = (&OPS) as *const Op;
        let mut code = callee.code.as_mut_ptr();
        let mut stack = context.stack.offset(0);
        let mut literals = callee.literals.as_mut_ptr();
        let context = &mut local_context as *mut _;

        let (mut opcode, mut operands) = operands::decode(code.read());
        loop {
            #[cfg(debug_assertions)]
            writeln!(stdout(), "{}", Disasm(opcode, operands)).ok();

            let op = ops.add(opcode as usize).read();
            match (op)(operands, ops as *mut _, code, stack, literals, context) {
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
        let callee = &mut (*current_frame.callee);
        match dispatch_current(
            (&OPS) as *const [Op; 256] as *mut (),
            callee.code.as_mut_ptr(),
            context.stack.offset(0),
            callee.literals.as_mut_ptr(),
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
    Continue(Opcode, u24, *mut Instruction, *mut StackSlot, *mut Literal),
}

pub type Instruction = u32;

pub struct Function {
    code: Box<[Instruction]>,
    literals: Box<[Literal]>,
    // TODO: store this as pointer tag
    registers: u8,
}

impl Function {
    pub fn new(
        code: impl Into<Box<[Instruction]>>,
        literals: impl Into<Box<[Literal]>>,
        registers: u8,
    ) -> Self {
        Self {
            code: code.into(),
            literals: literals.into(),
            registers,
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
struct CallFrame {
    callee: *mut Function,
    stack_base: u32,
    return_addr: u32,
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

pub struct Context {
    stack: DynArray<StackSlot>,
    frames: Vec<CallFrame>,
    functions: Vec<Arc<Function>>,
}

impl Context {
    pub fn new(functions: Vec<Arc<Function>>) -> Context {
        Self {
            stack: unsafe { DynArray::new() },
            frames: Vec::new(),
            functions,
        }
    }

    pub fn ret(&self) -> Value {
        unsafe { self.stack.offset(0).read() }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    InvalidOp,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Error::InvalidOp => write!(f, "invalid op"),
        }
    }
}

impl core::error::Error for Error {}

struct LocalContext {
    outer: *mut Context,
    current_frame: *mut CallFrame,
    error: *mut Option<Error>,
}

#[cfg(not(windows))]
type Op = unsafe extern "C" fn(
    operands: u24,
    ops: *mut (),
    code: *mut Instruction,
    stack: *mut StackSlot,
    literals: *mut Literal,
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
    ops: *mut (),
    code: *mut Instruction,
    stack: *mut Stack,
    literals: *mut Literal,
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
    ops: *mut (),
    code: *mut Instruction,
    stack: *mut StackSlot,
    literals: *mut Literal,
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
    ops: *mut (),
    code: *mut Instruction,
    stack: *mut StackSlot,
    literals: *mut Literal,
    context: *mut LocalContext,
) -> Control {
    let code = code.add(1);
    dispatch_current(ops, code, stack, literals, context)
}

wrap_abi! {
    unsafe fn invalid_op(
        _operands: u24,
        _ops: *mut (),
        _code: *mut Instruction,
        _stack: *mut StackSlot,
        _literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        mut_!(*(*context).error).write(Some(Error::InvalidOp));
        Control::Error
    }
}

wrap_abi! {
    unsafe fn mov(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
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
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
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
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
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
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst, src): info::load_i16 = operands.split();

        stack.add(dst as usize).write(Value::I64(src as i64));

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn load_true(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
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
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst,): info::load_false = operands.split();

        stack.add(dst as usize).write(Value::Bool(false));

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (offset,): info::jump = operands.split();

        let code = code.add(offset as usize);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump_long(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (offset,): info::jump_long = operands.split();
        let offset = literals.add(offset as usize).read().unbox_jmp();

        let code = code.add(offset);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump_if_false(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (condition, offset): info::jump_if_false = operands.split();
        let condition = stack.add(condition as usize).read();

        let code = if !condition.unbox().into_bool_unchecked() {
            code.add(offset as usize)
        } else {
            code.add(1)
        };

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn jump_if_false_long(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (condition, offset): info::jump_if_false_long = operands.split();
        let condition = stack.add(condition as usize).read();
        let offset = literals.add(offset as usize).read().unbox_jmp();

        let code = if !condition.unbox().into_bool_unchecked() {
            code.add(offset)
        } else {
            code.add(1)
        };

        dispatch_current(ops, code, stack, literals, context)
    }
}

macro_rules! arithmetic {
    ($name:ident, $into:ident, $op:tt) => {
        wrap_abi! {
            unsafe fn $name(
                operands: u24,
                ops: *mut (),
                code: *mut Instruction,
                stack: *mut StackSlot,
                literals: *mut Literal,
                context: *mut LocalContext,
            ) -> Control {
                let (dst, lhs, rhs): info::$name = operands.split();

                let lhs = stack.add(lhs as usize).read().unbox().$into();
                let rhs = stack.add(rhs as usize).read().unbox().$into();
                let result = lhs $op rhs;

                #[cfg(debug_assertions)]
                writeln!(stdout(), concat!("{} ", stringify!($op), " {} = {}"), lhs, rhs, result).ok();

                stack.add(dst as usize).write(Value::from(result));

                dispatch_next(ops, code, stack, literals, context)
            }
        }
    };
}

arithmetic!(add_i64, into_i64_unchecked, +);
arithmetic!(add_f64, into_f64_unchecked, +);
arithmetic!(sub_i64, into_i64_unchecked, -);
arithmetic!(sub_f64, into_f64_unchecked, -);
arithmetic!(mul_i64, into_i64_unchecked, *);
arithmetic!(mul_f64, into_f64_unchecked, *);
arithmetic!(div_i64, into_i64_unchecked, /);
arithmetic!(div_f64, into_f64_unchecked, /);
arithmetic!(rem_i64, into_i64_unchecked, %);
arithmetic!(rem_f64, into_f64_unchecked, %);

macro_rules! comparison {
    ($name:ident, $into:ident, $op:tt) => {
        wrap_abi! {
            unsafe fn $name(
                operands: u24,
                ops: *mut (),
                code: *mut Instruction,
                stack: *mut StackSlot,
                literals: *mut Literal,
                context: *mut LocalContext,
            ) -> Control {
                let (dst, lhs, rhs): info::$name = operands.split();

                let lhs = stack.add(lhs as usize).read().unbox().$into();
                let rhs = stack.add(rhs as usize).read().unbox().$into();
                let result = lhs $op rhs;

                #[cfg(debug_assertions)]
                writeln!(stdout(), concat!("{} ", stringify!($op), " {} = {}"), lhs, rhs, result).ok();

                stack.add(dst as usize).write(Value::Bool(result));

                dispatch_next(ops, code, stack, literals, context)
            }
        }
    };
}

comparison!(cmp_eq_i64, into_i64_unchecked, ==);
comparison!(cmp_eq_f64, into_f64_unchecked, ==);
comparison!(cmp_eq_bool, into_bool_unchecked, ==);
comparison!(cmp_ne_i64, into_i64_unchecked, !=);
comparison!(cmp_ne_f64, into_f64_unchecked, !=);
comparison!(cmp_ne_bool, into_bool_unchecked, !=);
comparison!(cmp_gt_i64, into_i64_unchecked, >);
comparison!(cmp_gt_f64, into_f64_unchecked, >);
comparison!(cmp_lt_i64, into_i64_unchecked, <);
comparison!(cmp_lt_f64, into_f64_unchecked, <);
comparison!(cmp_ge_i64, into_i64_unchecked, >=);
comparison!(cmp_ge_f64, into_f64_unchecked, >=);
comparison!(cmp_le_i64, into_i64_unchecked, <=);
comparison!(cmp_le_f64, into_f64_unchecked, <=);

macro_rules! negate {
    ($name:ident, $into:ident) => {
        wrap_abi! {
            unsafe fn $name(
                operands: u24,
                ops: *mut (),
                code: *mut Instruction,
                stack: *mut StackSlot,
                literals: *mut Literal,
                context: *mut LocalContext,
            ) -> Control {
                let (dst, rhs): info::$name = operands.split();

                let rhs = stack.add(rhs as usize).read().unbox().$into();
                let result = -rhs;
                stack.add(dst as usize).write(Value::from(result));

                dispatch_next(ops, code, stack, literals, context)
            }
        }
    };
}

negate!(neg_i64, into_i64_unchecked);
negate!(neg_f64, into_f64_unchecked);

wrap_abi! {
    unsafe fn not_bool(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (dst, rhs): info::not_bool = operands.split();

        let rhs = stack.add(rhs as usize).read().unbox().into_bool_unchecked();
        let result = !rhs;
        stack.add(dst as usize).write(Value::Bool(result));

        dispatch_next(ops, code, stack, literals, context)
    }
}

unsafe fn prepare_call(
    callee: *mut Function,
    ret: Register,
    code_ptr: *mut Instruction,
    stack_ptr: *mut StackSlot,
    context_ptr: *mut LocalContext,
) -> (*mut Instruction, *mut StackSlot, *mut Literal) {
    let context = &mut *context_ptr;
    let current_frame = &mut *context.current_frame;
    let outer_context = &mut *context.outer;

    let current_code_addr = code_ptr.offset_from((*current_frame.callee).code.as_ptr()) as u32;

    let new_frame = CallFrame {
        callee,
        stack_base: current_frame.stack_base + ret as u32,
        return_addr: current_code_addr + 1,
    };
    #[cfg(debug_assertions)]
    writeln!(stdout(), "{new_frame:?}").ok();

    let new_code_ptr = (*callee).code.as_mut_ptr();

    let remaining_stack_space = outer_context.stack.remaining(new_frame.stack_base as usize);
    let required_stack_space = (*callee).registers as usize;
    let new_stack_ptr = if remaining_stack_space < required_stack_space {
        outer_context
            .stack
            .grow_with_ptr(stack_ptr)
            .add(new_frame.stack_base as usize)
    } else {
        stack_ptr.add(ret as usize)
    };

    let new_literals_ptr = (*callee).literals.as_mut_ptr();

    outer_context
        .frames
        .push(core::mem::replace(current_frame, new_frame));

    (new_code_ptr, new_stack_ptr, new_literals_ptr)
}

unsafe fn return_from_call(
    context_ptr: *mut LocalContext,
) -> (*mut Instruction, *mut StackSlot, *mut Literal) {
    let context = &mut *context_ptr;
    let current_frame = &mut *context.current_frame;
    let outer_context = &mut *context.outer;

    let prev_frame = outer_context.frames.pop().unwrap_unchecked();
    let current_frame = core::mem::replace(current_frame, prev_frame);

    let prev_literals_ptr = (*prev_frame.callee).literals.as_mut_ptr();
    let prev_stack_ptr = outer_context.stack.offset(prev_frame.stack_base as usize);
    let prev_code_ptr = (*prev_frame.callee)
        .code
        .as_mut_ptr()
        .offset(current_frame.return_addr as isize);

    (prev_code_ptr, prev_stack_ptr, prev_literals_ptr)
}

wrap_abi! {
    unsafe fn call(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        _literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (ret, callee): info::call = operands.split();

        let callee: &Function = (*(*context).outer).functions.get_unchecked(callee as usize);
        let callee = callee as *const _ as *mut Function;

        let (code, stack, literals) = prepare_call(callee, ret, code, stack, context);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn call_host(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (_ret, _callee): info::call_host = operands.split();

        // TODO

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn call_reg(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (_ret, _callee): info::call_reg = operands.split();

        // TODO

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn call_host_reg(
        operands: u24,
        ops: *mut (),
        code: *mut Instruction,
        stack: *mut StackSlot,
        literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (_ret, _callee): info::call_host_reg = operands.split();

        // TODO

        dispatch_next(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn ret(
        _operands: u24,
        ops: *mut (),
        _code: *mut Instruction,
        _stack: *mut StackSlot,
        _literals: *mut Literal,
        context: *mut LocalContext,
    ) -> Control {
        let (code, stack, literals) = return_from_call(context);

        dispatch_current(ops, code, stack, literals, context)
    }
}

wrap_abi! {
    unsafe fn end(
        _operands: u24,
        _ops: *mut (),
        _code: *mut Instruction,
        _stack: *mut StackSlot,
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
        $table:ident: [$Opcode:ident] = [
            $($index:literal = $op:ident $(($($operand:ident : $operand_ty:ty),+))?),* $(,)?
        ]
    ) => {
        static $table: [Op; 256] = const {
            let mut table: [Op; 256] = [invalid_op; 256];

            $(
                table[$index as usize] = unsafe{
                    core::mem::transmute::<*const (), Op>($op as *const ())
                };
            )*

            table
        };

        #[allow(non_camel_case_types)]
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum $Opcode {
            $($op = $index),*
        }

        pub mod $info {
            #![allow(non_camel_case_types)]

            use $crate::operands::*;

            $(
                pub type $op = ($($($operand_ty,)*)?);
            )*
        }

        pub mod $asm {
            use super::*;

            $(
                pub fn $op(
                    $(
                        $($operand : $operand_ty),*
                    )?
                ) -> Instruction {
                    $crate::operands::encode(
                        $Opcode::$op,
                        ($($($operand,)*)?)
                    )
                }
            )*
        }

        pub struct $Disasm(pub $Opcode, pub u24);

        impl core::fmt::Display for Disasm {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let Disasm(opcode, operands) = self;

                match opcode {
                    $(
                        $Opcode::$op => {
                            let ($($($operand,)*)?): $info::$op = operands.split();
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
                }
            }
        }
    }
}

ops! {
    mod info;
    mod asm;
    struct Disasm;
    OPS: [Opcode] = [
        0x00 = invalid_op,
        0x01 = mov(dst: Register, src: Register),

        0x02 = load_unit(dst: Register),
        0x03 = load_literal(dst: Register, src: LiteralId),
        0x04 = load_i16(dst: Register, src: i16),
        0x05 = load_true(dst: Register),
        0x06 = load_false(dst: Register),

        0x20 = jump(offset: Offset),
        0x21 = jump_long(offset: LiteralId),
        0x22 = jump_if_false(condition: Register, offset: Offset),
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
        0x81 = call_host(ret: Register, callee: ExternFunctionId),
        0x82 = call_reg(ret: Register, callee: Register),
        0x83 = call_host_reg(ret: Register, callee: Register),

        0x90 = ret,

        0xFF = end,
    ]
}
