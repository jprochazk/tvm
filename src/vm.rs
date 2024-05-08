#[macro_use]
mod macros;

use std::sync::Arc;

use crate::code::op::{Fnid, Lit, Op, Reg};
use crate::code::Function;
use crate::error::Result;
use crate::value::Literal;

pub struct State<'a> {
    main: Arc<Function>,
    functions: Functions<'a>,
    vstack: Stack,
    cstack: Vec<CallFrame>,
    ip: isize,
    #[cfg(test)]
    debug_hook: DebugHook,
}

impl State<'_> {
    fn interpret(&mut self) -> Result<Value> {
        let State {
            main,
            functions,
            vstack,
            cstack,
            ip,
            #[cfg(test)]
            debug_hook,
        } = self;

        let mut current_frame = CallFrame {
            callee: main.clone(),
            ret_addr: 0,
            stack_base: 0,
        };

        'setup_frame: loop {
            let callee = current_frame.callee.clone();
            let code = &callee.bytecode[..];
            let pool = Literals::new(&callee.literals);

            loop {
                let op = code[*ip as usize];

                #[cfg(test)]
                {
                    debug_hook(DebugEvent::Dispatch {
                        op,
                        stack_frame: StackFrame::get(vstack, &current_frame),
                    });
                }

                match op {
                    Op::Nop => {
                        // no-op
                    }

                    Op::Mov { src, dst } => {
                        let value = vstack.get_raw(src);
                        vstack.set(dst, value);
                    }

                    Op::Load_Literal { token, dst, src } => {
                        let value = pool.get(src, token);
                        vstack.set(dst, value);
                    }

                    Op::Load_Unit { dst } => {
                        vstack.set(dst, Value::Unit(()));
                    }

                    Op::Load_Fn { dst, id } => todo!(),

                    Op::Load_I16 { dst, val } => {
                        vstack.set(dst, Value::I64(val as i64));
                    }

                    Op::Load_Bool { val, dst } => {
                        vstack.set(dst, Value::Bool(val));
                    }

                    Op::Jump { offset } => {
                        *ip += offset.sign_extend();
                    }

                    Op::Jump_Long { token, offset } => {
                        let offset = pool.get(offset, token);
                        *ip += offset;
                    }

                    Op::JumpIfFalse { cond, offset } => todo!(),
                    Op::JumpIfFalse_Long {
                        token,
                        cond,
                        offset,
                    } => todo!(),

                    Op::Add_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, +, =dst),
                    Op::Add_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, +, =dst),

                    Op::Sub_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, -, =dst),
                    Op::Sub_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, -, =dst),

                    Op::Mul_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, *, =dst),
                    Op::Mul_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, *, =dst),

                    Op::Div_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, /, =dst),
                    Op::Div_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, /, =dst),

                    Op::Rem_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, %, =dst),
                    Op::Rem_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => binary_op!(vstack, token, lhs, rhs, %, =dst),

                    Op::Call_Id { ret, callee } => {
                        let callee = functions.get(callee);
                        let ret_addr = *ip + 1;
                        let stack_size = callee.registers;
                        let new_frame = CallFrame {
                            callee,
                            ret_addr,
                            stack_base: current_frame.stack_base + ret.to_index(),
                        };

                        #[cfg(test)]
                        {
                            debug_hook(DebugEvent::Call {
                                call_frame: &new_frame,
                            });
                        }

                        *ip = 0;
                        vstack.base = new_frame.stack_base;
                        vstack.reserve(stack_size);
                        cstack.push(core::mem::replace(&mut current_frame, new_frame));

                        continue 'setup_frame;
                    }

                    Op::Call_Reg { callee } => todo!(),

                    Op::Ret => match cstack.pop() {
                        Some(prev_frame) => {
                            *ip = current_frame.ret_addr;
                            vstack.base = prev_frame.stack_base;
                            current_frame = prev_frame;
                            continue 'setup_frame;
                        }
                        None => return Ok(vstack.ret()),
                    },
                }

                #[cfg(test)]
                {
                    debug_hook(DebugEvent::PostDispatch {
                        stack_frame: StackFrame::get(vstack, &current_frame),
                    });
                }

                *ip += 1;
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Value {
    Unit(()) = 0,
    Bool(bool),
    I64(i64),
    F64(f64),
}

impl From<()> for Value {
    fn from(value: ()) -> Self {
        Value::Unit(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::I64(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::F64(value)
    }
}

struct Literals<'a> {
    pool: &'a [Literal],
}

impl<'a> Literals<'a> {
    fn new(pool: &'a [Literal]) -> Self {
        Self { pool }
    }

    fn get<Token: token::Cast<Literal>>(&self, src: Lit, token: Token) -> Token::Output {
        unsafe {
            let value = core::ptr::read(self.pool.get_unchecked(src.to_index()));
            token.cast(value).unwrap_unchecked()
        }
    }
}

struct Functions<'a> {
    functions: &'a [Arc<Function>],
}

impl<'a> Functions<'a> {
    fn new(functions: &'a [Arc<Function>]) -> Self {
        Self { functions }
    }

    fn get(&self, src: Fnid) -> Arc<Function> {
        unsafe { Arc::clone(self.functions.get_unchecked(src.to_index())) }
    }
}

struct Stack {
    base: usize,
    inner: Vec<Value>,
}

impl Stack {
    fn new(capacity: usize) -> Self {
        Self {
            base: 0,
            inner: vec![Value::Unit(()); capacity],
        }
    }

    #[inline]
    fn reserve(&mut self, stack_size: u8) {
        // TODO: big bad, do better
        // use `Vec<MaybeUninit<Value>>` instead of zeroing
        // safe to `assume_init` because any read value
        // is guaranteed to be written to first
        let stack_size = stack_size as usize;
        if self.inner[self.base..].len() < stack_size {
            self.inner.extend((0..stack_size).map(|_| Value::Unit(())));
        }
    }

    #[inline]
    fn ret(&self) -> Value {
        self.get_raw(Reg(0))
    }

    #[inline]
    fn set(&mut self, dst: Reg, value: impl Into<Value>) {
        unsafe {
            let dst = self.inner.get_unchecked_mut(self.base + dst.to_index());
            *dst = value.into();
        }
    }

    #[inline]
    fn get_raw(&self, src: Reg) -> Value {
        unsafe {
            let src = self.inner.get_unchecked(self.base + src.to_index());
            *src
        }
    }

    #[inline]
    fn get<Token: token::Cast<Value>>(&self, src: Reg, token: Token) -> Token::Output {
        unsafe {
            let value = self.get_raw(src);
            token.cast(value).unwrap_unchecked()
        }
    }
}

#[derive(Debug)]
struct CallFrame {
    callee: Arc<Function>,
    ret_addr: isize,
    stack_base: usize,
}

pub struct Module {
    pub(crate) main: Arc<Function>,
    pub(crate) functions: Vec<Arc<Function>>,
}

enum DebugEvent<'a> {
    Dispatch { op: Op, stack_frame: StackFrame<'a> },
    PostDispatch { stack_frame: StackFrame<'a> },
    Call { call_frame: &'a CallFrame },
}

struct StackFrame<'a>(&'a [Value]);

impl<'a> StackFrame<'a> {
    fn get(stack: &'a Stack, call_frame: &CallFrame) -> Self {
        Self(&stack.inner[stack.base..stack.base + call_frame.callee.registers as usize])
    }
}

type DebugHook = fn(DebugEvent<'_>);

impl<'a> State<'a> {
    fn new(module: &'a Module) -> Self {
        Self {
            main: module.main.clone(),
            functions: Functions::new(&module.functions),
            vstack: Stack::new(512),
            cstack: Vec::with_capacity(16),
            ip: 0,
            #[cfg(test)]
            debug_hook: |_| {},
        }
    }
}

impl Module {
    pub fn run(&self) -> Result<Value> {
        State::new(self).interpret()
    }

    #[cfg(test)]
    fn debug(&self, debug_hook: DebugHook) -> Result<Value> {
        let mut state = State::new(self);
        state.debug_hook = debug_hook;
        state.interpret()
    }
}

pub mod token {
    #![allow(dead_code)]

    use crate::value::Literal;
    use crate::vm::Value;

    #[doc(hidden)]
    pub(crate) trait Cast<Input>: Sized {
        type Output;

        fn cast(self, value: Input) -> Option<Self::Output>;
    }

    macro_rules! define_token {
        ($name:ident) => {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            #[repr(transparent)]
            pub struct $name([u8; 0]);

            paste::paste! {
                pub(crate) unsafe fn [<$name:snake>]() -> $name {
                    $name([])
                }
            }
        };
    }

    macro_rules! define_cast {
        (from $input:ident :: $variant:ident, to $output:ty, using $token:ident) => {
            impl Cast<$input> for $token {
                type Output = $output;

                #[inline(always)]
                fn cast(self, value: $input) -> Option<Self::Output> {
                    match value {
                        $input::$variant(value) => Some(value.into()),
                        _ => None,
                    }
                }
            }
        };
    }

    define_token!(ToBool);
    define_token!(ToI64);
    define_token!(ToF64);
    define_token!(ToValue);
    define_token!(ToOffset);

    define_cast!(from Value::Bool, to bool, using ToBool);
    define_cast!(from Value::I64, to i64, using ToI64);
    define_cast!(from Value::F64, to f64, using ToF64);

    impl Cast<Literal> for ToValue {
        type Output = Value;

        fn cast(self, value: Literal) -> Option<Self::Output> {
            match value {
                Literal::I64(v) => Some(Value::I64(v)),
                Literal::F64(v) => Some(Value::F64(v.into())),
                Literal::Str(_) => todo!(),
                Literal::Jmp(_) => None,
            }
        }
    }

    impl Cast<Literal> for ToOffset {
        type Output = isize;

        fn cast(self, value: Literal) -> Option<Self::Output> {
            match value {
                Literal::Jmp(value) => Some(value),
                _ => None,
            }
        }
    }
}

#[cfg(test)]
mod tests;
