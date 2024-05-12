#[macro_use]
mod macros;

use std::sync::Arc;

use crate::code::op::{FnId, HostId, Lit, Op, Reg};
use crate::code::{ExternFunction, Function};
use crate::error::Result;
use crate::value::Literal;

pub struct Vm {
    vstack: Stack,
    cstack: Vec<CallFrame>,
    ip: isize,
    #[cfg(test)]
    debug_hook: DebugHook,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            vstack: Stack::new(512),
            cstack: Vec::with_capacity(16),
            ip: 0,
            #[cfg(test)]
            debug_hook: |_| {},
        }
    }

    pub fn run(&mut self, module: impl AsRef<Module>) -> Result<Value> {
        State::new(self, module.as_ref()).interpret()
    }

    #[cfg(test)]
    fn run_debug(&mut self, module: impl AsRef<Module>, debug_hook: DebugHook) -> Result<Value> {
        State::new(self, module.as_ref())
            .with_debug_hook(debug_hook)
            .interpret()
    }
}

pub struct State<'a> {
    main: Arc<Function>,
    functions: Functions<'a>,
    vm: &'a mut Vm,
}

impl<'a> State<'a> {
    fn new(vm: &'a mut Vm, module: &'a Module) -> Self {
        Self {
            main: module.main.clone(),
            functions: Functions::new(&module.script_fns, &module.host_fns),
            vm,
        }
    }

    #[cfg(test)]
    fn with_debug_hook(self, debug_hook: DebugHook) -> Self {
        self.vm.debug_hook = debug_hook;
        self
    }
}

macro_rules! invoke_hook {
    ($self:ident, dispatch, $op:ident, $current_frame:ident) => {{
        #[cfg(test)]
        {
            ($self.vm.debug_hook)(DebugEvent::Dispatch {
                op: $op,
                stack_frame: StackFrame::get(&$self.vm.vstack, &$current_frame),
            });
        };
    }};

    ($self:ident, post_dispatch, $current_frame:ident) => {{
        #[cfg(test)]
        {
            ($self.vm.debug_hook)(DebugEvent::PostDispatch {
                stack_frame: StackFrame::get(&$self.vm.vstack, &$current_frame),
            });
        };
    }};
}

impl State<'_> {
    fn interpret(&mut self) -> Result<Value> {
        /* let State {
            main,
            functions,
            vm:
                Vm {
                    vstack,
                    cstack,
                    ip,
                    #[cfg(test)]
                    debug_hook,
                },
        } = self; */

        let mut current_frame = CallFrame {
            callee: self.main.clone(),
            ret_addr: 0,
            stack_base: 0,
        };

        macro_rules! dispatch {
            (next) => {
                self.vm.ip += 1;
                invoke_hook!(self, post_dispatch, current_frame);
                continue;
            };
            (current) => {
                invoke_hook!(self, post_dispatch, current_frame);
                continue;
            };
        }

        'setup_frame: loop {
            let callee = current_frame.callee.clone();
            let code = &callee.bytecode[..];
            let pool = Literals::new(&callee.literals);

            loop {
                let op = code[self.vm.ip as usize];
                invoke_hook!(self, dispatch, op, current_frame);
                match op {
                    Op::Nop => {
                        // no-op
                        dispatch!(next);
                    }

                    Op::Mov { src, dst } => {
                        let value = self.vm.vstack.get_raw(src);
                        self.vm.vstack.set(dst, value);
                        dispatch!(next);
                    }

                    Op::Load_Literal { token, dst, src } => {
                        let value = pool.get(src, token);
                        self.vm.vstack.set(dst, value);
                        dispatch!(next);
                    }

                    Op::Load_Unit { dst } => {
                        self.vm.vstack.set(dst, Value::Unit(()));
                        dispatch!(next);
                    }

                    Op::Load_Fn { dst, id } => todo!(),

                    Op::Load_Fn_Host { dst, id } => todo!(),

                    Op::Load_I16 { dst, val } => {
                        self.vm.vstack.set(dst, Value::I64(val as i64));
                        dispatch!(next);
                    }

                    Op::Load_Bool { val, dst } => {
                        self.vm.vstack.set(dst, Value::Bool(val));
                        dispatch!(next);
                    }

                    Op::Jump { offset } => {
                        self.vm.ip += offset.sign_extend();
                        dispatch!(current);
                    }

                    Op::Jump_Long { token, offset } => {
                        let offset = pool.get(offset, token);
                        self.vm.ip += offset;
                        dispatch!(current);
                    }

                    Op::JumpIfFalse { ty, cond, offset } => {
                        let cond = self.vm.vstack.get(cond, ty);
                        if !cond {
                            self.vm.ip += offset.sign_extend();
                        } else {
                            self.vm.ip += 1;
                        }
                        dispatch!(current);
                    }

                    Op::JumpIfFalse_Long {
                        ty,
                        token,
                        cond,
                        offset,
                    } => {
                        let cond = self.vm.vstack.get(cond, ty);
                        let offset = pool.get(offset, token);
                        if !cond {
                            self.vm.ip += offset;
                        } else {
                            self.vm.ip += 1;
                        }
                        dispatch!(current);
                    }

                    Op::Add_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, +, =dst);
                        dispatch!(next);
                    }
                    Op::Add_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, +, =dst);
                        dispatch!(next);
                    }

                    Op::Sub_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, -, =dst);
                        dispatch!(next);
                    }
                    Op::Sub_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, -, =dst);
                        dispatch!(next);
                    }

                    Op::Mul_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, *, =dst);
                        dispatch!(next);
                    }
                    Op::Mul_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, *, =dst);
                        dispatch!(next);
                    }

                    Op::Div_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, /, =dst);
                        dispatch!(next);
                    }
                    Op::Div_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, /, =dst);
                        dispatch!(next);
                    }

                    Op::Rem_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, %, =dst);
                        dispatch!(next);
                    }
                    Op::Rem_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, %, =dst);
                        dispatch!(next);
                    }

                    Op::Compare_Eq_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, ==, =dst);
                        dispatch!(next);
                    }
                    Op::Compare_Eq_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, ==, =dst);
                        dispatch!(next);
                    }

                    Op::Compare_Ne_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, !=, =dst);
                        dispatch!(next);
                    }
                    Op::Compare_Ne_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, !=, =dst);
                        dispatch!(next);
                    }

                    Op::Compare_Gt_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, >, =dst);
                        dispatch!(next);
                    }
                    Op::Compare_Gt_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, >, =dst);
                        dispatch!(next);
                    }

                    Op::Compare_Lt_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, <, =dst);
                        dispatch!(next);
                    }
                    Op::Compare_Lt_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, <, =dst);
                        dispatch!(next);
                    }

                    Op::Compare_Ge_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, >=, =dst);
                        dispatch!(next);
                    }
                    Op::Compare_Ge_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, >=, =dst);
                        dispatch!(next);
                    }

                    Op::Compare_Le_I64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, <=, =dst);
                        dispatch!(next);
                    }
                    Op::Compare_Le_F64 {
                        token,
                        lhs,
                        rhs,
                        dst,
                    } => {
                        binary_op!(self, token, lhs, rhs, <=, =dst);
                        dispatch!(next);
                    }

                    Op::Minus_I64 { token, rhs, dst } => {
                        let rhs = self.vm.vstack.get(rhs, token);
                        self.vm.vstack.set(dst, -rhs);
                        dispatch!(next);
                    }
                    Op::Minus_F64 { token, rhs, dst } => {
                        let rhs = self.vm.vstack.get(rhs, token);
                        self.vm.vstack.set(dst, -rhs);
                        dispatch!(next);
                    }

                    Op::Not { token, rhs, dst } => {
                        let rhs = self.vm.vstack.get(rhs, token);
                        self.vm.vstack.set(dst, !rhs);
                        dispatch!(next);
                    }

                    Op::Call_Id { ret, callee } => {
                        let callee = self.functions.get_script(callee);
                        let ret_addr = self.vm.ip + 1;
                        let stack_size = callee.registers;
                        let new_frame = CallFrame {
                            callee,
                            ret_addr,
                            stack_base: current_frame.stack_base + ret.to_index(),
                        };

                        #[cfg(test)]
                        {
                            (self.vm.debug_hook)(DebugEvent::Call {
                                call_frame: &new_frame,
                            });
                        }

                        self.vm.ip = 0;
                        self.vm.vstack.base = new_frame.stack_base;
                        self.vm.vstack.reserve(stack_size);
                        self.vm
                            .cstack
                            .push(core::mem::replace(&mut current_frame, new_frame));

                        continue 'setup_frame;
                    }

                    Op::Call_Id_Host { callee, ret } => {
                        let callee = self.functions.get_host(callee);
                        let result = (callee.callback)(Scope {
                            state: State {
                                main: self.main.clone(),
                                functions: self.functions,
                                vm: &mut *self.vm,
                            },
                            // first argument lives at `ret+1`
                            args_base: 1 + ret.to_index(),
                        })?;
                        self.vm.vstack.set(ret, result);

                        dispatch!(next);
                    }

                    Op::Call_Reg { callee } => todo!(),

                    Op::Ret => match self.vm.cstack.pop() {
                        Some(prev_frame) => {
                            self.vm.ip = current_frame.ret_addr;
                            self.vm.vstack.base = prev_frame.stack_base;
                            current_frame = prev_frame;
                            continue 'setup_frame;
                        }
                        None => return Ok(self.vm.vstack.ret()),
                    },
                }
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
            let out = token.cast(value);
            debug_assert!(
                out.is_some(),
                "cast failed: {token:?} {:?}",
                core::ptr::read(self.pool.get_unchecked(src.to_index()))
            );
            out.unwrap_unchecked()
        }
    }
}

#[derive(Clone, Copy)]
struct Functions<'a> {
    script: &'a [Arc<Function>],
    host: &'a [ExternFunction],
}

impl<'a> Functions<'a> {
    fn new(script: &'a [Arc<Function>], host: &'a [ExternFunction]) -> Self {
        Self { script, host }
    }

    fn get_script(&self, src: FnId) -> Arc<Function> {
        unsafe { Arc::clone(self.script.get_unchecked(src.to_index())) }
    }

    fn get_host(&self, src: HostId) -> &ExternFunction {
        unsafe { self.host.get_unchecked(src.to_index()) }
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
    unsafe fn get_raw_by_base_relative_index(&self, index: usize) -> Value {
        let src = self.inner.get_unchecked(self.base + index);
        *src
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
            let out = token.cast(value);
            debug_assert!(out.is_some(), "cast failed: {token:?} {value:?}");
            out.unwrap_unchecked()
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
    pub(crate) script_fns: Vec<Arc<Function>>,
    pub(crate) host_fns: Vec<ExternFunction>,
}

impl AsRef<Module> for Module {
    fn as_ref(&self) -> &Module {
        self
    }
}

#[cfg(test)]
enum DebugEvent<'a> {
    Dispatch { op: Op, stack_frame: StackFrame<'a> },
    PostDispatch { stack_frame: StackFrame<'a> },
    Call { call_frame: &'a CallFrame },
}

#[cfg(test)]
struct StackFrame<'a>(&'a [Value]);

#[cfg(test)]
impl<'a> StackFrame<'a> {
    fn get(stack: &'a Stack, call_frame: &CallFrame) -> Self {
        Self(&stack.inner[stack.base..stack.base + call_frame.callee.registers as usize])
    }
}

#[cfg(test)]
type DebugHook = fn(DebugEvent<'_>);

pub mod token {
    #![allow(dead_code)]

    use crate::value::Literal;

    #[doc(hidden)]
    pub(crate) trait Cast<Input>: Copy + Clone + Sized + std::fmt::Debug {
        type Output: std::fmt::Debug;

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

    define_token!(Bool);
    define_token!(I64);
    define_token!(F64);
    define_token!(Value);
    define_token!(Offset);

    type V = super::Value;
    define_cast!(from V::Bool, to bool, using Bool);
    define_cast!(from V::I64, to i64, using I64);
    define_cast!(from V::F64, to f64, using F64);

    impl Cast<Literal> for Value {
        type Output = V;

        fn cast(self, value: Literal) -> Option<Self::Output> {
            match value {
                Literal::I64(v) => Some(V::I64(v)),
                Literal::F64(v) => Some(V::F64(v.into())),
                Literal::Str(_) => todo!(),
                Literal::Jmp(_) => None,
            }
        }
    }

    impl Cast<Literal> for Offset {
        type Output = isize;

        fn cast(self, value: Literal) -> Option<Self::Output> {
            match value {
                Literal::Jmp(value) => Some(value),
                _ => None,
            }
        }
    }
}

pub struct Scope<'a> {
    state: State<'a>,
    args_base: usize,
}

impl<'a> Scope<'a> {
    /// ### Safety
    /// This function does not check if the argument index is out of bounds.
    /// `index` must be in the range `0..N`, where `N` is the number of
    /// parameters the function expects.
    pub unsafe fn arg(&self, index: u8) -> Value {
        self.state
            .vm
            .vstack
            .get_raw_by_base_relative_index(self.args_base + index as usize)
    }
}

pub type ExternFunctionCallback = fn(Scope<'_>) -> Result<Value>;

/// Wrap a function
#[doc(hidden)]
#[macro_export]
macro_rules! __f {
    ($f:expr) => {
        |scope: $crate::vm::Scope<'_>| {
            let result = ($f)(scope);
            $crate::vm::CallbackResult::into_result(result)
        }
    };
}

pub use crate::__f as f;

pub trait CallbackResult: Sized {
    fn into_result(self) -> Result<Value>;
}

impl<T: Into<Value>> CallbackResult for Result<T> {
    fn into_result(self) -> Result<Value> {
        self.map(|v| v.into())
    }
}

impl<T: Into<Value>> CallbackResult for T {
    fn into_result(self) -> Result<Value> {
        Ok(self.into())
    }
}

#[cfg(test)]
mod tests;
