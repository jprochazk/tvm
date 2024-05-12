use crate::vm::token;

// TODO: instead of storing token in the instruction,
//       store it in the operand, e.g. `Reg<i64>`

#[rustfmt::skip]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Op {
    /// No-op.
    Nop,

    /// `dst = src`
    Mov { src: Reg, dst: Reg },

    /// `dst = literal_pool[src]`
    Load_Literal { token: token::Value, dst: Reg, src: Lit },

    /// `dst = UNIT`
    Load_Unit { dst: Reg },

    /// `dst = function_table[id]`
    Load_Fn { dst: Reg, id: FnId },

    /// `dst = function_table[id]`
    Load_Fn_Host { dst: Reg, id: HostId },

    /// `dst = val as i64`
    Load_I16 { dst: Reg, val: i16 },

    /// `dst = val`
    Load_Bool { val: bool, dst: Reg },

    /// Increment program counter by `offset`.
    Jump { offset: Rel },

    /// Increment program counter by offset stored in literal pool at `offset`.
    Jump_Long { token: token::Offset, offset: Lit },

    /// If `cond` is `false`, increment program counter by `offset`.
    JumpIfFalse { ty: token::Bool, cond: Reg, offset: Rel },

    /// If `cond` is `false`, increment program counter by offset
    /// stored in literal pool at `offset`.
    JumpIfFalse_Long { ty: token::Bool, token: token::Offset, cond: Reg, offset: Lit },

    /// `dst = lhs + rhs`
    Add_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs + rhs`
    Add_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs - rhs`
    Sub_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs - rhs`
    Sub_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs * rhs`
    Mul_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs * rhs`
    Mul_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs / rhs`
    Div_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg }, 

    /// `dst = lhs / rhs`
    Div_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs % rhs`
    Rem_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs % rhs`
    Rem_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs == rhs`
    Compare_Eq_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs == rhs`
    Compare_Eq_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs != rhs`
    Compare_Ne_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs != rhs`
    Compare_Ne_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs > rhs`
    Compare_Gt_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs > rhs`
    Compare_Gt_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs < rhs`
    Compare_Lt_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs < rhs`
    Compare_Lt_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs >= rhs`
    Compare_Ge_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs >= rhs`
    Compare_Ge_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs <= rhs`
    Compare_Le_I64 { token: token::I64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = lhs <= rhs`
    Compare_Le_F64 { token: token::F64, lhs: Reg, rhs: Reg, dst: Reg },

    /// `dst = -rhs`
    Minus_I64 { token: token::I64, rhs: Reg, dst: Reg },

    /// `dst = -rhs`
    Minus_F64 { token: token::F64, rhs: Reg, dst: Reg },

    /// `dst = !dst`
    Not { token: token::Bool, rhs: Reg, dst: Reg },

    /// ```text,ignore
    /// fn = functions.script[callee]
    /// ret = call(fn, ret)
    /// ```
    Call_Id { ret: Reg, callee: FnId },

    /// ```text,ignore
    /// fn = functions.host[callee]
    /// ret = call(fn, ret)
    /// ```
    Call_Id_Host { ret: Reg, callee: HostId },

    /// ```text,ignore
    /// fn = call(callee)
    /// ```
    ///
    /// `callee` is used as `dst`.
    Call_Reg { callee: Reg },

    /// Return from call.
    Ret,
}

/// The following constant asserts that `size_of::<Op> == 4`.
///
/// If the "assertion" fails, then ensure that the total size
/// of the operands of any variant in `Op` do not add up to more
/// than 3 bytes. Additionally, the largest field must always be
/// _last_ in its variant.
///
/// For example:
///
/// ```rust,ignore
/// enum Op {
///   Call_Id { callee: Fnid, ret: Reg }
/// }
/// ```
///
/// `size_of::<Fnid> == 2`, so this should instead be:
///
/// ```rust,ignore
/// enum Op {
///   Call_Id { ret: Reg, callee: Fnid }
/// }
/// ```
const _: () = {
    let _ = core::mem::transmute::<Op, u32>;
};

impl Op {
    pub fn is_jump(self) -> bool {
        match self {
            Op::Jump { .. } | Op::JumpIfFalse { .. } => true,
            Op::Jump_Long { .. } | Op::JumpIfFalse_Long { .. } => false,
            _ => false,
        }
    }
}

pub mod asm {
    #![doc = " Bytecode assembler"]
    use super::*;

    #[inline]
    pub fn nop() -> Op {
        Op::Nop {}
    }

    #[inline]
    pub fn mov(src: Reg, dst: Reg) -> Op {
        Op::Mov { src, dst }
    }

    #[inline]
    pub fn load_cst(src: Lit, dst: Reg) -> Op {
        Op::Load_Literal {
            token: unsafe { token::value() },
            src,
            dst,
        }
    }

    #[inline]
    pub fn load_unit(dst: Reg) -> Op {
        Op::Load_Unit { dst }
    }

    #[inline]
    pub fn load_fn(id: FnId, dst: Reg) -> Op {
        Op::Load_Fn { id, dst }
    }

    #[inline]
    pub fn load_fn_host(id: HostId, dst: Reg) -> Op {
        Op::Load_Fn_Host { id, dst }
    }

    #[inline]
    pub fn load_i16(val: i16, dst: Reg) -> Op {
        Op::Load_I16 { val, dst }
    }

    #[inline]
    pub fn load_bool(val: bool, dst: Reg) -> Op {
        Op::Load_Bool { val, dst }
    }

    #[inline]
    pub fn jmp(offset: Offset) -> Op {
        match offset {
            Offset::Rel(offset) => Op::Jump { offset },
            Offset::Cst(offset) => Op::Jump_Long {
                token: unsafe { token::offset() },
                offset,
            },
        }
    }

    #[inline]
    pub fn jmpf(cond: Reg, offset: Offset) -> Op {
        match offset {
            Offset::Rel(offset) => Op::JumpIfFalse {
                ty: unsafe { token::bool() },
                cond,
                offset,
            },
            Offset::Cst(offset) => Op::JumpIfFalse_Long {
                ty: unsafe { token::bool() },
                token: unsafe { token::offset() },
                cond,
                offset,
            },
        }
    }

    macro_rules! binop {
        ($name:ident, $instruction_prefix:ident) => {
            #[inline]
            pub fn $name(ty: Type, lhs: Reg, rhs: Reg, dst: Reg) -> Op {
                paste::paste! {
                    match ty {
                        I64 => Op::[<$instruction_prefix _I64>] {
                            token: unsafe { token::i64() },
                            lhs,
                            rhs,
                            dst,
                        },
                        F64 => Op::[<$instruction_prefix _F64>] {
                            token: unsafe { token::f64() },
                            lhs,
                            rhs,
                            dst,
                        },
                    }
                }
            }
        };
    }

    binop!(add, Add);
    binop!(sub, Sub);
    binop!(mul, Mul);
    binop!(div, Div);
    binop!(rem, Rem);

    binop!(ceq, Compare_Eq);
    binop!(cne, Compare_Ne);
    binop!(cgt, Compare_Gt);
    binop!(clt, Compare_Lt);
    binop!(cge, Compare_Ge);
    binop!(cle, Compare_Le);

    macro_rules! unop {
        ($name:ident, $instruction_prefix:ident) => {
            #[inline]
            pub fn $name(ty: Type, rhs: Reg, dst: Reg) -> Op {
                paste::paste! {
                    match ty {
                        I64 => Op::[<$instruction_prefix _I64>] {
                            token: unsafe { token::i64() },
                            rhs,
                            dst,
                        },
                        F64 => Op::[<$instruction_prefix _F64>] {
                            token: unsafe { token::f64() },
                            rhs,
                            dst,
                        },
                    }
                }
            }
        };
    }

    unop!(mns, Minus);

    #[inline]
    pub fn not(rhs: Reg, dst: Reg) -> Op {
        Op::Not {
            token: unsafe { token::bool() },
            rhs,
            dst,
        }
    }

    pub enum Callee {
        Script(FnId),
        Host(HostId),
    }

    #[inline]
    pub fn call_id(callee: Callee, ret: Reg) -> Op {
        match callee {
            Callee::Script(callee) => Op::Call_Id { callee, ret },
            Callee::Host(callee) => Op::Call_Id_Host { callee, ret },
        }
    }

    #[inline]
    pub fn call_reg(callee: Reg) -> Op {
        Op::Call_Reg { callee }
    }

    #[inline]
    pub fn ret() -> Op {
        Op::Ret {}
    }
}

#[must_use = "unused Reg"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Reg(pub(crate) u8);
impl Reg {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u8: TryFrom<T>,
    {
        <u8>::try_from(v).map(Reg).ok()
    }

    #[inline]
    pub fn get(self) -> u8 {
        self.0
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{0}", self.0)
    }
}

#[must_use = "unused Cst"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Lit(pub(crate) u16);

impl Lit {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u16: TryFrom<T>,
    {
        <u16>::try_from(v).map(Lit).ok()
    }

    #[inline]
    pub fn get(self) -> u16 {
        self.0
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{0}]", self.0)
    }
}

#[must_use = "unused MVar"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MVar(pub(crate) u16);

impl MVar {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u16: TryFrom<T>,
    {
        <u16>::try_from(v).map(MVar).ok()
    }

    #[inline]
    pub fn get(self) -> u16 {
        self.0
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for MVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{0}", self.0)
    }
}

#[must_use = "unused FnId"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(pub(crate) u16);

impl FnId {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u16: TryFrom<T>,
    {
        <u16>::try_from(v).map(FnId).ok()
    }

    #[inline]
    pub fn get(self) -> u16 {
        self.0
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for FnId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "f{0}", self.0)
    }
}

#[must_use = "unused HostId"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HostId(pub(crate) u16);

impl HostId {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u16: TryFrom<T>,
    {
        <u16>::try_from(v).map(HostId).ok()
    }

    #[inline]
    pub fn get(self) -> u16 {
        self.0
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for HostId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "h{0}", self.0)
    }
}

#[must_use = "unused Rel"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rel(pub(crate) i16);

impl Rel {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        i16: TryFrom<T>,
    {
        <i16>::try_from(v).map(Rel).ok()
    }

    #[inline]
    pub fn get(self) -> i16 {
        self.0
    }

    #[inline]
    pub fn sign_extend(self) -> isize {
        self.0 as isize
    }
}

impl std::fmt::Display for Rel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.signum() {
            0 => write!(f, "0"),
            -1 => write!(f, "{}", self.0),
            1 => write!(f, "+{}", self.0),
            _ => unreachable!(),
        }
    }
}

pub mod ty {}

pub enum Type {
    I64,
    F64,
}
pub use Type::*;

#[derive(Clone, Copy)]
pub enum Offset {
    Rel(Rel),
    Cst(Lit),
}

impl Offset {
    pub fn placeholder() -> Self {
        Self::Rel(Rel(i16::MIN))
    }
}

impl std::fmt::Display for Op {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Nop => write!(f, "nop "),
            Op::Mov { src, dst } => write!(f, "mov {src}, {dst}"),

            Op::Load_Literal { token: _, src, dst } => write!(f, "lit {src}, {dst}"),
            Op::Load_Unit { dst } => write!(f, "unit {dst}"),
            Op::Load_Fn { id, dst } => write!(f, "fn {id}, {dst}"),
            Op::Load_Fn_Host { id, dst } => write!(f, "fn {id}, {dst}"),
            Op::Load_I16 { val, dst } => write!(f, "smi {val}, {dst}"),
            Op::Load_Bool { val, dst } => write!(f, "bool {val}, {dst}"),

            Op::Jump { offset } => write!(f, "jmp {offset}"),
            Op::Jump_Long { token: _, offset } => write!(f, "jmp {offset}"),
            Op::JumpIfFalse { ty: _, cond, offset } => write!(f, "jmpf {cond}, {offset}"),
            Op::JumpIfFalse_Long { ty: _, token: _, cond, offset } => write!(f, "jmpf {cond}, {offset}"),

            Op::Add_I64 { token: _, lhs, rhs, dst } => write!(f, "add.i64 {lhs}, {rhs}, {dst}"),
            Op::Add_F64 { token: _, lhs, rhs, dst } => write!(f, "add.f64 {lhs}, {rhs}, {dst}"),

            Op::Sub_I64 { token: _, lhs, rhs, dst } => write!(f, "sub.i64 {lhs}, {rhs}, {dst}"),
            Op::Sub_F64 { token: _, lhs, rhs, dst } => write!(f, "sub.f64 {lhs}, {rhs}, {dst}"),

            Op::Mul_I64 { token: _, lhs, rhs, dst } => write!(f, "mul.i64 {lhs}, {rhs}, {dst}"),
            Op::Mul_F64 { token: _, lhs, rhs, dst } => write!(f, "mul.f64 {lhs}, {rhs}, {dst}"),

            Op::Div_I64 { token: _, lhs, rhs, dst } => write!(f, "div.i64 {lhs}, {rhs}, {dst}"),
            Op::Div_F64 { token: _, lhs, rhs, dst } => write!(f, "div.f64 {lhs}, {rhs}, {dst}"),

            Op::Rem_I64 { token: _, lhs, rhs, dst } => write!(f, "rem.i64 {lhs}, {rhs}, {dst}"),
            Op::Rem_F64 { token: _, lhs, rhs, dst } => write!(f, "rem.f64 {lhs}, {rhs}, {dst}"),

            Op::Compare_Eq_I64 { token: _, lhs, rhs, dst } => write!(f, "ceq.i64 {lhs}, {rhs}, {dst}"),
            Op::Compare_Eq_F64 { token: _, lhs, rhs, dst } => write!(f, "ceq.f64 {lhs}, {rhs}, {dst}"),

            Op::Compare_Ne_I64 { token: _, lhs, rhs, dst } => write!(f, "cne.i64 {lhs}, {rhs}, {dst}"),
            Op::Compare_Ne_F64 { token: _, lhs, rhs, dst } => write!(f, "cne.f64 {lhs}, {rhs}, {dst}"),

            Op::Compare_Gt_I64 { token: _, lhs, rhs, dst } => write!(f, "cgt.i64 {lhs}, {rhs}, {dst}"),
            Op::Compare_Gt_F64 { token: _, lhs, rhs, dst } => write!(f, "cgt.f64 {lhs}, {rhs}, {dst}"),

            Op::Compare_Lt_I64 { token: _, lhs, rhs, dst } => write!(f, "clt.i64 {lhs}, {rhs}, {dst}"),
            Op::Compare_Lt_F64 { token: _, lhs, rhs, dst } => write!(f, "clt.f64 {lhs}, {rhs}, {dst}"),

            Op::Compare_Ge_I64 { token: _, lhs, rhs, dst } => write!(f, "cge.i64 {lhs}, {rhs}, {dst}"),
            Op::Compare_Ge_F64 { token: _, lhs, rhs, dst } => write!(f, "cge.f64 {lhs}, {rhs}, {dst}"),

            Op::Compare_Le_I64 { token: _, lhs, rhs, dst } => write!(f, "cle.i64 {lhs}, {rhs}, {dst}"),
            Op::Compare_Le_F64 { token: _, lhs, rhs, dst } => write!(f, "cle.f64 {lhs}, {rhs}, {dst}"),

            Op::Minus_I64 { token: _, rhs, dst } => write!(f, "mns.i64 {rhs}, {dst}"),
            Op::Minus_F64 { token: _, rhs, dst } => write!(f, "mns.f64 {rhs}, {dst}"),

            Op::Not { token: _, rhs, dst } => write!(f, "not {rhs}, {dst}"),

            Op::Call_Id { callee, ret } => write!(f, "call {callee}, {ret}"),
            Op::Call_Id_Host { callee, ret } => write!(f, "call {callee}, {ret}"),
            Op::Call_Reg { callee } => write!(f, "call {callee}"),

            Op::Ret {} => write!(f, "ret"),
        }
    }
}
