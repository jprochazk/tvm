use crate::vm::token;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Op {
    /// No-op.
    Nop,

    /// `dst = src`
    Mov { src: Reg, dst: Reg },

    /// `dst = literal_pool[src]`
    Load_Literal {
        token: token::ToValue,
        dst: Reg,
        src: Lit,
    },

    /// `dst = UNIT`
    Load_Unit { dst: Reg },

    /// `dst = function_table[id]`
    Load_Fn { dst: Reg, id: Fnid },

    /// `dst = sign_extend<i64>(val)`
    Load_I16 { dst: Reg, val: i16 },

    /// `dst = val`
    Load_Bool { val: bool, dst: Reg },

    /// Increment program counter by `offset`.
    Jump { offset: Rel },

    /// Increment program counter by offset stored in literal pool at `offset`.
    Jump_Long { token: token::ToOffset, offset: Lit },

    /// If `cond` is `false`, increment program counter by `offset`.
    JumpIfFalse { cond: Reg, offset: Rel },

    /// If `cond` is `false`, increment program counter by offset
    /// stored in literal pool at `offset`.
    JumpIfFalse_Long {
        token: token::ToOffset,
        cond: Reg,
        offset: Lit,
    },

    /// `dst = lhs + rhs`
    ///
    /// All operands are `i64`.
    Add_I64 {
        token: token::ToI64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs + rhs`
    ///
    /// All operands are `f64`.
    Add_F64 {
        token: token::ToF64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs - rhs`
    ///
    /// All operands are `i64`.
    Sub_I64 {
        token: token::ToI64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs - rhs`
    ///
    /// All operands are `f64`.
    Sub_F64 {
        token: token::ToF64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs * rhs`
    ///
    /// All operands are `i64`.
    Mul_I64 {
        token: token::ToI64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs * rhs`
    ///
    /// All operands are `f64`.
    Mul_F64 {
        token: token::ToF64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs / rhs`
    ///
    /// All operands are `i64`.
    ///
    /// Fails if `rhs` is zero.
    Div_I64 {
        token: token::ToI64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs / rhs`
    ///
    /// All operands are `f64`.
    ///
    /// Division by zero is defined by IEEE-754.
    Div_F64 {
        token: token::ToF64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs % rhs`
    ///
    /// All operands are `i64`.
    ///
    /// Fails if `rhs` is zero.
    Rem_I64 {
        token: token::ToI64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// `dst = lhs % rhs`
    ///
    /// All operands are `f64`.
    ///
    /// Division by zero is defined by IEEE-754.
    Rem_F64 {
        token: token::ToF64,
        lhs: Reg,
        rhs: Reg,
        dst: Reg,
    },

    /// ```text,ignore
    /// fn = function_table[callee]
    /// ret = call(fn, ret)
    /// ```
    Call_Id { ret: Reg, callee: Fnid },

    /// ```text,ignore
    /// fn = call(callee)
    /// ```
    ///
    /// `callee` is used as `dst`.
    Call_Reg { callee: Reg },

    /// Return from call.
    Ret,
}

pub enum Type {
    I64,
    F64,
}
pub use Type::*;

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
            token: unsafe { token::to_value() },
            src,
            dst,
        }
    }

    #[inline]
    pub fn load_unit(dst: Reg) -> Op {
        Op::Load_Unit { dst }
    }

    #[inline]
    pub fn load_fn(id: Fnid, dst: Reg) -> Op {
        Op::Load_Fn { id, dst }
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
                token: unsafe { token::to_offset() },
                offset,
            },
        }
    }

    #[inline]
    pub fn jmpf(cond: Reg, offset: Offset) -> Op {
        match offset {
            Offset::Rel(offset) => Op::JumpIfFalse { cond, offset },
            Offset::Cst(offset) => Op::JumpIfFalse_Long {
                token: unsafe { token::to_offset() },
                cond,
                offset,
            },
        }
    }

    #[inline]
    pub fn add(ty: Type, lhs: Reg, rhs: Reg, dst: Reg) -> Op {
        match ty {
            I64 => Op::Add_I64 {
                token: unsafe { token::to_i64() },
                lhs,
                rhs,
                dst,
            },
            F64 => Op::Add_F64 {
                token: unsafe { token::to_f64() },
                lhs,
                rhs,
                dst,
            },
        }
    }

    #[inline]
    pub fn sub(ty: Type, lhs: Reg, rhs: Reg, dst: Reg) -> Op {
        match ty {
            I64 => Op::Sub_I64 {
                token: unsafe { token::to_i64() },
                lhs,
                rhs,
                dst,
            },
            F64 => Op::Sub_F64 {
                token: unsafe { token::to_f64() },
                lhs,
                rhs,
                dst,
            },
        }
    }

    #[inline]
    pub fn mul(ty: Type, lhs: Reg, rhs: Reg, dst: Reg) -> Op {
        match ty {
            I64 => Op::Mul_I64 {
                token: unsafe { token::to_i64() },
                lhs,
                rhs,
                dst,
            },
            F64 => Op::Mul_F64 {
                token: unsafe { token::to_f64() },
                lhs,
                rhs,
                dst,
            },
        }
    }

    #[inline]
    pub fn div(ty: Type, lhs: Reg, rhs: Reg, dst: Reg) -> Op {
        match ty {
            I64 => Op::Div_I64 {
                token: unsafe { token::to_i64() },
                lhs,
                rhs,
                dst,
            },
            F64 => Op::Div_F64 {
                token: unsafe { token::to_f64() },
                lhs,
                rhs,
                dst,
            },
        }
    }

    #[inline]
    pub fn rem(ty: Type, lhs: Reg, rhs: Reg, dst: Reg) -> Op {
        match ty {
            I64 => Op::Rem_I64 {
                token: unsafe { token::to_i64() },
                lhs,
                rhs,
                dst,
            },
            F64 => Op::Rem_F64 {
                token: unsafe { token::to_f64() },
                lhs,
                rhs,
                dst,
            },
        }
    }

    #[inline]
    pub fn call_id(callee: Fnid, ret: Reg) -> Op {
        Op::Call_Id { callee, ret }
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

#[must_use = "unused Mvar"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Mvar(pub(crate) u16);

impl Mvar {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u16: TryFrom<T>,
    {
        <u16>::try_from(v).map(Mvar).ok()
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

impl std::fmt::Display for Mvar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{0}", self.0)
    }
}

#[must_use = "unused Fnid"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fnid(pub(crate) u16);

impl Fnid {
    #[inline]
    pub fn try_new<T>(v: T) -> Option<Self>
    where
        u16: TryFrom<T>,
    {
        <u16>::try_from(v).map(Fnid).ok()
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

impl std::fmt::Display for Fnid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "f{0}", self.0)
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Nop => write!(f, "nop "),
            Op::Mov { src, dst } => write!(f, "mov {src}, {dst}"),
            Op::Load_Literal { token: _, src, dst } => write!(f, "lit {src}, {dst}"),
            Op::Load_Unit { dst } => write!(f, "unit {dst}"),
            Op::Load_Fn { id, dst } => write!(f, "fn {id}, {dst}"),
            Op::Load_I16 { val, dst } => write!(f, "smi {val}, {dst}"),
            Op::Load_Bool { val, dst } => write!(f, "bool {val}, {dst}"),
            Op::Jump { offset } => write!(f, "jmp {offset}"),
            Op::Jump_Long { token: _, offset } => write!(f, "jmp {offset}"),
            Op::JumpIfFalse { cond, offset } => write!(f, "jmpf {cond}, {offset}"),
            Op::JumpIfFalse_Long {
                token: _,
                cond,
                offset,
            } => write!(f, "jmpf {cond}, {offset}"),
            Op::Add_I64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "add <i64> {lhs}, {rhs}, {dst}")
            }
            Op::Add_F64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "add <f64> {lhs}, {rhs}, {dst}")
            }
            Op::Sub_I64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "sub <i64> {lhs}, {rhs}, {dst}")
            }
            Op::Sub_F64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "sub <f64> {lhs}, {rhs}, {dst}")
            }
            Op::Mul_I64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "mul <i64> {lhs}, {rhs}, {dst}")
            }
            Op::Mul_F64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "mul <f64> {lhs}, {rhs}, {dst}")
            }
            Op::Div_I64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "div <i64> {lhs}, {rhs}, {dst}")
            }
            Op::Div_F64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "div <f64> {lhs}, {rhs}, {dst}")
            }
            Op::Rem_I64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "rem <i64> {lhs}, {rhs}, {dst}")
            }
            Op::Rem_F64 {
                token: _,
                lhs,
                rhs,
                dst,
            } => {
                write!(f, "rem <f64> {lhs}, {rhs}, {dst}")
            }
            Op::Call_Id { callee, ret } => {
                write!(f, "call {callee}, {ret}")
            }
            Op::Call_Reg { callee } => {
                write!(f, "call {callee}")
            }
            Op::Ret {} => write!(f, "ret"),
        }
    }
}
