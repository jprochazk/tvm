pub struct Instruction {
    /// Name of this instruction.
    pub name: Name,

    /// Operands encoded into the instruction stream.
    pub operands: Operands,

    /// How many values this instruction will pop from the stack.
    pub pop_n: usize,

    /// How many values this instruction will push onto the stack.
    pub push_n: usize,

    /// The type specializations of this instruction.
    pub variants: Variants,
}

pub type Name = &'static str;
pub type Operands = &'static [(Name, Operand)];
pub type Variants = &'static [Variant];
pub type Types = &'static [Type];

impl Instruction {
    pub const fn new(name: Name) -> Self {
        Self {
            name,
            operands: &[],
            pop_n: 0,
            push_n: 0,
            variants: &[],
        }
    }

    pub const fn set_operands(mut self, v: Operands) -> Self {
        self.operands = v;
        self
    }

    pub const fn set_pop_n(mut self, v: usize) -> Self {
        self.pop_n = v;
        self
    }

    pub const fn set_push_n(mut self, v: usize) -> Self {
        self.push_n = v;
        self
    }

    pub const fn set_variants(mut self, variants: &'static [Types]) -> Self {
        self.variants = unsafe { std::mem::transmute(variants) };
        self
    }
}

pub enum Operand {
    // Numeric operands
    I8,
    I16,

    /// Index of the instruction to jump to, specified
    /// as an offset relative to the instruction pointer.
    Rel,

    /// Index offset from the top of the stack.
    Local,

    /// Index into the constant pool.
    Const,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Variant {
    pub types: &'static [Type],
}

pub enum Type {
    Int,
    Num,
}
