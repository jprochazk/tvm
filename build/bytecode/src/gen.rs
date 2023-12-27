use proc_macro2::Ident;

use crate::decl::*;

pub fn run(instructions: &[Instruction]) -> String {
    todo!()
}

macro_rules! q {
    ($buf:ident, [$($tt:tt)*]) => {{
        use std::fmt::Write;
        let _ = $buf.write_fmt(format_args!("{}\n", quote::quote!($($tt)*)));
    }};
}

fn opcode() {}

fn op() {}

fn asm() {}

fn sym() {}

/*
- encode
- decode
  - symbolic
  - opcode enum + inner ZST with `decode` method
- disasm


pub struct Local(usize);
pub struct Const(usize);

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Opcode {
    Nop(Nop) = 0,
    LoadLocal(op::LoadLocal) = 1,
    StoreLocal(op::StoreLocal) = 2,
    LoadConst(op::LoadConst) = 3,
}

pub mod op {
    //! Operand definitions for each instruction.

    #[derive(Clone, Copy)]
    pub struct Nop {}

    impl Encode for Nop {
        // ...
    }

    #[derive(Clone, Copy)]
    pub struct LoadLocal {
        pub offset: super::Local,
    }

    impl Encode for LoadLocal {
        // ...
    }

    #[derive(Clone, Copy)]
    pub struct StoreLocal {
        pub offset: super::Local,
    }

    impl Encode for StoreLocal {
        // ...
    }

    #[derive(Clone, Copy)]
    pub struct LoadConst {
        pub index: super::Const
    }

    impl Encode for LoadConst {
        // ...
    }
}

pub mod asm {
    //! Bytecode assembler

    use super::*;

    // impl<const N: usize, T: Encode> Encode for [T; N] { ... }

    pub fn nop() -> impl Encode {
        op::Nop {}
    }

    pub fn load_local(offset: Local) -> impl Encode {
        op::LoadLocal { offset }
    }

    pub fn store_local(offset: Local) -> impl Encode {
        op::StoreLocal { offset }
    }

    pub fn load_const(index: Const) -> impl Encode {
        op::LoadConst { index }
    }
}

pub mod sym {
    //! Symbolic representation of instructions.

    #[derive(Clone, Copy)]
    pub enum Instruction {
        Nop,
        LoadLocal { offset: Local },
        StoreLocal { offset: Local },
        LoadConst { index: Const },
    }

    impl Decode for Instruction {
        // ...
    }
}
*/
