mod decl;
mod gen;

use decl::{Instruction, Operand, Type};

fn main() {
    let code = gen::run(&[
        Instruction::new("nop"),
        Instruction::new("local.get")
            .set_push_n(1)
            .set_operands(&[("offset", Operand::Local)]),
        Instruction::new("local.set")
            .set_pop_n(1)
            .set_operands(&[("offset", Operand::Local)]),
        Instruction::new("const")
            .set_push_n(1)
            .set_operands(&[("index", Operand::Const)]),
        arithmetic("add"),
        arithmetic("sub"),
        arithmetic("mul"),
        arithmetic("div"),
    ]);

    println!("{code}");
}

const fn arithmetic(name: &'static str) -> Instruction {
    Instruction::new(name)
        .set_pop_n(2)
        .set_push_n(1)
        .set_variants(&[&[Type::Int, Type::Int], &[Type::Num, Type::Num]])
}
