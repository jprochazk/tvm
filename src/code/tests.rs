use super::op::*;
use crate::error::Error;

fn report(e: Vec<Error>) -> String {
    e.into_iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn _emit(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{}", report(e)),
    };
    let hir = match crate::ty::check(&ast) {
        Ok(hir) => hir,
        Err(e) => panic!("{}", report(e)),
    };
    match crate::code::compile(hir) {
        Ok(m) => m.display(input).to_string(),
        Err(e) => report(e),
    }
}

macro_rules! emit {
    ($input:literal) => {
        _emit(indoc::indoc!($input))
    };
}

macro_rules! test {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            insta::assert_snapshot!(emit!($input))
        }
    };
}

test! {
    variables,
    r#"
        let a = 0;
        let b = 0;
    "#
}

test! {
    variable_shadowing,
    r#"
        let a = 0;
        let a = 0;
        let b = 0;
    "#
}

test! {
    variable_move,
    r#"
        let a = 0;
        let a = a;
        let b = 0;
    "#
}

test! {
    variable_copy,
    r#"
        let a = 0;
        let b = a;
    "#
}

struct Buffer {
    code: Vec<u8>,
}

impl Buffer {
    fn new() -> Self {
        Self { code: Vec::new() }
    }

    fn emit(&mut self, i: impl Encode) {
        i.encode(&mut self.code);
    }

    fn decode(&self) -> Vec<symbolic::Instruction> {
        let mut v = Vec::new();
        let buf = &mut self.code.as_slice();
        while !buf.is_empty() {
            v.push(unsafe { symbolic::Instruction::decode_unchecked(buf) });
        }
        v
    }
}

#[test]
fn roundtrip_bytecode() {
    use asm::*;
    use symbolic::{self as sym, Instruction};

    let mut buf = Buffer::new();

    let r0 = Reg::new(163);
    let c0 = Cst::new(12573);
    let smi = Smi::new(-5i8);

    buf.emit(nop());
    buf.emit(mov(r0, r0));
    buf.emit(load_cst(c0, r0));
    buf.emit(load_smi(smi, r0));
    buf.emit(load_true(r0));
    buf.emit(load_false(r0));

    assert_eq!(
        buf.decode(),
        [
            Instruction::Nop(sym::Nop {}),
            Instruction::Mov(sym::Mov { src: r0, dst: r0 }),
            Instruction::LoadCst(sym::LoadCst { src: c0, dst: r0 }),
            Instruction::LoadSmi(sym::LoadSmi { val: smi, dst: r0 }),
            Instruction::LoadTrue(sym::LoadTrue { dst: r0 }),
            Instruction::LoadFalse(sym::LoadFalse { dst: r0 }),
        ]
    );
}
