use super::op::*;

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
            v.push(unsafe { symbolic::Instruction::decode(buf) });
        }
        v
    }
}

#[test]
fn roundtrip_bytecode() {
    use asm::*;
    use symbolic::{self as sym, Instruction};

    let mut buf = Buffer::new();

    let r0 = Register::new(163);
    let c0 = Constant::new(12573);
    let smi = -5i8;

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
