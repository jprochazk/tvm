use std::sync::Arc;

use vm::*;

use ops::Function;
use value::Value;

macro_rules! op {
    ($name:ident, $operands:expr) => {
        ::vm::operands::encode(::vm::ops::Opcode::$name, $operands)
    };
}

#[test]
fn run() {
    let functions = vec![Arc::new(Function::new(
        vec![
            op!(load_i16, (0u8, 10i16)),
            op!(load_i16, (1u8, 10i16)),
            op!(add_i64, (0, 1, 0)),
            op!(ret, ()),
        ],
        vec![],
        2,
    ))];

    let mut context = ops::Context::new(functions);

    ops::dispatch(&mut context, 0).unwrap();

    assert!(
        matches!(context.ret(), Value::I64(20)),
        "{:?}",
        context.ret()
    );
}
