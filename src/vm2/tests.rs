use std::sync::Arc;

use super::value::Value;
use super::{asm, dispatch, Context, Error, Function};

fn context(functions: Vec<Arc<Function>>) -> Context {
    // always test with starting stack size of `1` to
    // properly exercise the stack growth logic
    Context::builder()
        .functions(functions)
        .initial_stack_size(1)
        .build()
}

#[test]
fn add_i64() {
    let functions = vec![Arc::new(Function::new(
        vec![
            asm::load_i16(0, 10),
            asm::load_i16(1, 10),
            asm::add_i64(0, 1, 0),
            asm::ret(),
        ],
        vec![],
        2,
    ))];

    let mut context = context(functions);
    dispatch(&mut context, 0).unwrap();

    assert!(
        matches!(context.ret(), Value::I64(20)),
        "{:?}",
        context.ret()
    );
    assert_eq!(context.stack_size(), 2);
}

#[test]
fn fibonacci() {
    let r0 = 0u8;
    let r1 = 1u8;
    let r2 = 2u8;
    let r3 = 3u8;
    let r4 = 4u8;
    let f1 = 1u16;

    // fib(3) + fib(7)

    let functions = vec![
        Arc::new(Function::new(
            vec![
                asm::load_i16(r1, 3i16),
                asm::call(r0, f1),
                asm::load_i16(r2, 7i16),
                asm::call(r1, f1),
                asm::add_i64(r0, r0, r1),
                asm::ret(),
            ],
            vec![],
            3,
        )),
        Arc::new(Function::new(
            vec![
                asm::load_i16(r2, 2i16),
                asm::cmp_lt_i64(r0, r1, r2),
                asm::jump_if_false(r0, 3),
                asm::mov(r0, r1),
                asm::jump(9),
                asm::load_i16(r4, 1i16),
                asm::sub_i64(r3, r1, r4),
                asm::call(r2, f1),
                asm::mov(r0, r2),
                asm::load_i16(r4, 2i16),
                asm::sub_i64(r3, r1, r4),
                asm::call(r2, f1),
                asm::add_i64(r0, r0, r2),
                asm::ret(),
            ],
            vec![],
            5,
        )),
    ];

    let mut context = context(functions);

    dispatch(&mut context, 0).unwrap();

    assert_eq!(
        context.ret().i64().unwrap(),
        iter_fib(3) + iter_fib(7),
        "{:?}",
        context.ret()
    );
}

fn iter_fib(n: i64) -> i64 {
    let mut a = 0;
    let mut b = 1;
    for _ in 0..n {
        let c = a + b;
        a = b;
        b = c;
    }

    a
}

#[test]
fn invalid_op() {
    let functions = vec![Arc::new(Function::new(vec![asm::invalid_op()], vec![], 0))];

    let mut context = context(functions);

    assert!(matches!(dispatch(&mut context, 0), Err(Error::InvalidOp)));
}
