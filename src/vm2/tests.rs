use std::sync::Arc;

use super::operands::FunctionId;
use super::value::Value;
use super::{asm, dispatch, Error, Function, Module, Vm};
use crate::code;

fn init_vm() -> Vm {
    // always test with starting stack size of `1` to
    // properly exercise the stack growth logic
    Vm::builder().initial_stack_size(1).build()
}

fn init_module(entry: FunctionId, functions: Vec<code::Function>) -> Arc<Module> {
    Arc::new(Module {
        entry: 0,
        functions,
        external_functions: vec![],
    })
}

macro_rules! function {
    ($name:ident ($registers:expr) {
        $($inst:ident $($operand:expr)*);*
        $(;)?
    }) => (code::Function::test(
        stringify!($name),
        vec![
            $(asm::$inst($($operand),*),)*
        ],
        vec![],
        $registers,
    ));
}

#[test]
fn add_i64() {
    let m = init_module(
        0,
        vec![function![add_i64(2) {
            load_i16 0 10;
            load_i16 1 10;
            add_i64 0 1 0;
            ret;
        }]],
    );

    let mut vm = init_vm();
    dispatch(&mut vm, m).unwrap();

    assert!(matches!(vm.ret(), Value::I64(20)), "{:?}", vm.ret());
    assert_eq!(vm.stack_size(), 2);
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

    let m = init_module(
        0,
        vec![
            function![main(3) {
                load_i16 r1 3i16;
                call r0 f1;
                load_i16 r2 7i16;
                call r1 f1;
                add_i64 r0 r0 r1;
                ret;
            }],
            function![fib(5) {
                load_i16 r2 2i16;
                cmp_lt_i64 r0 r1 r2;
                jump_if_false r0 3;
                mov r0 r1;
                jump 9;
                load_i16 r4 1i16;
                sub_i64 r3 r1 r4;
                call r2 f1;
                mov r0 r2;
                load_i16 r4 2i16;
                sub_i64 r3 r1 r4;
                call r2 f1;
                add_i64 r0 r0 r2;
                ret;
            }],
        ],
    );

    let mut vm = init_vm();
    dispatch(&mut vm, m).unwrap();

    assert_eq!(
        vm.ret().i64().unwrap(),
        iter_fib(3) + iter_fib(7),
        "{:?}",
        vm.ret()
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
    let m = init_module(0, vec![function![invalid_op(0) { invalid_op; }]]);

    let mut vm = init_vm();
    assert!(matches!(dispatch(&mut vm, m), Err(Error::InvalidOp)));
}

// Compile-test:
#[test]
fn extern_() {
    use super::{f, Scope};

    fn _unit(_: Scope) {}
    fn _int(_: Scope, _: i64) -> i64 {
        0
    }
    fn _int2(_: Scope, _: i64, _: i64) -> i64 {
        0
    }
    fn _num(_: Scope, _: f64) -> f64 {
        0.0
    }
    fn _bool(_: Scope, _: bool) -> bool {
        false
    }

    f!(_unit);
    f!(_int);
    f!(_int2);
    f!(_num);
    f!(_bool);
}
