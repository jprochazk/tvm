use std::sync::Arc;

use super::operands::FunctionId;
use super::value::Value;
use super::{asm, dispatch, Error, Function, Module, Vm};

fn init_vm() -> Vm {
    // always test with starting stack size of `1` to
    // properly exercise the stack growth logic
    Vm::builder().initial_stack_size(1).build()
}

fn init_module(entry: FunctionId, functions: Vec<Function>) -> Arc<Module> {
    Arc::new(Module {
        entry,
        functions,
        external_functions: vec![],
    })
}

macro_rules! asm_function {
    ($name:ident ($registers:expr) {
        $($inst:ident $($operand:expr)*);*
        $(;)?
    }) => (Function::new(
        stringify!($name),
        vec![
            $(asm::$inst($($operand),*),)*
        ],
        vec![],
        $registers,
    ));
}

#[test]
fn manual_simple_arithmetic() {
    let m = init_module(
        0,
        vec![asm_function![add_i64(2) {
            load_i16 0 10;
            load_i16 1 10;
            add_i64 0 1 0;
            ret;
        }]],
    );

    let mut vm = init_vm();
    dispatch(&mut vm, &m).unwrap();

    assert!(matches!(vm.ret(), Value::I64(20)), "{:?}", vm.ret());
    assert_eq!(vm.stack_size(), 2);
}

#[test]
fn manual_recursive_fibonacci() {
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
            asm_function![main(3) {
                load_i16 r1 3i16;
                call r0 f1;
                load_i16 r2 7i16;
                call r1 f1;
                add_i64 r0 r0 r1;
                ret;
            }],
            asm_function![fib(5) {
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
    dispatch(&mut vm, &m).unwrap();

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
    let m = init_module(0, vec![asm_function![invalid_op(0) { invalid_op; }]]);

    let mut vm = init_vm();
    assert!(matches!(dispatch(&mut vm, &m), Err(Error::InvalidOp)));
}

// Compile-test:
#[test]
fn extern_() {
    use super::{function, Scope};

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

    function!(_unit);
    function!(_int);
    function!(_int2);
    function!(_num);
    function!(_bool);
}

fn _emit(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{e}"),
    };
    let hir = match crate::ty::check(&ast) {
        Ok(hir) => hir,
        Err(e) => panic!("{e}"),
    };
    let library = crate::Library::new();
    let module = match crate::code::compile(hir, &library) {
        Ok(m) => m,
        Err(e) => panic!("{e}"),
    };

    let mut vm = crate::Vm::new();
    match vm.run(&module) {
        Ok(value) => format!("{value:?}"),
        Err(e) => format!("error: {e}"),
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
            assert_snapshot!(emit!($input))
        }
    };
}

test! {
    i64,
    r#"
        123
    "#
}

test! {
    f64,
    r#"
        1.23
    "#
}

test! {
    variable,
    r#"
        let v = 0;
    "#
}

test! {
    add_i64,
    r#"
        10 + 10
    "#
}

test! {
    arithmetic_i64,
    r#"
        10 + 10 * 20 - 10 / 2
    "#
}

test! {
    add_f64,
    r#"
        10.5 + 20.5
    "#
}

test! {
    arithmetic_f64,
    r#"
        1.23 + 3.21 * 6.12 - 3.0 / 1.5
    "#
}

test! {
    simple_call,
    r#"
        fn add(a: int, b: int) -> int { a + b }

        add(2, 3)
    "#
}

test! {
    nested_call,
    r#"
        fn id(v: int) -> int { v }
        fn add(a: int, b: int) -> int { id(a) + id(b) }

        add(id(2), id(3))
    "#
}

test! {
    fibonacci,
    r#"
        fn fib(n: int) -> int {
            if n < 2 { n }
            else { fib(n - 1) + fib(n - 2) }
        }

        fib(25)
    "#
}

test! {
    assignment,
    r#"
        let v: int = 1;
        v += 1; // 2
        v /= 2; // 1
        v *= 2; // 2
        v -= 1; // 1
        v %= 1; // 0

        v
    "#
}

test! {
    count_loop,
    r#"
        let i = 0;
        loop {
            if i >= 10 { break }
            i += 1;
        }
        i
    "#
}

test! {
    bool_not,
    r#"
        let v = true;
        !v
    "#
}

test! {
    minus_i64,
    r#"
        let v = 1;
        -v
    "#
}

test! {
    minus_f64,
    r#"
        let v = 1.0;
        -v
    "#
}
