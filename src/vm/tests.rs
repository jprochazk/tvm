use std::cell::{Cell, RefCell};
use std::fmt::Write;
use std::panic::catch_unwind;

use super::*;
use crate::code::print::DisplayModule;
use crate::util::JoinIter as _;

thread_local! {
    static EVENTS: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    static COLLECT_EVENTS: Cell<bool> = const { Cell::new(true) };
}

fn debug_hook(event: DebugEvent) {
    if !COLLECT_EVENTS.get() {
        return;
    }

    const ALIGN: usize = 24;
    EVENTS.with_borrow_mut(|events| match event {
        DebugEvent::Dispatch { op, stack_frame } => {
            let op = op.to_string();
            events.push(format!("{op:ALIGN$} [{stack_frame}]"));
        }
        DebugEvent::PostDispatch { stack_frame } => {
            let prev = events.last_mut().unwrap();
            write!(prev, " -> [{stack_frame}]").unwrap();
        }
        DebugEvent::Call { call_frame } => events.push(format!("{}", call_frame)),
    })
}

fn get_panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    match payload.downcast::<&'static str>() {
        Ok(s) => String::from(*s),
        Err(payload) => match payload.downcast::<String>() {
            Ok(s) => *s,
            Err(_) => "<failed to get panic message>".into(),
        },
    }
}

fn _run(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{e}"),
    };
    let hir = match crate::ty::check(&ast) {
        Ok(hir) => hir,
        Err(e) => panic!("{e}"),
    };
    let code = match crate::code::compile(hir) {
        Ok(code) => code,
        Err(e) => panic!("{e}"),
    };

    let mut out = String::new();
    writeln!(
        &mut out,
        "## Module\n\n{}",
        DisplayModule(&code, Some(input))
    )
    .unwrap();

    write!(&mut out, "## Output\n\n").unwrap();
    let module = code.link().unwrap();
    match catch_unwind(|| Vm::new().run_debug(&module, debug_hook)) {
        Ok(Ok(value)) => {
            writeln!(&mut out, "{value:?}\n").unwrap();
        }
        Ok(Err(e)) => {
            writeln!(&mut out, "{e}\n").unwrap();
        }
        Err(payload) => {
            writeln!(&mut out, "PANIC: {}\n", get_panic_message(payload)).unwrap();
        }
    };

    if COLLECT_EVENTS.get() {
        EVENTS.with_borrow(|events| {
            writeln!(&mut out, "## Events\n\n{}", events.iter().join("\n")).unwrap();
        });
    }

    out
}

macro_rules! run {
    ($input:literal) => {
        _run(indoc::indoc!($input))
    };
}

macro_rules! test {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            assert_snapshot!(run!($input))
        }
    };
    ($name:ident, silent, $input:literal) => {
        #[test]
        fn $name() {
            COLLECT_EVENTS.set(false);
            assert_snapshot!(run!($input));
            COLLECT_EVENTS.set(true);
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
    silent,
    r#"
        fn fib(n: int) -> int {
            if n < 2 { n }
            else { fib(n - 1) + fib(n - 2) }
        }

        fib(3) + fib(7)
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
