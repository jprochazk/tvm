use std::cell::RefCell;
use std::fmt::{Display, Write};

use super::*;
use crate::code::print::DisplayModule;
use crate::error::Error;
use crate::util::JoinIter as _;

fn report(e: Vec<Error>) -> String {
    e.into_iter().join("\n").to_string()
}

thread_local! {
    static EVENTS: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

struct DisplayValue<'a>(&'a Value);
impl Display for DisplayValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Value::Unit(_) => write!(f, "_"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::I64(v) => write!(f, "{v}i"),
            Value::F64(v) => write!(f, "{v}i"),
        }
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<frame {:?} base={}, ret={}>",
            self.callee.name, self.stack_base, self.ret_addr
        )
    }
}

impl Display for StackFrame<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(DisplayValue).join(", "))
    }
}

fn debug_hook(event: DebugEvent) {
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

fn _run(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{}", report(e)),
    };
    let hir = match crate::ty::check(&ast) {
        Ok(hir) => hir,
        Err(e) => panic!("{}", report(e)),
    };
    let code = match crate::code::compile(hir) {
        Ok(code) => code,
        Err(e) => panic!("{}", report(e)),
    };

    let mut out = String::new();
    writeln!(
        &mut out,
        "## Module\n\n{}",
        DisplayModule(&code, Some(input))
    )
    .unwrap();

    write!(&mut out, "## Output\n\n").unwrap();
    match code.link().debug(debug_hook) {
        Ok(value) => {
            writeln!(&mut out, "{value:?}\n").unwrap();
        }
        Err(e) => {
            writeln!(&mut out, "{}\n", report(vec![e])).unwrap();
        }
    };

    EVENTS.with_borrow(|events| {
        writeln!(&mut out, "## Events\n\n{}", events.iter().join("\n")).unwrap();
    });

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
            insta::assert_snapshot!(run!($input))
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
