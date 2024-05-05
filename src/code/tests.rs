use crate::error::Error;
use crate::util::JoinIter as _;

fn report(e: Vec<Error>) -> String {
    e.into_iter().join("\n").to_string()
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
    literals,
    r#"
        let v = 0;
        let v = 1.0;
        let v = true;
        let v = false;
        let v = "test";
        let v = "\nyo\n";
    "#
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

test! {
    arithmetic,
    r#"
        let a = 1;
        let b = 2;
        let r = a + b; // Some(dst)
        a + b;         // None
        let r = a - b; // Some(dst)
        a - b;         // None
        let r = a * b; // Some(dst)
        a * b;         // None
        let r = a / b; // Some(dst)
        a / b;         // None
        let r = a % b; // Some(dst)
        a % b;         // None

        let a = 1.2;
        let b = 2.3;
        let r = a + b; // Some(dst)
        a + b;         // None
        let r = a - b; // Some(dst)
        a - b;         // None
        let r = a * b; // Some(dst)
        a * b;         // None
        let r = a / b; // Some(dst)
        a / b;         // None
        let r = a % b; // Some(dst)
        a % b;         // None
    "#
}

test! {
    functions,
    r#"
        fn f(v: int) -> int {
            v + 1
        }

        fn g() -> int {
            2
        }
    "#
}

test! {
    function_call_direct,
    r#"
        fn f(v: int) -> int {
            v
        }

        f(10);
    "#
}

test! {
    function_call_indirect,
    r#"
        fn f(v: int) -> int {
            v
        }

        let g = f;
        g(10);
    "#
}
