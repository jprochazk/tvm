fn _emit(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{e}"),
    };
    let hir = match crate::ty::check(&ast) {
        Ok(hir) => hir,
        Err(e) => panic!("{e}"),
    };
    match crate::code::compile(hir) {
        Ok(m) => super::print::DisplayModule(&m, Some(input)).to_string(),
        Err(e) => e.to_string(),
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
    comparison,
    r#"
        let a = 1;
        let b = 2;

        a == b;
        a != b;
        a < b;
        a > b;
        a <= b;
        a >= b;
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

test! {
    basic_loop_0,
    r#"
        loop {}
    "#
}

test! {
    basic_loop_1,
    r#"
        loop {
            let v = 0;
        }
    "#
}

test! {
    basic_loop_continue,
    r#"
        loop {
            let v = 0;
            continue;
            let v = 1;
        }
    "#
}

test! {
    basic_loop_break,
    r#"
        loop {
            let v = 0;
            break;
            let v = 1;
        }
    "#
}

test! {
    if_expr,
    r#"
        if true {
            0
        } else if true {
            1
        } else {
            2
        }
    "#
}

test! {
    if_stmt,
    r#"
        if true {
            0
        } else if true {
            1
        } else {
            2
        };
    "#
}

test! {
    fibonacci,
    r#"
        fn fib(n: int) -> int {
            if n < 2 { n } else { fib(n - 1) + fib(n - 2) }
        }

        fib(15)
    "#
}
