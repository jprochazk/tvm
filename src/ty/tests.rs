use super::DisplayHir;

fn _check(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{e}"),
    };
    match crate::ty::check(&ast) {
        Ok(hir) => DisplayHir(&hir).to_string(),
        Err(e) => format!("{e}"),
    }
}

macro_rules! check {
    ($input:literal) => {
        _check(indoc::indoc!($input))
    };
}

macro_rules! test {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            insta::assert_snapshot!(check!($input))
        }
    };
}

// TODO: uncomment all tests

test! {
    empty_type,
    r#"
        type T;
    "#
}

test! {
    extern_type_cons_not_callable,
    r#"
        extern type T;
        T();
    "#
}

test! {
    unit_not_callable,
    r#"
        (do {})();
    "#
}

test! {
    def_not_callable,
    r#"
        type T;
        let v = T();
        v();
    "#
}

test! {
    variables,
    r#"
        let v = 0;
        let v: int = 0;
    "#
}

test! {
    primitives,
    r#"
        0;
        1.2;
        true;
        "test";
    "#
}

test! {
    binop_numeric,
    r#"
        let ia: int = 1;
        let ib: int = 1;

        let fa: int = 1;
        let fb: int = 1;
        
        ia + ib;
        ia - ib;
        ia * ib;
        ia / ib;
        ia < ib;
        ia > ib;
        ia >= ib;
        ia <= ib;
        ia == ib;
        ia != ib;

        fa + fb;
        fa - fb;
        fa * fb;
        fa / fb;
        fa < fb;
        fa > fb;
        fa <= fb;
        fa >= fb;
        fa == fb;
        fa != fb;
    "#
}

test! {
    if_,
    r#"
        if true {
            0
        } else {
            1.0
        }

        let v = if true {0} else {1};
    "#
}

test! {
    bad_if,
    r#"
        let v = if true {0} else {1.0};
        let v: int = if true {1.0} else {1.0};
    "#
}

test! {
    if_in_fn,
    r#"
        fn test() -> int {
            if true { 0 } else { 1 }
        }
    "#
}

test! {
    return_,
    r#"
        fn a() {
            return
        }

        fn b() -> int {
            return 10
        }
    "#
}

test! {
    bad_return,
    r#"
        fn f() {
            return 10
        }
    "#
}

test! {
    bad_return_outside_fn,
    r#"
        return
    "#
}

test! {
    return_assign,
    r#"
        fn f() {
            // `return` is a basic block exit, can assign to anything
            let v: bool = return;
        }
    "#
}

test! {
    block_expr,
    r#"
        let v: int = do {0};
    "#
}

test! {
    bad_block_expr,
    r#"
        let v: int = do {};
    "#
}

test! {
    var_assign,
    r#"
        let v: int = 0;
        v = 10;
        v += 10;
    "#
}

test! {
    bad_var_assign,
    r#"
        let v: bool = true;
        v = 0;
        v += 10;

        let v: bool = true;
        v += true;
    "#
}

test! {
    bad_assign_to_fn_or_cons,
    r#"
        fn function() {}
        type Constructor;

        function = true;
        Constructor = true;
    "#
}

test! {
    group_span,
    r#"
        let v: bool = 1 * 2 + 3 * (4 + 5 * 6);
    "#
}

test! {
    unary_ops,
    r#"
        -1;
        -1.0;
        !true;
        !false;
    "#
}

test! {
    extern_fn_call,
    r#"
        extern fn add1(v: int) -> int;

        add1(1)
    "#
}

test! {
    bad_extern_fn_call,
    r#"
        extern fn add1(v: int) -> int;

        add1(true)
    "#
}

test! {
    extern_fn_call_dyn,
    r#"
        extern fn print(v: dynamic);

        print(10);
        print(10.0);
        print(true);
    "#
}

test! {
    bad_extern_fn_call_dyn,
    r#"
        extern fn print(v: dynamic);

        print();
    "#
}
