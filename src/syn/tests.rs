fn _parse(input: &str) -> String {
    match super::try_parse(input) {
        Ok(ast) => format!("{ast:#?}"),
        Err(e) => format!("{e}"),
    }
}

macro_rules! parse {
    ($input:literal) => {
        _parse(indoc::indoc!($input))
    };
}

macro_rules! test {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            assert_snapshot!(parse!($input))
        }
    };
}

test! {
    variables,
    r#"
        let v = 0;
        let v: int = 0;
    "#
}

test! {
    function,
    r#"
        fn f() {}
        fn f(a: A) {}
        fn f(a: A, b: B, c: C) {}
        fn f() -> R {}
        fn f(a: A) -> R {}
        fn f(a: A, b: B, c: C) -> R {}

        extern fn f();
    "#
}

test! {
    type_decl,
    r#"
        type T;
        type T();
        type T(a: A);
        type T(a: A,);
        type T(a: A, b: B, c: C);

        extern type T;
    "#
}

test! {
    lone_fn_keyword,
    r#"
        fn
    "#
}

test! {
    block_exprs,
    r#"
        let v = do {};
        let v = do {1};
        let v = do {1;};
        let v = do {do {}};
        let v = do {do {1}};
        let v = do {do {1;}};
    "#
}

test! {
    literals,
    r#"
        let v: int = 1;
        let v: num = 1.0;
        let v: bool = true;
        let v: str = "yo";
    "#
}

test! {
    operators,
    r#"
        2 + 2;
        2 - 2;
        2 / 2;
        2 * 2;
        2 % 2;
        2 ** 2;
        2 == 2;
        2 != 2;
        2 > 2;
        2 >= 2;
        2 < 2;
        2 <= 2;
        -2;
        !true;
        true && true;
        false || true;
        a ?? b;
        ?a.b
    "#
}

test! {
    assignment,
    r#"
        name = 1;
        name += 1;
        name -= 1;
        name /= 1;
        name *= 1;
        name %= 1;
        name **= 1;
        name ??= 1;
    "#
}

test! {
    calls,
    r#"
        f();
        f(0);
        f(0,);
        f(0, 1, 2);
        f(0, 1, 2,);
        f(a: 0);
        f(a: 0,);
        f(a: 0, b: 1, c: 2);
        f(a: 0, b: 1, c: 2,);
    "#
}

test! {
    partial_resilience,
    r#"
        f()
        f(); // still parsed
    "#
}

test! {
    if_,
    r#"
        if true {0}

        if a {0}
        else if b {1}
        else if c {2}
        else {0}

        let v: int = if true {0} else {1};
    "#
}

test! {
    control_expressions,
    r#"
        return value;
        return;
        break;
        continue;

        let v = return value;
        let v = return;
        let v = break;
        let v = continue;
    "#
}

test! {
    postfix,
    r#"
        v.a[a](a);
        v(a)[a].a;
        v.a();
    "#
}

test! {
    postfix_assign,
    r#"
        v.a = 1;
        v[a] = 2;
        v(a).a = 3;
        v(a)[a] = 4;
    "#
}

test! {
    bad_assign,
    r#"
        0 = 1;
    "#
}

test! {
    no_nested_fn,
    r#"
        fn test() {
            fn inner() {}
        }
    "#
}

test! {
    strings,
    r#"
        "";
        "\n";
        "\\n";
        "a\nb";
        "a\\nb";
    "#
}

test! {
    invalid_escapes,
    r#"
        "\{";
        "\}";
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
