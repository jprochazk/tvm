fn _parse(input: &str) -> String {
  match super::try_parse(input) {
    Ok(ast) => format!("{ast:#?}"),
    Err(e) => e
      .into_iter()
      .map(|e| format!("{}", e.with_src(input)))
      .collect::<Vec<_>>()
      .join("\n"),
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
      insta::assert_snapshot!(parse!($input))
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
  opt_type,
  r#"
    let opt: int? = 1;
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
    let v: int? = none;
    let v: num = 1.0;
    let v: bool = true;
    let v: str = "yo";
    let v: [int] = [1, 2, 3];
    let v: [int] = [0; 10];
    let v: [_] = [0; 10];
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
    if a {0}
    else if b {1}
    else if c {2}
    else {0}

    let v: int? = if true {0};
    let v: int = if true {0} else {1};
  "#
}

test! {
  control_expressions,
  r#"
    return value;
    return;
    yield value;
    yield;
    break;
    continue;

    let v = return value;
    let v = return;
    let v = yield value;
    let v = yield;
    let v = break;
    let v = continue;

    let v = (yield 5) + (yield 5);
  "#
}

test! {
  postfix,
  r#"
    v.a[a](a);
    v(a)[a].a;
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
