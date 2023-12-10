fn _check(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => {
            panic!(
                "{}",
                e.into_iter()
                    .map(|e| format!("{e}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
    };

    match super::type_check(&ast) {
        Ok(db) => format!("{db}"),
        Err(e) => e
            .into_iter()
            .map(|e| format!("{e}"))
            .collect::<Vec<_>>()
            .join("\n"),
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

test! {
  literal_int,
  r#"
    let v = 10;
  "#
}

test! {
  literal_num,
  r#"
    let v = 10.5;
  "#
}

test! {
  literal_bool,
  r#"
    let a = true;
    let b = false;
  "#
}

test! {
  literal_str,
  r#"
    let v = "test";
  "#
}

test! {
  literal_array_empty,
  r#"
    let v = [];
  "#
}

test! {
  literal_array_single_item,
  r#"
    let v = [0];
  "#
}

test! {
  literal_array_len,
  r#"
    let v = [0; 16];
  "#
}

test! {
  use_var,
  r#"
    let v = 1;
    v;
  "#
}

test! {
  literal_array_type_inference,
  r#"
    let a = [];
    a.push(10);
  "#
}

/*
test! {
  binary_ops,
  r#"
    1 + 1; 1.5 + 1.5; "a" + "b";
    1 < 1; 1.5 < 1.5; "a" < "b";
    1 > 1; 1.5 > 1.5; "a" > "b";
    1 <= 1; 1.5 <= 1.5; "a" <= "b";
    1 >= 1; 1.5 >= 1.5; "a" >= "b";

    1 + 1; 1.5 + 1.5;
    1 - 1; 1.5 - 1.5;
    1 * 1; 1.5 * 1.5;
    1 / 1; 1.5 / 1.5;
    1 % 1; 1.5 % 1.5;
    1 ** 1; 1.5 ** 1.5;

    true && true; false && false;
    true || true; false || false;

    1 == 1; 1.5 == 1.5; "a" == "b";
    true == true; false == false;
    [] == []; none == none;

    1 != 1; 1.5 != 1.5; "a" != "b";
    true != true; false != false;
    [] != []; none != none;
  "#
}

test! {
  bad_binary_add,
  r#"
    10 + "test";
  "#
}

test! {
  let_stmt,
  r#"
    let v: int = 0;
  "#
}

test! {
  if_stmt,
  r#"
    let v0: int = if true { 0 } else { 1 };
    let v1: int? = if true { 0 };
  "#
}

test! {
  opt_subtyping,
  r#"
    let v0: int? = 1;
    let v1: [int]? = none;
    let v2: [int]? = [];
  "#
}

test! {
  function,
  r#"
    fn f0() { none }
    fn f1(a: int) { none }
    fn f2(a: int, b: int, c: int) { none }
    fn f3() -> int { 0 }
    fn f4(a: int) -> int { 0 }
    fn f5(a: int, b: int, c: int) -> int { 0 }
  "#
}

test! {
  variable_scope,
  r#"
    let v = 0;
    { let v = 0; }

    fn f() {
      let v = 0;
      { let v = 0; }
    }
  "#
}

test! {
  deeply_nested_subexpr,
  r#"
    1 + 1 + 1 + 1;
  "#
}

test! {
  arrays,
  r#"
    [];
    [0];
    [0; 16];
  "#
}

test! {
  array_bad_len,
  r#"
    [0; "a"];
  "#
}

test! {
  map_bad_key,
  r#"
    ({true});
    ({true: 0});
  "#
}

test! {
  literals,
  r#"
    let v0: _ = none;
    let v1: _ = 1;
    let v2: _ = 1.2;
    let v3: _ = true;
    let v4: _ = "test";
    let v5: _ = [];
    let v6: _ = [0];
    let v7: _ = [0; 16];
    let v8: _ = {0};
    let v9: _ = {0: "a"};
  "#
}

test! {
  control_expressions,
  r#"
    return 0;
    return;
    break;
    continue;

    let v0 = return 0;
    let v1 = return;
    let v4 = break;
    let v5 = continue;
  "#
}

test! {
  operators,
  r#"
    (2 + 2);
    (2 - 2);
    (2 / 2);
    (2 * 2);
    (2 % 2);
    (2 ** 2);
    (2 == 2);
    (2 != 2);
    (2 > 2);
    (2 >= 2);
    (2 < 2);
    (2 <= 2);
    (-2);
    (!true);
    (true && true);
    (false || true);
    (none ?? 0);
  "#
}

test! {
  variable_use,
  r#"
    let v = 0;
    (v + 5);
  "#
}

test! {
  collection_index,
  r#"
    let a = [0];
    (a[0] + 5);

    let m = {0: 0};
    (m[0] + 5);
  "#
}

test! {
  assign_flow_typing,
  r#"
    let v = 0;
    (v + 10);
    v = 5.0;
    (v + 10);
  "#
}

test! {
  collection_index_assign,
  r#"
    let a = [0];
    a[0] = 10;
    let m = {0: 0};
    m[0] = 10;
  "#
}
 */
