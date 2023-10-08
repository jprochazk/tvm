use super::Parser;

fn _parse(input: &str) -> String {
  match Parser::new(input).try_parse() {
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
  bin_add,
  r#"
    2 + 2
  "#
}

test! {
  function,
  r#"
    fn f() {}
  "#
}

test! {
  lone_fn_keyword,
  r#"
    fn
  "#
}

test! {
  block_expr,
  r#"
    let v = do { 1 };
  "#
}
