fn _check(input: &str) -> String {
  let ast = match crate::syn::try_parse(input) {
    Ok(ast) => ast,
    Err(e) => {
      panic!(
        "{}",
        e.into_iter()
          .map(|e| format!("{}", e.with_src(input)))
          .collect::<Vec<_>>()
          .join("\n")
      );
    }
  };

  match super::check(ast) {
    Ok(hir) => format!("{hir}"),
    Err(e) => e.with_src(input).to_string(),
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
  let_stmt,
  r#"
    let v: int = 0;
  "#
}

test! {
  if_stmt,
  r#"
    let v: int = if true {
      0
    } else {
      1
    };
  "#
}
