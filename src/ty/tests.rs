use crate::error::Error;

fn report(e: Vec<Error>) -> String {
    e.into_iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn _check(input: &str) -> String {
    let ast = match crate::syn::try_parse(input) {
        Ok(ast) => ast,
        Err(e) => panic!("{}", report(e)),
    };
    match crate::ty::check(&ast) {
        Ok(hir) => format!("{hir:#?}"),
        Err(e) => report(e),
    }
}

macro_rules! check {
    ($input:literal) => {
        _check(($input))
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

// test! {
//     unit_not_callabe,
//     r#"
//         (do {})();
//     "#
// }

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
