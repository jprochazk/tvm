macro_rules! p {
    ($tcx:expr, $ty:expr) => {
        $crate::ty::TyPrinter {
            defs: &$tcx.defs,
            fns: &$tcx.fns,
        }
        .print($ty)
    };
}

#[cfg(test)]
macro_rules! assert_snapshot {
    ($expr:expr) => {{
        {
            #[cfg(any(miri, feature = "__disable_snapshots"))]
            $expr;
        }

        {
            #[cfg(all(not(miri), not(feature = "__disable_snapshots")))]
            insta::assert_snapshot!($expr);
        }
    }};
}
