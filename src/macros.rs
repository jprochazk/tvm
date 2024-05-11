macro_rules! p {
    ($tcx:expr, $ty:expr) => {
        $crate::ty::TyPrinter {
            defs: &$tcx.defs,
            fns: &$tcx.fns,
        }
        .print($ty)
    };
}
