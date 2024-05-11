macro_rules! ty_p {
    ($tcx:expr) => {
        |ty: $crate::ty::Ty| {
            $crate::ty::TyPrinter {
                defs: &$tcx.defs,
                fns: &$tcx.fns,
            }
            .print(ty)
        }
    };
}
