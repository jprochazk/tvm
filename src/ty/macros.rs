macro_rules! ty_p {
    ($tcx:ident) => {
        |ty: Ty| {
            TyPrinter {
                defs: &$tcx.defs,
                fns: &$tcx.fns,
            }
            .print(ty)
        }
    };
}
