fn main() {
    let src = "
        extern fn print(v: dynamic);

        print(10);
        print(10.0);
        print(true);
    ";

    let mut vm = tvm::Vm::new();
    let module = tvm::compile(src).unwrap().link_with(&PRINT).unwrap();
    vm.run(&module).unwrap();
}

fn print(scope: tvm::Scope) {
    match unsafe { scope.arg(0) } {
        tvm::Value::Unit(()) => println!("()"),
        tvm::Value::Bool(v) => println!("{v}"),
        tvm::Value::I64(v) => println!("{v}"),
        tvm::Value::F64(v) => println!("{v:.1}"),
    }
}

const PRINT: tvm::Library = unsafe {
    tvm::Library::from_static(&[tvm::ExternFunctionDecl {
        name: "print",
        sig: tvm::ExternFunctionSig {
            params: std::borrow::Cow::Borrowed(&[tvm::Ty::Dynamic]),
            ret: tvm::Ty::Unit,
        },
        callback: tvm::f!(print),
    }])
};
