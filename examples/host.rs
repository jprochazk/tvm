fn main() {
    let src = "
        extern fn print(v: dynamic);

        print(10);
        print(10.0);
        print(true);
    ";

    let mut vm = tvm::Vm::new();
    let module = tvm::compile_with(src, &IO).unwrap();
    vm.run(&module).unwrap();
}

fn print(_: tvm::Scope, arg: tvm::Value) {
    match arg {
        tvm::Value::Unit => println!("()"),
        tvm::Value::Bool(v) => println!("{v}"),
        tvm::Value::I64(v) => println!("{v}"),
        tvm::Value::F64(v) => println!("{v:.1}"),
    }
}

tvm::library!(IO: print);
