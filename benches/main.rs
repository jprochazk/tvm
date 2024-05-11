mod fib;

use divan::{black_box, Bencher, Divan};

fn main() {
    Divan::from_args().threads([1]).main();
}

#[divan::bench(args = [5, 10, 15, 20, 25])]
fn fib(bencher: Bencher, n: usize) {
    let fib_src = include_str!("./fib.hebi");
    let src = format!(
        r#"
            {fib_src}

            fib({n})
        "#
    );

    let mut vm = tvm::Vm::new();
    let module = tvm::compile(&src).unwrap().link();

    bencher.bench_local(move || black_box(vm.run(&module)));
}

#[divan::bench(args = [5, 10, 15, 20, 25])]
fn native_fib(n: i32) -> i32 {
    fib::fib(black_box(n))
}
