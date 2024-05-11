use divan::{black_box, Bencher};

fn main() {
    divan::main();
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
fn fib_native(n: i32) -> i32 {
    #[inline(never)]
    fn fib(n: i32) -> i32 {
        if n < 2 {
            n
        } else {
            fib(n - 1) + fib(n - 2)
        }
    }

    fib(black_box(n))
}
