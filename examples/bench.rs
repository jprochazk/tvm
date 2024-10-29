use std::cell::{Cell, RefCell};
use std::time::Instant;
use std::{env, fs, process};

use tvm::*;

macro_rules! e {
    ($e:expr) => {
        Error($e.into())
    };
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{e}");
        process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let filename = env::args().nth(1).ok_or(e!("missing argument: filename"))?;
    let file = fs::read_to_string(filename)?;
    run(&file)
}

fn run(file: &str) -> Result<()> {
    let mut vm = Vm::new();
    let m = tvm::compile_with(file, &BENCH)?;

    vm.run(&m)?;

    Ok(())
}

struct Scalar(f64);

impl std::fmt::Display for Scalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let units = ["s", "ms", "us", "ns"];
        let mut i = 0;
        let mut v = self.0;
        while v < 1.0 {
            i += 1;
            v *= 1000.0;
        }
        write!(f, "{v:.2}{}", units[i])
    }
}

thread_local! {
    static SAMPLES: RefCell<Vec<f64>> = const { RefCell::new(Vec::new()) };
    static VALUE: Cell<Option<i64>> = const { Cell::new(None) };
    static START: Cell<Option<Instant>> = const { Cell::new(None) };
}

fn benchmark_start(_: Scope, value: i64) {
    VALUE.set(Some(value));
    SAMPLES.with_borrow_mut(|s| s.clear())
}

fn benchmark_end(_: Scope) {
    SAMPLES.with_borrow_mut(|s| {
        let value = VALUE.get().unwrap();
        s.sort_unstable_by(|a, b| a.total_cmp(b));
        let mean = s.iter().copied().sum::<f64>() / s.len() as f64;
        let median = s[s.len() / 2];

        println!("{} mean={}, median={}", value, Scalar(mean), Scalar(median));
    })
}

fn timer_start(_: Scope) {
    START.set(Some(Instant::now()));
}

fn timer_end(_: Scope) {
    SAMPLES.with_borrow_mut(|s| s.push(START.get().unwrap().elapsed().as_secs_f64()))
}

tvm::library!(BENCH: benchmark_start, benchmark_end, timer_start, timer_end);

type Result<T, E = Box<dyn std::error::Error + 'static>> = std::result::Result<T, E>;

struct Error(std::borrow::Cow<'static, str>);
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}
impl std::error::Error for Error {}

impl From<&'static str> for Error {
    fn from(value: &'static str) -> Self {
        Error(value.into())
    }
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Error(value.into())
    }
}
