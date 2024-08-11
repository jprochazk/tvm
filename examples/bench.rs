use std::cell::Cell;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::process;
use std::time::Instant;

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

macro_rules! e {
    ($e:expr) => {
        Error($e.into())
    };
}

fn try_main() -> Result<()> {
    let filename = env::args().nth(1).ok_or(e!("missing argument: filename"))?;
    let file = fs::read_to_string(filename)?;
    run(&file)
}

fn run(file: &str) -> Result<()> {
    use tvm::*;

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

    fn benchmark_start(scope: Scope) {
        VALUE.set(Some(unsafe { scope.arg(0).to_i64_unchecked() }));
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

    static LIB: Library = unsafe {
        Library::from_static(&[
            ExternFunctionDecl {
                name: "benchmark_start",
                sig: ExternFunctionSig {
                    params: Cow::Borrowed(&[ty::Int.ty()]),
                    ret: Ty::Unit,
                },
                callback: f!(benchmark_start),
            },
            ExternFunctionDecl {
                name: "benchmark_end",
                sig: ExternFunctionSig {
                    params: Cow::Borrowed(&[]),
                    ret: Ty::Unit,
                },
                callback: f!(benchmark_end),
            },
            ExternFunctionDecl {
                name: "timer_start",
                sig: ExternFunctionSig {
                    params: Cow::Borrowed(&[]),
                    ret: Ty::Unit,
                },
                callback: f!(timer_start),
            },
            ExternFunctionDecl {
                name: "timer_end",
                sig: ExternFunctionSig {
                    params: Cow::Borrowed(&[]),
                    ret: Ty::Unit,
                },
                callback: f!(timer_end),
            },
        ])
    };

    let mut vm = Vm::new();
    let unit = tvm::compile(file)?;
    let module = unit.link_with(&LIB)?;

    vm.run(&module)?;

    Ok(())
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{e}");
        process::exit(1);
    }
}
