use std::ffi::OsStr;
use std::process::Command;

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

fn try_main() -> Result {
    let (cmd, args) = {
        let mut args = std::env::args().skip(1);

        let Some(cmd) = args.next() else {
            help();
        };
        if cmd == "-h" || cmd == "--help" {
            help();
        }

        let args = args.collect::<Vec<_>>();
        for arg in &args {
            if arg == "-h" || arg == "--help" {
                help();
            }
        }

        (cmd, args)
    };

    match cmd.as_str() {
        "setup" => setup(&args),
        "test" => test(&args),
        "miri" => miri(&args),
        _ => help(),
    }
}

/// Print usage information
fn help() -> ! {
    println!(
        "{}",
        r#"
Usage: xtask <command> [<args>...]

Commands:
    setup   Install tools needed for development
    test    Run tests
    miri    Run tests with miri

Global options:
    --dry-run   Print commands instead of running them
    -h, --help  Print this help message
"#
        .trim()
    );
    std::process::exit(0)
}

type Result<T = (), E = Box<dyn std::error::Error + 'static>> = std::result::Result<T, E>;

fn setup(_: &[String]) -> Result {
    cargo("install")
        .with_args(["cargo-binstall", "--locked"])
        .run()?;
    cargo("binstall").with_args(["cargo-insta", "-y"]).run()?;
    cargo("binstall").with_args(["cargo-nextest", "-y"]).run()?;
    rustup("component").with_args(["add", "miri"]).run()?;

    Ok(())
}

fn test(args: &[String]) -> Result {
    cargo("test").with_args(args).run()
}

fn miri(args: &[String]) -> Result {
    const MIRIFLAGS: &str = "-Zmiri-tree-borrows";
    cargo("miri")
        .with_args(["test"])
        .with_args(args)
        .with_envs([("MIRIFLAGS", MIRIFLAGS)])
        .run()
}

fn cargo(cmd: &str) -> Command {
    Command::new("cargo").with_arg(cmd)
}

fn rustup(cmd: &str) -> Command {
    Command::new("rustup").with_arg(cmd)
}

#[allow(dead_code)]
trait CommandExt {
    fn with_arg(self, arg: impl AsRef<OsStr>) -> Self;
    fn with_args(self, args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> Self;
    fn with_env(self, key: impl AsRef<OsStr>, value: impl AsRef<OsStr>) -> Self;
    fn with_envs(
        self,
        envs: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    ) -> Self;
    fn run(self) -> Result;
}

impl CommandExt for Command {
    fn with_arg(mut self, arg: impl AsRef<OsStr>) -> Self {
        self.arg(arg);
        self
    }

    fn with_args(mut self, args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> Self {
        self.args(args);
        self
    }

    fn with_env(mut self, key: impl AsRef<OsStr>, value: impl AsRef<OsStr>) -> Self {
        self.env(key, value);
        self
    }

    fn with_envs(
        mut self,
        envs: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    ) -> Self {
        self.envs(envs);
        self
    }

    fn run(mut self) -> Result {
        println!(
            "$ {} {} {}",
            self.get_envs()
                .map(|(k, v)| format!(
                    "{}={}",
                    k.to_string_lossy(),
                    v.map(|v| v.to_string_lossy()).unwrap_or_default()
                ))
                .collect::<Vec<_>>()
                .join(" "),
            self.get_program().to_string_lossy(),
            self.get_args()
                .map(|v| v.to_string_lossy())
                .collect::<Vec<_>>()
                .join(" ")
        );
        let success = self.spawn()?.wait()?.success();
        if !success {
            return Err(format!("command failed with status: {success}").into());
        }

        Ok(())
    }
}
