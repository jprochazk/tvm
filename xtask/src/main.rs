fn main() {
    let (cmd, args, dry_run) = {
        let mut args = std::env::args().skip(1);

        let Some(cmd) = args.next() else {
            help();
        };

        let mut args = args.collect::<Vec<_>>();
        for arg in &args {
            if arg == "-h" || arg == "--help" {
                help();
            }
        }
        let dry_run = match args.iter().position(|v| v == "--dry-run") {
            Some(i) => {
                args.remove(i);
                true
            }
            None => false,
        };

        (cmd, args, dry_run)
    };

    let mut ctx = Context::new();
    match cmd.as_str() {
        "setup" => setup(&mut ctx, &args),
        "test" => test(&mut ctx, &args),
        "miri" => miri(&mut ctx, &args),
        _ => help(),
    }

    if dry_run {
        ctx.dry_run();
    } else {
        ctx.run();
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

fn setup(ctx: &mut Context, _: &[String]) {
    cargo(ctx, "install cargo-binstall --locked");
    cargo(ctx, "binstall cargo-nextest --secure");
    rustup(ctx, "component add miri");
}

fn test(ctx: &mut Context, _: &[String]) {
    cargo(ctx, "nextest run -p vm");
    cargo(ctx, "nextest run -p vm --release");
}

fn miri(ctx: &mut Context, _: &[String]) {
    cargo(ctx, "miri nextest run -p vm");
    cargo(ctx, "miri nextest run -p vm --release");
}

struct Context {
    commands: Vec<Command>,
}

impl Context {
    fn new() -> Self {
        Self { commands: vec![] }
    }

    fn dry_run(&self) {
        for command in &self.commands {
            println!("$ {command}");
        }
    }

    fn run(&self) {
        for command in &self.commands {
            println!("$ {command}");
            command.run();
        }
    }
}

struct Command {
    name: &'static str,
    args: String,
}

impl Command {
    fn run(&self) {
        match std::process::Command::new(self.name)
            .args(self.args.split_ascii_whitespace().filter(|v| !v.is_empty()))
            .status()
        {
            Ok(status) => {
                if !status.success() {
                    eprintln!("command failed with status: {status}");
                }
            }
            Err(err) => eprintln!("error running command: {err}"),
        };
    }
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.args.is_empty() {
            write!(f, " {}", self.args)?;
        }
        Ok(())
    }
}

fn cargo(ctx: &mut Context, args: &str) {
    ctx.commands.push(Command {
        name: "cargo",
        args: args.to_owned(),
    });
}

fn rustup(ctx: &mut Context, args: &str) {
    ctx.commands.push(Command {
        name: "rustup",
        args: args.to_owned(),
    })
}
