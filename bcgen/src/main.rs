mod decl;
mod gen;
pub mod support;

use std::env::args;
use std::fs;
use std::path::PathBuf;
use std::process::{exit, Command, ExitCode};

const USAGE: &str = "bcgen [input_path] [output_path]

Arguments:
  input_path    bytecode definition file path
  output_path   generated file path
";

fn main() -> ExitCode {
    let args = Args::parse_from_env();

    let input = std::fs::read_to_string(&args.input_path).expect("failed to read input file");
    let instructions = decl::parse(&input);

    let code = gen::run(&instructions);

    let now = chrono::Utc::now().to_rfc2822();
    let text = format!(
        "/*\
    \nThis file was generated by bcgen on {now}.\
    \nYou can regenerate it by running `cargo run -p bcgen -- src/code/op.x src/code/op.rs`.\
    \n*/"
    );
    let code = format!("{text}\n{code}");

    fs::write(&args.output_path, code).expect("failed to write output file");

    let result = Command::new("cargo")
        .args(["fmt", "--"])
        .arg(&args.output_path)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if !result.success() {
        eprintln!("failed to run `cargo fmt` on the generated file");
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

struct Args {
    input_path: PathBuf,
    output_path: PathBuf,
}

impl Args {
    fn parse_from_env() -> Self {
        let mut input_path = None;
        let mut output_path = None;

        for arg in args().skip(1) {
            match arg.as_str() {
                _ if input_path.is_none() => {
                    input_path = Some(PathBuf::from(arg));
                }
                _ if output_path.is_none() => {
                    output_path = Some(PathBuf::from(arg));
                }
                _ => {
                    eprintln!("too many positional arguments");
                    eprintln!("{USAGE}");
                    exit(1);
                }
            }
        }

        let Some(input_path) = input_path else {
            eprintln!("missing positional arguments: input_path, output_path");
            exit(1);
        };

        let Some(output_path) = output_path else {
            eprintln!("missing positional arguments: output_path");
            exit(1);
        };

        Self {
            input_path,
            output_path,
        }
    }
}
