// #![no_std]
#![allow(clippy::new_without_default)]

extern crate alloc;

mod dyn_array;
pub mod operands;
pub mod ops;
pub mod value;

struct WriteStdout(std::io::Stdout);

impl core::fmt::Write for WriteStdout {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        use std::io::Write as _;

        self.0.write_all(s.as_bytes()).map_err(|_| core::fmt::Error)
    }
}

fn stdout() -> WriteStdout {
    WriteStdout(std::io::stdout())
}
