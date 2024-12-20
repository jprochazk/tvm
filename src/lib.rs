#![allow(
    clippy::new_without_default,
    clippy::wrong_self_convention,
    clippy::new_ret_no_self,
    clippy::needless_lifetimes
)]

#[macro_use]
mod macros;

#[macro_use]
pub mod error;
#[macro_use]
pub mod lex;
#[macro_use]
pub mod ast;
pub mod code;
pub mod hir;
pub mod syn;
pub mod ty;
pub mod vm;

mod util;

pub use std::borrow::Cow;

pub use code::Library;
pub use hir::Ty;
pub use vm::value::Value;
pub use vm::{function, library, Module, Scope, Vm};

pub type Str<'a> = beef::lean::Cow<'a, str>;
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<T> = rustc_hash::FxHashSet<T>;

pub fn compile(s: &str) -> Result<Module, error::Report> {
    let ast = syn::try_parse(s)?;
    let hir = ty::check(&ast)?;
    code::compile(hir, &Library::new())
}

pub fn compile_with(s: &str, library: &Library) -> Result<Module, error::Report> {
    let ast = syn::try_parse(s)?;
    let hir = ty::check(&ast)?;
    code::compile(hir, library)
}
