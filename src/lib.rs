#![allow(
    clippy::new_without_default,
    clippy::wrong_self_convention,
    clippy::new_ret_no_self
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
pub mod vm2;

mod util;

pub type Str<'a> = beef::lean::Cow<'a, str>;
pub use std::borrow::Cow;
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<T> = rustc_hash::FxHashSet<T>;

pub fn compile(s: &str) -> Result<code::CodeUnit, error::Report> {
    let ast = syn::try_parse(s)?;
    let hir = ty::check(&ast)?;
    code::compile(hir)
}

pub use code::{ExternFunctionDecl, ExternFunctionSig, Library};
pub use hir::Ty;
pub use vm2::value::Value;
pub use vm2::{f, Context, Scope};
