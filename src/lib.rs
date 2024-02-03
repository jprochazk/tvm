#![allow(
    clippy::new_without_default,
    clippy::wrong_self_convention,
    clippy::new_ret_no_self
)]

#[macro_use]
pub mod error;
#[macro_use]
pub mod lex;
#[macro_use]
pub mod ast;
pub mod code;
pub mod hir;
pub mod rc;
pub mod syn;
pub mod ty;
pub mod value;

mod util;

pub type Str<'a> = beef::lean::Cow<'a, str>;
pub use std::borrow::Cow;
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<T> = rustc_hash::FxHashSet<T>;
