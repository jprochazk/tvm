#![allow(clippy::new_without_default, clippy::wrong_self_convention)]

#[macro_use]
pub mod error;

#[macro_use]
pub mod lex;

#[macro_use]
pub mod ast;

pub mod syn;

pub mod ty;

mod util;

use beef::lean::Cow;
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<T> = rustc_hash::FxHashSet<T>;
