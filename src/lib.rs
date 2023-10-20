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
use rustc_hash::FxHashMap as HashMap;
type PreHash<T> = std::collections::HashMap<u64, T, nohash_hasher::BuildNoHashHasher<u64>>;
