use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::FromStr;

use crate::{HashMap, Str};

#[derive(Default)]
pub struct ConstantPoolBuilder<'src> {
    ints: HashMap<i64, u16>,
    nums: HashMap<f64n, u16>,
    strings: HashMap<Str<'src>, u16>,
    jump_offsets: HashMap<isize, u16>,
    pool: Vec<Constant>,
}

impl<'src> ConstantPoolBuilder<'src> {
    pub fn new() -> Self {
        Self {
            ints: HashMap::default(),
            nums: HashMap::default(),
            strings: HashMap::default(),
            jump_offsets: HashMap::default(),
            pool: Vec::new(),
        }
    }

    pub fn insert_int(&mut self, v: i64) -> Option<u16> {
        insert_or_get_in(&mut self.ints, &mut self.pool, v)
    }

    pub fn insert_num(&mut self, v: f64n) -> Option<u16> {
        insert_or_get_in(&mut self.nums, &mut self.pool, v)
    }

    pub fn insert_str(&mut self, v: Str<'src>) -> Option<u16> {
        insert_or_get_in(&mut self.strings, &mut self.pool, v)
    }

    pub fn insert_jump_offset(&mut self, v: isize) -> Option<u16> {
        insert_or_get_in(&mut self.jump_offsets, &mut self.pool, v)
    }

    pub fn finish(self) -> Vec<Constant> {
        self.pool
    }
}

fn insert_or_get_in<T: Clone + Eq + Hash + ToConstant>(
    m: &mut HashMap<T, u16>,
    p: &mut Vec<Constant>,
    v: T,
) -> Option<u16> {
    // too many constants
    if p.len() >= u16::MAX as usize {
        return None;
    }

    use std::collections::hash_map::Entry as E;
    let idx = match m.entry(v.clone()) {
        E::Occupied(e) => *e.get(),
        E::Vacant(e) => {
            let idx = *e.insert(p.len() as u16);
            p.push(v.to_constant());
            idx
        }
    };
    Some(idx)
}

#[derive(Debug)]
pub enum Constant {
    Int(i64),
    Num(f64n),
    Str(String),
    Jmp(isize),
}

/// An `f64` which is guaranteed to be non-NaN
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct f64n {
    value: u64,
}

impl f64n {
    /// # Panics
    /// If `value.is_nan()`
    pub fn new(value: f64) -> Self {
        assert!(!value.is_nan());

        Self {
            value: value.to_bits(),
        }
    }

    #[inline]
    pub fn get(self) -> f64 {
        f64::from_bits(self.value)
    }
}

impl Debug for f64n {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.get(), f)
    }
}

impl Display for f64n {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.get(), f)
    }
}

pub struct ParseFloatError {
    inner: ParseFloatErrorInner,
}

enum ParseFloatErrorInner {
    Std(std::num::ParseFloatError),
    Nan,
}

impl Debug for ParseFloatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            ParseFloatErrorInner::Std(e) => Debug::fmt(e, f),
            ParseFloatErrorInner::Nan => f
                .debug_struct("ParseFloatError")
                .field("kind", &"NaN")
                .finish(),
        }
    }
}

impl Display for ParseFloatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            ParseFloatErrorInner::Std(e) => Display::fmt(e, f),
            ParseFloatErrorInner::Nan => f.write_str("float literal must not be nan"),
        }
    }
}

impl std::error::Error for ParseFloatError {}

impl FromStr for f64n {
    type Err = ParseFloatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v = f64::from_str(s).map_err(|e| ParseFloatError {
            inner: ParseFloatErrorInner::Std(e),
        })?;
        if v.is_nan() {
            return Err(ParseFloatError {
                inner: ParseFloatErrorInner::Nan,
            });
        }
        Ok(f64n { value: v.to_bits() })
    }
}

trait ToConstant {
    fn to_constant(self) -> Constant;
}

impl ToConstant for i64 {
    fn to_constant(self) -> Constant {
        Constant::Int(self)
    }
}

impl ToConstant for f64n {
    fn to_constant(self) -> Constant {
        Constant::Num(self)
    }
}

impl ToConstant for Str<'_> {
    fn to_constant(self) -> Constant {
        Constant::Str(self.to_string())
    }
}

impl ToConstant for isize {
    fn to_constant(self) -> Constant {
        Constant::Jmp(self)
    }
}
