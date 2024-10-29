use super::{ExternFunctionError, TryFromValue, ValueAbi};
use crate::hir;

pub mod intern;
pub mod pool;

#[derive(Default, Debug, Clone, Copy)]
#[repr(u64)]
pub enum Value {
    #[default]
    Unit = 0,
    I64(i64) = 1,
    F64(f64) = 2,
    Bool(bool) = 3,
}

impl Value {
    #[inline]
    pub fn tag(self) -> u64 {
        // SAFETY: Because `Self` is marked `repr(u64)`, its layout is a `repr(C)`
        // `union` between `repr(C)` structs, each of which has the `u64`
        // discriminant as its first field, so we can read the discriminant
        // without offsetting the pointer.
        unsafe {
            let ptr = &self as *const Self as *const u64;
            ptr.read()
        }
    }

    #[inline]
    pub fn unit(self) -> Option<()> {
        match self {
            Self::Unit => Some(()),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Self::Unit`
    #[inline]
    pub unsafe fn unit_unchecked(self) {
        match self {
            Value::Unit => (),
            _ => core::hint::unreachable_unchecked(),
        }
    }

    #[inline]
    pub fn i64(self) -> Option<i64> {
        match self {
            Self::I64(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Self::I64`
    #[inline]
    pub unsafe fn i64_unchecked(self) -> i64 {
        assert!(matches!(self, Self::I64(_)));
        match self {
            Value::I64(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }

    #[inline]
    pub fn f64(self) -> Option<f64> {
        match self {
            Self::F64(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Self::F64`
    #[inline]
    pub unsafe fn f64_unchecked(self) -> f64 {
        debug_assert!(matches!(self, Self::F64(_)));
        match self {
            Value::F64(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }

    #[inline]
    pub fn bool(self) -> Option<bool> {
        match self {
            Self::Bool(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Self::Bool`
    #[inline]
    pub unsafe fn bool_unchecked(self) -> bool {
        debug_assert!(matches!(self, Self::Bool(_)));
        match self {
            Value::Bool(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }
}

impl TryFromValue for Value {
    #[inline]
    fn try_from_value(value: Value) -> std::result::Result<Self, ExternFunctionError> {
        Ok(value)
    }
}

impl ValueAbi for Value {
    const TYPE: hir::Ty = hir::Ty::Dynamic;
}

impl From<()> for Value {
    #[inline]
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl TryFromValue for () {
    #[inline]
    fn try_from_value(value: Value) -> std::result::Result<Self, ExternFunctionError> {
        match value {
            Value::Unit => Ok(()),
            // TODO: proper error
            _ => Err(()),
        }
    }
}

impl ValueAbi for () {
    const TYPE: hir::Ty = hir::Ty::Unit;
}

impl From<i64> for Value {
    #[inline]
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl TryFromValue for i64 {
    #[inline]
    fn try_from_value(value: Value) -> std::result::Result<Self, ExternFunctionError> {
        match value {
            Value::I64(v) => Ok(v),
            // TODO: proper error
            _ => Err(()),
        }
    }
}

impl ValueAbi for i64 {
    const TYPE: hir::Ty = hir::Ty::INT;
}

impl From<f64> for Value {
    #[inline]
    fn from(value: f64) -> Self {
        Self::F64(value)
    }
}

impl TryFromValue for f64 {
    #[inline]
    fn try_from_value(value: Value) -> std::result::Result<Self, ExternFunctionError> {
        match value {
            Value::F64(v) => Ok(v),
            // TODO: proper error
            _ => Err(()),
        }
    }
}

impl ValueAbi for f64 {
    const TYPE: hir::Ty = hir::Ty::NUM;
}

impl From<bool> for Value {
    #[inline]
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl TryFromValue for bool {
    #[inline]
    fn try_from_value(value: Value) -> std::result::Result<Self, ExternFunctionError> {
        match value {
            Value::Bool(v) => Ok(v),
            // TODO: proper error
            _ => Err(()),
        }
    }
}

impl ValueAbi for bool {
    const TYPE: hir::Ty = hir::Ty::BOOL;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u64)]
pub enum Literal {
    JumpOffset(isize) = 0,
    I64(i64) = 1,
    F64(f64n) = 2,
}

impl Literal {
    /// # Safety
    /// - `literal` must not be `Literal::Jmp`
    #[inline]
    pub unsafe fn into_value(self) -> Value {
        debug_assert!(!matches!(self, Literal::JumpOffset(_)));
        match self {
            Literal::JumpOffset(_) => unsafe { core::hint::unreachable_unchecked() },
            Literal::I64(v) => Value::I64(v),
            Literal::F64(v) => Value::F64(v.into()),
        }
    }

    #[inline]
    pub fn tag(self) -> u64 {
        // SAFETY: Because `Self` is marked `repr(u64)`, its layout is a `repr(C)`
        // `union` between `repr(C)` structs, each of which has the `u64`
        // discriminant as its first field, so we can read the discriminant
        // without offsetting the pointer.
        unsafe {
            let ptr = &self as *const Self as *const u64;
            ptr.read()
        }
    }

    #[inline]
    pub fn i64(self) -> Option<i64> {
        match self {
            Self::I64(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Literal::I64`
    #[inline]
    pub unsafe fn i64_unchecked(self) -> i64 {
        debug_assert!(!matches!(self, Literal::JumpOffset(_)));
        match self {
            Literal::I64(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }

    #[inline]
    pub fn f64(self) -> Option<f64> {
        match self {
            Self::F64(v) => Some(v.get()),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Literal::F64`
    #[inline]
    pub unsafe fn f64_unchecked(self) -> f64 {
        debug_assert!(!matches!(self, Literal::JumpOffset(_)));
        match self {
            Literal::F64(v) => v.get(),
            _ => core::hint::unreachable_unchecked(),
        }
    }

    #[inline]
    pub fn jump_offset(self) -> Option<isize> {
        match self {
            Self::JumpOffset(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Literal::JumpOffset`
    #[inline]
    pub unsafe fn jump_offset_unchecked(self) -> isize {
        debug_assert!(matches!(self, Literal::JumpOffset(_)));
        match self {
            Literal::JumpOffset(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }
}

impl Literal {
    #[inline]
    pub fn jmp(offset: isize) -> Self {
        Self::JumpOffset(offset)
    }
}

impl From<i64> for Literal {
    #[inline]
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<f64n> for Literal {
    #[inline]
    fn from(value: f64n) -> Self {
        Self::F64(value)
    }
}

/// An `f64` which is guaranteed to be non-NaN
#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct f64n {
    value: f64,
}

impl f64n {
    pub const ZERO: f64n = f64n { value: 0.0 };

    /// # Panics
    /// If `value.is_nan()`
    #[inline]
    pub fn new(value: f64) -> Self {
        assert!(!value.is_nan());

        Self { value }
    }

    #[inline]
    pub fn try_new(value: f64) -> Option<Self> {
        if value.is_nan() {
            return None;
        }

        Some(Self { value })
    }

    #[inline]
    pub fn get(self) -> f64 {
        self.value
    }
}

impl std::fmt::Debug for f64n {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.get(), f)
    }
}

impl std::fmt::Display for f64n {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.get(), f)
    }
}

impl PartialEq for f64n {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value.to_bits() == other.value.to_bits()
    }
}

impl Eq for f64n {}

impl PartialOrd for f64n {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for f64n {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.to_bits().cmp(&other.value.to_bits())
    }
}

impl std::hash::Hash for f64n {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.to_bits().hash(state);
    }
}

impl From<f64n> for f64 {
    #[inline]
    fn from(value: f64n) -> Self {
        value.get()
    }
}

pub struct ParseFloatError {
    inner: ParseFloatErrorInner,
}

enum ParseFloatErrorInner {
    Std(std::num::ParseFloatError),
    Nan,
}

impl std::fmt::Debug for ParseFloatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            ParseFloatErrorInner::Std(e) => std::fmt::Debug::fmt(e, f),
            ParseFloatErrorInner::Nan => f
                .debug_struct("ParseFloatError")
                .field("kind", &"NaN")
                .finish(),
        }
    }
}

impl std::fmt::Display for ParseFloatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            ParseFloatErrorInner::Std(e) => std::fmt::Display::fmt(e, f),
            ParseFloatErrorInner::Nan => f.write_str("float literal must not be nan"),
        }
    }
}

impl std::error::Error for ParseFloatError {}

impl From<std::num::ParseFloatError> for ParseFloatError {
    #[inline]
    fn from(value: std::num::ParseFloatError) -> Self {
        ParseFloatError {
            inner: ParseFloatErrorInner::Std(value),
        }
    }
}

impl std::str::FromStr for f64n {
    type Err = ParseFloatError;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v = f64::from_str(s)?;
        f64n::try_new(v).ok_or(ParseFloatError {
            inner: ParseFloatErrorInner::Nan,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_value_tags_match() {
        assert_eq!(Literal::I64(0).tag(), Value::I64(0).tag());
        assert_eq!(Literal::F64(f64n::ZERO).tag(), Value::F64(0.0).tag());
    }
}
