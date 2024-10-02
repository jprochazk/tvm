use core::mem::transmute;

#[derive(Debug, Clone, Copy)]
#[repr(u64)]
pub enum Value {
    Unit = 0,
    I64(i64) = 1,
    F64(f64) = 2,
    Bool(bool) = 3,
}

/// `Value` is a tagged union due to the `repr(u64)` attribute,
/// and has the same layout as this type.
#[repr(C)]
struct RawTaggedUnion {
    tag: u64,
    value: u64,
}

impl Value {
    #[inline]
    pub fn tag(self) -> u64 {
        unsafe { transmute::<Value, RawTaggedUnion>(self).tag }
    }

    #[inline]
    pub fn unbox(self) -> Opaque {
        unsafe { Opaque(transmute::<Value, RawTaggedUnion>(self).value) }
    }
}

impl From<()> for Value {
    #[inline]
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl From<i64> for Value {
    #[inline]
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(value: f64) -> Self {
        Self::F64(value)
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u64)]
pub enum Literal {
    Jmp(usize) = 0,
    I64(i64) = 1,
    F64(f64) = 2,
}

impl Literal {
    /// # Safety
    /// - `literal` must not be `Literal::Jmp`
    #[inline]
    pub unsafe fn into_value(self) -> Value {
        debug_assert!(!matches!(self, Literal::Jmp(_)));
        match self {
            Literal::Jmp(_) => unsafe { core::hint::unreachable_unchecked() },
            Literal::I64(v) => Value::I64(v),
            Literal::F64(v) => Value::F64(v),
        }
    }

    #[inline]
    pub fn tag(self) -> u64 {
        unsafe { transmute::<Literal, RawTaggedUnion>(self).tag }
    }

    /// # Safety
    /// - `literal` must not be `Literal::Jmp`
    #[inline]
    pub unsafe fn unbox(self) -> Opaque {
        debug_assert!(!matches!(self, Literal::Jmp(_)));
        Opaque(transmute::<Literal, RawTaggedUnion>(self).value)
    }

    /// # Safety
    /// - `literal` must be `Literal::Jmp`
    #[inline]
    pub unsafe fn unbox_jmp(self) -> usize {
        debug_assert!(matches!(self, Literal::Jmp(_)));
        transmute::<Literal, RawTaggedUnion>(self).value as usize
    }
}

impl Literal {
    #[inline]
    pub fn jmp(offset: usize) -> Self {
        Self::Jmp(offset)
    }
}

impl From<i64> for Literal {
    #[inline]
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<f64> for Literal {
    #[inline]
    fn from(value: f64) -> Self {
        Self::F64(value)
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Opaque(u64);

impl Opaque {
    pub const UNIT: Opaque = Opaque(0);

    /// # Safety
    /// - `self` must be a valid `i64`
    #[inline]
    pub unsafe fn into_i64_unchecked(self) -> i64 {
        transmute::<u64, i64>(self.0)
    }

    #[inline]
    pub fn from_i64(v: i64) -> Self {
        unsafe { Self(transmute::<i64, u64>(v)) }
    }

    /// # Safety
    /// - `self` must be a valid `f64`
    #[inline]
    pub unsafe fn into_f64_unchecked(self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline]
    pub fn from_f64(v: f64) -> Self {
        Self(v.to_bits())
    }

    /// # Safety
    /// - `self` must be a valid `bool`
    #[inline]
    pub unsafe fn into_bool_unchecked(self) -> bool {
        #![allow(clippy::transmute_int_to_bool)]

        unsafe { transmute::<u8, bool>(self.0 as u8) }
    }

    #[inline]
    pub fn from_bool(v: bool) -> Self {
        Self(v as u8 as u64)
    }
}

impl From<i64> for Opaque {
    #[inline]
    fn from(value: i64) -> Self {
        Self::from_i64(value)
    }
}

impl From<f64> for Opaque {
    #[inline]
    fn from(value: f64) -> Self {
        Self::from_f64(value)
    }
}

impl From<bool> for Opaque {
    #[inline]
    fn from(value: bool) -> Self {
        Self::from_bool(value)
    }
}

impl From<Value> for Opaque {
    #[inline]
    fn from(value: Value) -> Self {
        value.unbox()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_value_tags_match() {
        assert_eq!(Literal::I64(0).tag(), Value::I64(0).tag());
        assert_eq!(Literal::F64(0.0).tag(), Value::F64(0.0).tag());
    }
}
