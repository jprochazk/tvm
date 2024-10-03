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
        debug_assert!(matches!(self, Self::I64(_)));
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
    JumpOffset(usize) = 0,
    I64(i64) = 1,
    F64(f64) = 2,
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
            Literal::F64(v) => Value::F64(v),
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
            Self::F64(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Literal::F64`
    #[inline]
    pub unsafe fn f64_unchecked(self) -> f64 {
        debug_assert!(!matches!(self, Literal::JumpOffset(_)));
        match self {
            Literal::F64(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }

    #[inline]
    pub fn jump_offset(self) -> Option<usize> {
        match self {
            Self::JumpOffset(v) => Some(v),
            _ => None,
        }
    }

    /// # Safety
    /// - `self` must be `Literal::JumpOffset`
    #[inline]
    pub unsafe fn jump_offset_unchecked(self) -> usize {
        debug_assert!(matches!(self, Literal::JumpOffset(_)));
        match self {
            Literal::JumpOffset(v) => v,
            _ => core::hint::unreachable_unchecked(),
        }
    }
}

impl Literal {
    #[inline]
    pub fn jmp(offset: usize) -> Self {
        Self::JumpOffset(offset)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_value_tags_match() {
        assert_eq!(Literal::I64(0).tag(), Value::I64(0).tag());
        assert_eq!(Literal::F64(0.0).tag(), Value::F64(0.0).tag());
    }
}
