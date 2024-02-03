use std::borrow::Cow;

use indexmap::IndexMap;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};

pub type Name = Cow<'static, str>;

#[derive(Clone, Default)]
pub struct Bytecode {
    pub ops: IndexMap<Name, Operands>,
    pub types: IndexMap<Name, Type>,
}

impl Bytecode {
    pub fn first(&self) -> (&Name, &Operands) {
        self.ops.iter().next().unwrap()
    }

    pub fn last(&self) -> (&Name, &Operands) {
        self.ops.iter().last().unwrap()
    }
}

pub type Operands = IndexMap<Name, Name>;

#[derive(Clone)]
pub struct Type {
    pub inner: Name,
    pub fmt: &'static str,
}

#[doc(hidden)]
pub(crate) fn ensure() -> MappedMutexGuard<'static, Bytecode> {
    static BYTECODE: Mutex<Option<Bytecode>> = Mutex::new(None);
    MutexGuard::map(BYTECODE.lock(), |v| v.get_or_insert_with(Bytecode::default))
}

pub fn finish() -> Bytecode {
    ensure().clone()
}

#[doc(hidden)]
#[macro_export]
macro_rules! __def {
    (
        $(op $name:ident($($arg:ident : $ty:ty),*))*
    ) => {{
        $(
            $crate::decl::ensure()
            .ops
            .insert(
                stringify!($name).into(),
                [$((stringify!($arg).into(), stringify!($ty).into())),*].into_iter().collect(),
            );
        )*
    }};

    (
        with $name:ident
        $([$($sub:ident),*])*
    ) => {{
        $(
            $name($(stringify!($sub)),*);
        )*
    }};

    (
        template $name:ident<$($sub:ident),*>
        $name_fmt:literal ($($arg:ident : $ty:ty),*)
    ) => {
        fn $name($($sub : impl Into<$crate::decl::Name>),*) {
            $crate::decl::ensure()
                .ops
                .insert(
                    format!($name_fmt, $($sub=($sub).into()),*).into(),
                    [$((stringify!($arg).into(), stringify!($ty).into())),*].into_iter().collect(),
                );
        }
    };

    (
        $(
            type $name:ident: $inner:ty = $fmt:literal
        )*
    ) => {{
        $(
            $crate::decl::ensure()
            .types
            .insert(
                stringify!($name).into(),
                $crate::decl::Type {
                    inner: stringify!($inner).into(),
                    fmt: $fmt,
                }
            );
        )*
    }};
}

pub use crate::__def as def;
