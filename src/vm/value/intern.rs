use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;

use hashbrown::hash_map::RawEntryMut;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl Symbol {
    #[inline]
    fn from_index(index: usize) -> Self {
        Symbol(unsafe { NonZeroU32::new_unchecked(index as u32 + 1) })
    }

    #[inline]
    fn index(&self) -> usize {
        self.0.get() as usize - 1
    }
}

/// Used to ensure that none of the `HashMap` methods
/// which perform hashing internally are used, as we
/// need to do all hashing/equality operations manually.
#[repr(transparent)]
#[derive(Clone, Copy)]
struct NoHash<T>(T);

pub struct Interner {
    table: hashbrown::HashMap<NoHash<Symbol>, (), ()>,
    /// Indexed by `Symbol`, stores strings prefixed by length.
    buffer: Vec<u8>,
}

#[inline]
fn fxhash_str_bytes(s: &[u8]) -> u64 {
    let mut hasher = rustc_hash::FxHasher::default();
    hasher.write(s);
    hasher.write_u8(0xFF); // maintain prefix-freedom
    hasher.finish()
}

/// # Safety
/// - `symbol` must have been created by a call to `intern` that holds the given
///   `buffer`.
#[inline]
unsafe fn get_bytes_unchecked(buffer: &[u8], symbol: Symbol) -> &[u8] {
    let idx = symbol.index();
    // SAFETY: The indexing here is guaranteed to not be
    //         out of bounds for the buffer.
    debug_assert!(idx + 4 < buffer.len(), "symbol out of bounds");
    let len = buffer.get_unchecked(idx..idx + 4);
    let len = u32::from_le_bytes(len.try_into().unwrap()) as usize;
    debug_assert!(idx + 4 + len <= buffer.len(), "symbol out of bounds");
    buffer.get_unchecked(idx + 4..idx + 4 + len)
}

#[inline]
fn get_bytes(buffer: &[u8], symbol: Symbol) -> Option<&[u8]> {
    let idx = symbol.index();
    let len = buffer.get(idx..idx + 4)?;
    let len = u32::from_le_bytes(len.try_into().unwrap()) as usize;
    buffer.get(idx + 4..idx + 4 + len)
}

impl Interner {
    #[inline]
    pub fn new() -> Self {
        Self {
            table: hashbrown::HashMap::with_hasher(()),
            buffer: Vec::new(),
        }
    }

    pub fn intern(&mut self, data: &str) -> Symbol {
        let hash = fxhash_str_bytes(data.as_bytes());
        let entry = self
            .table
            .raw_entry_mut()
            .from_hash(hash, |&NoHash(symbol)| unsafe {
                get_bytes_unchecked(&self.buffer, symbol) == data.as_bytes()
            });
        match entry {
            RawEntryMut::Occupied(entry) => entry.key().0,
            RawEntryMut::Vacant(entry) => {
                let symbol = Symbol::from_index(self.buffer.len());
                self.buffer
                    .extend_from_slice(&(data.len() as u32).to_le_bytes());
                self.buffer.extend_from_slice(data.as_bytes());

                let (key, _) =
                    entry.insert_with_hasher(hash, NoHash(symbol), (), |&NoHash(symbol)| unsafe {
                        fxhash_str_bytes(get_bytes_unchecked(&self.buffer, symbol))
                    });

                key.0
            }
        }
    }

    #[inline]
    pub fn get(&self, symbol: Symbol) -> &str {
        let Some(bytes) = get_bytes(&self.buffer, symbol) else {
            panic!("symbol used with different interner");
        };

        let Ok(str) = std::str::from_utf8(bytes) else {
            panic!("symbol used with different interner");
        };

        str
    }

    /// # Safety
    /// - `symbol` must have been created by a call to `intern` that holds the
    ///   given `buffer`.
    #[inline]
    pub unsafe fn get_unchecked(&self, symbol: Symbol) -> &str {
        std::str::from_utf8_unchecked(get_bytes_unchecked(&self.buffer, symbol))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_stuff() {
        let mut interner = Interner::new();
        let id = interner.intern("test");
        assert_eq!(interner.get(id), "test");
    }
}
