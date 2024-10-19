use super::Literal;
use crate::vm2::operands::LiteralId;

#[derive(Default)]
pub struct LiteralPool {
    inner: Vec<Literal>,
    table: hashbrown::HashMap<LiteralId, (), ()>,
}

fn hash_literal(literal: &Literal) -> u64 {
    use std::hash::{Hash, Hasher};

    let mut hasher = rustc_hash::FxHasher::default();
    literal.hash(&mut hasher);
    hasher.finish()
}

impl LiteralPool {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            table: hashbrown::HashMap::default(),
        }
    }

    pub fn insert(&mut self, literal: impl Into<Literal>) -> LiteralId {
        let literal = literal.into();

        let hash = hash_literal(&literal);
        let entry = self
            .table
            .raw_entry_mut()
            .from_hash(hash, |&id| literal == self.inner[id as usize]);
        match entry {
            hashbrown::hash_map::RawEntryMut::Occupied(entry) => *entry.key(),
            hashbrown::hash_map::RawEntryMut::Vacant(raw_vacant_entry_mut) => {
                let id = self.inner.len() as u16;
                raw_vacant_entry_mut
                    .insert_with_hasher(hash, id, (), |&id| hash_literal(&self.inner[id as usize]));
                self.inner.push(literal);
                id
            }
        }
    }
}
