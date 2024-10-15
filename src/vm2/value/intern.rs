#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(usize);

pub struct Interner {
    // Conceptually a `Vec<(len: usize, data: [u8; len])>`
    buffer: Vec<u8>,
}

impl Interner {
    pub fn new() -> Self {
        Self { buffer: Vec::new() }
    }

    pub fn intern(&mut self, data: &str) -> Id {
        let id = Id(self.buffer.len());
        self.buffer
            .extend_from_slice(&(data.len() as u32).to_le_bytes());
        self.buffer.extend_from_slice(data.as_bytes());
        id
    }

    pub fn get(&self, id: Id) -> &str {
        let encoded = &self.buffer[id.0 as usize..];
        let len = u32::from_le_bytes(encoded[..4].try_into().unwrap()) as usize;
        let bytes = &encoded[4..4 + len];
        // SAFETY: The bytes we obtained are guaranteed to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(bytes) }
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
