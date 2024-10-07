use super::ops::{Opcode, RawOpcode};
use crate::ops::Instruction;

pub type Register = u8;

pub type LiteralId = u16;

pub type Offset = u16;

pub type FunctionId = u16;

pub type ExternFunctionId = u16;

/// 24-bit unsigned integer
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(non_camel_case_types)]
pub struct u24(u32);

impl u24 {
    pub const MAX: u24 = u24((2 << 24) - 1);
    pub const MIN: u24 = u24(0);

    #[inline]
    pub const fn new(v: u32) -> Self {
        assert!(v <= Self::MAX.0);
        Self(v)
    }

    #[inline]
    pub const fn get(self) -> u32 {
        self.0
    }

    #[inline]
    pub const fn to_le_bytes(self) -> [u8; 3] {
        let [a, b, c, _] = self.0.to_le_bytes();
        [a, b, c]
    }

    #[inline]
    pub const fn from_le_bytes(v: [u8; 3]) -> Self {
        let [a, b, c] = v;
        Self(u32::from_le_bytes([a, b, c, 0]))
    }
}

impl core::fmt::Debug for u24 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.0, f)
    }
}

impl core::fmt::Display for u24 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&self.0, f)
    }
}

pub trait SplitOperands<Operands>: Clone + Copy
where
    Operands: Clone + Copy,
{
    fn split(self) -> Operands;
}

pub trait JoinOperands: Clone + Copy {
    fn join(self) -> u24;
}

impl SplitOperands<()> for u24 {
    fn split(self) {}
}

impl JoinOperands for () {
    #[inline]
    fn join(self) -> u24 {
        u24::MIN
    }
}

impl SplitOperands<(u8,)> for u24 {
    #[inline]
    fn split(self) -> (u8,) {
        let [a, _, _] = self.to_le_bytes();
        (a,)
    }
}

impl JoinOperands for (u8,) {
    #[inline]
    fn join(self) -> u24 {
        let (a,) = self;
        u24::from_le_bytes([a, 0, 0])
    }
}

impl SplitOperands<(u8, u8)> for u24 {
    #[inline]
    fn split(self) -> (u8, u8) {
        let [a, b, _] = self.to_le_bytes();
        (a, b)
    }
}

impl JoinOperands for (u8, u8) {
    #[inline]
    fn join(self) -> u24 {
        let (a, b) = self;
        u24::from_le_bytes([a, b, 0])
    }
}

impl SplitOperands<(u8, u8, u8)> for u24 {
    #[inline]
    fn split(self) -> (u8, u8, u8) {
        let [a, b, c] = self.to_le_bytes();
        (a, b, c)
    }
}

impl JoinOperands for (u8, u8, u8) {
    #[inline]
    fn join(self) -> u24 {
        let (a, b, c) = self;
        u24::from_le_bytes([a, b, c])
    }
}

impl SplitOperands<(u16,)> for u24 {
    #[inline]
    fn split(self) -> (u16,) {
        let [a, b, _] = self.to_le_bytes();
        (u16::from_le_bytes([a, b]),)
    }
}

impl JoinOperands for (u16,) {
    #[inline]
    fn join(self) -> u24 {
        let (ab,) = self;
        let [a, b] = ab.to_le_bytes();
        u24::from_le_bytes([a, b, 0])
    }
}

impl SplitOperands<(u8, u16)> for u24 {
    #[inline]
    fn split(self) -> (u8, u16) {
        let [a, b_low, b_high] = self.to_le_bytes();
        (a, u16::from_le_bytes([b_low, b_high]))
    }
}

impl JoinOperands for (u8, u16) {
    #[inline]
    fn join(self) -> u24 {
        let (a, b) = self;
        let [b_low, b_high] = b.to_le_bytes();
        u24::from_le_bytes([a, b_low, b_high])
    }
}

impl SplitOperands<(u8, i16)> for u24 {
    #[inline]
    fn split(self) -> (u8, i16) {
        let [a, b_low, b_high] = self.to_le_bytes();
        (a, i16::from_le_bytes([b_low, b_high]))
    }
}

impl JoinOperands for (u8, i16) {
    #[inline]
    fn join(self) -> u24 {
        let (a, b) = self;
        let [b_low, b_high] = b.to_le_bytes();
        u24::from_le_bytes([a, b_low, b_high])
    }
}

/// `[ op : 8, a : 24 ]`
#[inline]
pub fn decode(encoded: Instruction) -> (RawOpcode, u24) {
    let [op, a, b, c] = encoded.0.to_le_bytes();
    (op, u24::from_le_bytes([a, b, c]))
}

/// `[ op : 8, a : 24 ]`
#[inline]
pub fn encode(opcode: Opcode, operands: impl JoinOperands) -> Instruction {
    let [a, b, c] = operands.join().to_le_bytes();
    Instruction(u32::from_le_bytes([opcode as u8, a, b, c]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_decode() {
        let opcode = Opcode::mov;
        let encoded = encode(opcode, (0x01, 0x02, 0x03));
        let decoded = decode(encoded);

        assert_eq!(
            encoded.0.to_le_bytes(),
            [Opcode::mov as u8, 0x01, 0x02, 0x03]
        );
        assert_eq!(decoded, (opcode as u8, (0x01, 0x02, 0x03).join()));
    }
}
