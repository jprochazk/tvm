pub trait Encoder {
    fn encode_slice(&mut self, v: &[u8]);

    #[inline]
    fn encode_u8(&mut self, v: u8) {
        self.encode_slice(&v.to_le_bytes());
    }

    #[inline]
    fn encode_u16(&mut self, v: u16) {
        self.encode_slice(&v.to_le_bytes());
    }

    #[inline]
    fn encode_u32(&mut self, v: u32) {
        self.encode_slice(&v.to_le_bytes());
    }

    #[inline]
    fn encode_u64(&mut self, v: u64) {
        self.encode_slice(&v.to_le_bytes());
    }

    fn encode_i8(&mut self, v: i8) {
        self.encode_slice(&v.to_le_bytes());
    }

    #[inline]
    fn encode_i16(&mut self, v: i16) {
        self.encode_slice(&v.to_le_bytes());
    }

    #[inline]
    fn encode_i32(&mut self, v: i32) {
        self.encode_slice(&v.to_le_bytes());
    }

    #[inline]
    fn encode_i64(&mut self, v: i64) {
        self.encode_slice(&v.to_le_bytes());
    }
}

pub trait Encode {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder;
}

pub trait Decoder {
    /// # Safety
    /// `self` must have enough bytes left to decode a `u8`.
    unsafe fn decode_u8_unchecked(&mut self) -> u8;

    /// # Safety
    /// `self` must have enough bytes left to decode a `u16`.
    unsafe fn decode_u16_unchecked(&mut self) -> u16;

    /// # Safety
    /// `self` must have enough bytes left to decode a `u32`.
    unsafe fn decode_u32_unchecked(&mut self) -> u32;

    /// # Safety
    /// `self` must have enough bytes left to decode a `u64`.
    unsafe fn decode_u64_unchecked(&mut self) -> u64;

    /// # Safety
    /// `self` must have enough bytes left to decode an `i8`.
    unsafe fn decode_i8_unchecked(&mut self) -> i8;

    /// # Safety
    /// `self` must have enough bytes left to decode an `i16`.
    unsafe fn decode_i16_unchecked(&mut self) -> i16;

    /// # Safety
    /// `self` must have enough bytes left to decode an `i32`.
    unsafe fn decode_i32_unchecked(&mut self) -> i32;

    /// # Safety
    /// `self` must have enough bytes left to decode an `i64`.
    unsafe fn decode_i64_unchecked(&mut self) -> i64;
}

pub trait Decode: Sized {
    /// # Safety
    /// `dec` must have at least `size_of::<Self>` bytes left.
    unsafe fn decode(dec: &mut impl Decoder) -> Self;
}

impl Encoder for Vec<u8> {
    #[inline]
    fn encode_slice(&mut self, v: &[u8]) {
        self.extend_from_slice(v);
    }
}

#[inline(always)]
unsafe fn unsafe_advance(slice: &mut &[u8], n: usize) {
    use std::ops::Sub;
    use std::slice::from_raw_parts;

    debug_assert!(n <= slice.len());
    let (ptr, len) = ((*slice).as_ptr(), (*slice).len());
    let remainder = from_raw_parts(ptr.add(n), len.sub(n));
    *slice = remainder;
}

impl Decoder for &'_ [u8] {
    #[inline(always)]
    unsafe fn decode_u8_unchecked(&mut self) -> u8 {
        debug_assert!(self.len() >= std::mem::size_of::<u8>());
        let v = u8::from_le_bytes([*self.get_unchecked(0)]);
        unsafe_advance(self, std::mem::size_of::<u8>());
        v
    }

    #[inline(always)]
    unsafe fn decode_u16_unchecked(&mut self) -> u16 {
        debug_assert!(self.len() >= std::mem::size_of::<u16>());
        let v = u16::from_le_bytes([*self.get_unchecked(0), *self.get_unchecked(1)]);
        unsafe_advance(self, std::mem::size_of::<u16>());
        v
    }

    #[inline(always)]
    unsafe fn decode_u32_unchecked(&mut self) -> u32 {
        debug_assert!(self.len() >= std::mem::size_of::<u32>());
        let v = u32::from_le_bytes([
            *self.get_unchecked(0),
            *self.get_unchecked(1),
            *self.get_unchecked(2),
            *self.get_unchecked(3),
        ]);
        unsafe_advance(self, std::mem::size_of::<u32>());
        v
    }

    #[inline(always)]
    unsafe fn decode_u64_unchecked(&mut self) -> u64 {
        debug_assert!(self.len() >= std::mem::size_of::<u64>());
        let v = u64::from_le_bytes([
            *self.get_unchecked(0),
            *self.get_unchecked(1),
            *self.get_unchecked(2),
            *self.get_unchecked(3),
            *self.get_unchecked(4),
            *self.get_unchecked(5),
            *self.get_unchecked(6),
            *self.get_unchecked(7),
        ]);
        unsafe_advance(self, std::mem::size_of::<u64>());
        v
    }

    #[inline(always)]
    unsafe fn decode_i8_unchecked(&mut self) -> i8 {
        debug_assert!(self.len() >= std::mem::size_of::<i8>());
        let v = i8::from_le_bytes([*self.get_unchecked(0)]);
        unsafe_advance(self, std::mem::size_of::<i8>());
        v
    }

    #[inline(always)]
    unsafe fn decode_i16_unchecked(&mut self) -> i16 {
        debug_assert!(self.len() >= std::mem::size_of::<i16>());
        let v = i16::from_le_bytes([*self.get_unchecked(0), *self.get_unchecked(1)]);
        unsafe_advance(self, std::mem::size_of::<i16>());
        v
    }

    #[inline(always)]
    unsafe fn decode_i32_unchecked(&mut self) -> i32 {
        debug_assert!(self.len() >= std::mem::size_of::<i32>());
        let v = i32::from_le_bytes([
            *self.get_unchecked(0),
            *self.get_unchecked(1),
            *self.get_unchecked(2),
            *self.get_unchecked(3),
        ]);
        unsafe_advance(self, std::mem::size_of::<i32>());
        v
    }

    #[inline(always)]
    unsafe fn decode_i64_unchecked(&mut self) -> i64 {
        debug_assert!(self.len() >= std::mem::size_of::<i64>());
        let v = i64::from_le_bytes([
            *self.get_unchecked(0),
            *self.get_unchecked(1),
            *self.get_unchecked(2),
            *self.get_unchecked(3),
            *self.get_unchecked(4),
            *self.get_unchecked(5),
            *self.get_unchecked(6),
            *self.get_unchecked(7),
        ]);
        unsafe_advance(self, std::mem::size_of::<i64>());
        v
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(u8);

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl Register {
    #[inline]
    pub fn new(v: u8) -> Self {
        Self(v)
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl Encode for Register {
    #[inline]
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_u8(self.0)
    }
}

impl Decode for Register {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        Self(dec.decode_u8_unchecked())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant(u16);

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "c{}", self.0)
    }
}

impl Constant {
    #[inline]
    pub fn new(v: u16) -> Self {
        Self(v)
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.0 as usize
    }
}

impl Encode for Constant {
    #[inline]
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_u16(self.0)
    }
}

impl Decode for Constant {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        Self(dec.decode_u16_unchecked())
    }
}

impl Encode for u8 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_u8(self)
    }
}

impl Encode for u16 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_u16(self)
    }
}

impl Encode for u32 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_u32(self)
    }
}

impl Encode for u64 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_u64(self)
    }
}

impl Encode for i8 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_i8(self)
    }
}

impl Encode for i16 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_i16(self)
    }
}

impl Encode for i32 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_i32(self)
    }
}

impl Encode for i64 {
    fn encode<E>(self, enc: &mut E)
    where
        E: ?Sized + Encoder,
    {
        enc.encode_i64(self)
    }
}

impl Decode for u8 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_u8_unchecked()
    }
}

impl Decode for u16 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_u16_unchecked()
    }
}

impl Decode for u32 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_u32_unchecked()
    }
}

impl Decode for u64 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_u64_unchecked()
    }
}

impl Decode for i8 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_i8_unchecked()
    }
}

impl Decode for i16 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_i16_unchecked()
    }
}

impl Decode for i32 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_i32_unchecked()
    }
}

impl Decode for i64 {
    #[inline(always)]
    unsafe fn decode(dec: &mut impl Decoder) -> Self {
        dec.decode_i64_unchecked()
    }
}
