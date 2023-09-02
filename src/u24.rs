use bitfield::bitfield_bitrange;
use bitfield::BitRangeMut;

use std::fmt;
use std::ops::*;

// A faux primative to store 24-bit addresses
#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
#[allow(non_camel_case_types)]
pub struct u24(pub u32);

bitfield_bitrange! {struct u24(u32)}

impl u24 {
    pub fn raw(&self) -> usize {
        (self.0 & 0xFF_FFFF) as usize
    }

    pub fn lo16(&self) -> u16 {
        (self.0 & 0xFFFF) as u16
    }

    pub fn lo_byte(&self) -> u8 {
        self.0 as u8
    }

    pub fn set_lo_byte(&mut self, data: u8) {
        self.set_bit_range(7, 0, data);
    }

    pub fn hi_byte(&self) -> u8 {
        (self.0 >> 8) as u8
    }

    pub fn set_hi_byte(&mut self, data: u8) {
        self.set_bit_range(15, 8, data);
    }

    pub fn bank(&self) -> u8 {
        (self.0 >> 16) as u8
    }

    pub fn set_bank(&mut self, data: u8) {
        self.set_bit_range(23, 16, data);
    }

    pub fn wrapping_add_signed(&self, other: i32) -> u24 {
        u24(self.0.wrapping_add_signed(other))
    }
}

impl fmt::Display for u24 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#08X}", self.0)
    }
}

impl fmt::UpperHex for u24 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::UpperHex::fmt(&self.0, f)
    }
}

impl fmt::LowerHex for u24 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::LowerHex::fmt(&self.0, f)
    }
}

impl Into<usize> for u24 {
    fn into(self) -> usize {
        self.raw()
    }
}

impl From<usize> for u24 {
    fn from(num: usize) -> Self {
        u24(num as u32)
    }
}

impl From<u32> for u24 {
    fn from(num: u32) -> Self {
        u24(num & 0xFFFFFF)
    }
}

impl From<u16> for u24 {
    fn from(num: u16) -> Self {
        u24(num as u32)
    }
}

impl From<u8> for u24 {
    fn from(num: u8) -> Self {
        u24(num as u32)
    }
}

impl<T: Into<u24>> Add<T> for u24 {
    type Output = Self;

    fn add(self, other: T) -> Self {
        Self((self.0 + other.into().0) & 0xFFFFFF)
    }
}

impl<T: Into<u24>> AddAssign<T> for u24 {
    fn add_assign(&mut self, other: T) {
        *self = *self + other
    }
}

impl<T: Into<u24>> Sub<T> for u24 {
    type Output = Self;

    fn sub(self, other: T) -> Self {
        Self((self.0 - other.into().0) & 0xFFFFFF)
    }
}

impl<T: Into<u24>> SubAssign<T> for u24 {
    fn sub_assign(&mut self, other: T) {
        *self = *self - other
    }
}

impl<T: Into<u24>> BitOr<T> for u24 {
    type Output = Self;

    fn bitor(self, other: T) -> Self {
        Self(self.0 | other.into().0)
    }
}

impl<T: Into<u24>> BitOrAssign<T> for u24 {
    fn bitor_assign(&mut self, other: T) {
        *self = *self | other
    }
}

impl<T: Into<u24>> BitAnd<T> for u24 {
    type Output = Self;

    fn bitand(self, other: T) -> Self {
        Self(self.0 & other.into().0)
    }
}

impl<T: Into<u24>> BitAndAssign<T> for u24 {
    fn bitand_assign(&mut self, other: T) {
        *self = *self & other
    }
}

impl<T: Into<i32>> Shl<T> for u24 {
    type Output = Self;

    fn shl(self, other: T) -> Self {
        Self(self.0 << other.into())
    }
}

impl<T: Into<i32>> ShlAssign<T> for u24 {
    fn shl_assign(&mut self, other: T) {
        *self = *self << other
    }
}

impl<T: Into<i32>> Shr<T> for u24 {
    type Output = Self;

    fn shr(self, other: T) -> Self {
        Self(self.0 << other.into())
    }
}

impl<T: Into<i32>> ShrAssign<T> for u24 {
    fn shr_assign(&mut self, other: T) {
        *self = *self << other
    }
}
