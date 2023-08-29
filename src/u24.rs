use bitfield::bitfield_bitrange;

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

    pub fn hi8(&self) -> u8 {
        (self.0 >> 16) as u8
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

impl<T: Into<u32>> From<T> for u24 {
    fn from(num: T) -> Self {
        u24(num.into() & 0xFFFFFF)
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
