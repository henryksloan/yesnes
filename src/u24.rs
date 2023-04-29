use std::ops::{Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign};

// A faux primative to store 24-bit addresses
#[derive(Copy, Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct u24(pub u32);

impl u24 {
    pub fn lo16(&self) -> u16 {
        (self.0 & 0xFFFF) as u16
    }

    pub fn hi8(&self) -> u8 {
        (self.0 >> 16) as u8
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
