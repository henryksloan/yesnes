//! Implements the emath `Numeric` trait for various processor types.
use crate::cpu::StatusRegister;
use crate::u24::u24;

impl eframe::emath::Numeric for u24 {
    const INTEGRAL: bool = true;
    const MIN: Self = u24(0);
    const MAX: Self = u24(0xFF_FFFF);

    fn to_f64(self) -> f64 {
        self.0 as f64
    }

    fn from_f64(num: f64) -> Self {
        Self(num as u32)
    }
}

impl eframe::emath::Numeric for StatusRegister {
    const INTEGRAL: bool = true;
    const MIN: Self = StatusRegister::new(0x00);
    const MAX: Self = StatusRegister::new(0xFF);

    fn to_f64(self) -> f64 {
        (self.get() as u32 | ((self.e as u32) << 8)) as f64
    }

    fn from_f64(num: f64) -> Self {
        let mut new_register = Self::new(num as u32 as u8);
        new_register.e = ((num as u32) >> 8) & 1 == 1;
        new_register
    }
}
