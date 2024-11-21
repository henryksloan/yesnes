pub mod hi_rom;
pub mod lo_rom;

pub use hi_rom::HiROM;
pub use lo_rom::LoROM;

use crate::u24::u24;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MapperType {
    Unknown,
    LoROM,
    HiROM,
    SA1,
}

impl MapperType {
    // Returns whether the map mode is "HiROM" according to the "ROM Speed and
    // Map Mode (FFD5h)" header attribute. e.g. SA1 is considered LoROM,
    // while SPC7110 is HiROM.
    pub fn is_hirom(&self) -> bool {
        match *self {
            Self::HiROM => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CoprocessorType {
    None,
    DSP,
    GSU,
    OBC1,
    SA1,
    SDD1,
    SRTC,
}

pub trait Mapper {
    fn reset(&mut self) {}
    fn try_read_u8(&self, addr: u24) -> Option<u8>;
    fn try_write_u8(&mut self, addr: u24, data: u8) -> bool;
}
