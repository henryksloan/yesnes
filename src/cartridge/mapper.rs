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
}

pub trait Mapper {
    fn try_read_u8(&self, addr: u24) -> Option<u8>;
    fn try_write_u8(&mut self, addr: u24, data: u8) -> bool;
}
