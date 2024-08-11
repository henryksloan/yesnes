use super::Mapper;

use crate::u24::u24;

use std::ops::DerefMut;

pub struct HiROM<SRAM: DerefMut<Target = [u8]>> {
    rom: Vec<u8>,
    sram: SRAM,
}

impl<SRAM: DerefMut<Target = [u8]>> HiROM<SRAM> {
    pub fn new(rom: Vec<u8>, sram: SRAM) -> Self {
        Self { rom, sram }
    }
}

impl<SRAM: DerefMut<Target = [u8]>> Mapper for HiROM<SRAM> {
    fn try_read_u8(&self, addr: u24) -> Option<u8> {
        match addr.bank() {
            0x00..=0x3F | 0x80..=0xBF if (0x8000..=0xFFFF).contains(&addr.lo16()) => Some(
                self.rom[(((addr.bank() as usize & !0x80) * 0x10000) | addr.lo16() as usize)
                    % self.rom.len()],
            ),
            0xC0..=0xFF => Some(
                self.rom[(((addr.bank() as usize & !0xC0) * 0x10000) | addr.lo16() as usize)
                    % self.rom.len()],
            ),
            0x30..=0x3F if (0x6000..=0x7FFF).contains(&addr.lo16()) => {
                let sram_size = self.sram.len();
                if sram_size > 0 {
                    Some(
                        self.sram[(((addr.bank() as usize - 0x30) * 0x2000)
                            | (addr.lo16() as usize - 0x6000))
                            % sram_size],
                    )
                } else {
                    Some(0)
                }
            }
            _ => None,
        }
    }

    fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
        match addr.bank() {
            0x30..=0x3F if (0x6000..=0x7FFF).contains(&addr.lo16()) => {
                let sram_size = self.sram.len();
                if sram_size > 0 {
                    self.sram[(((addr.bank() as usize - 0x30) * 0x2000)
                        | (addr.lo16() as usize - 0x6000))
                        % sram_size] = data;
                }
                true
            }
            _ => false,
        }
    }
}
