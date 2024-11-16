use super::Mapper;

use crate::u24::u24;

use std::ops::DerefMut;

pub struct HiROM {
    rom: Vec<u8>,
    sram: Box<dyn DerefMut<Target = [u8]>>,
    rom_mask: usize,
    sram_mask: usize,
}

impl HiROM {
    pub fn new(rom: Vec<u8>, sram: Box<dyn DerefMut<Target = [u8]>>) -> Self {
        Self {
            // TODO: This masking isn't robust to non-power-of-two sizes, though those may be impossible?
            // DO NOT SUBMIT: Maybe remove rom_mask
            rom_mask: rom.len().next_power_of_two() - 1,
            sram_mask: sram.len().next_power_of_two() - 1,
            rom,
            sram,
        }
    }
}

impl Mapper for HiROM {
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
                    Some(unsafe {
                        *self.sram.get_unchecked(
                            (((addr.bank() as usize - 0x30) * 0x2000)
                                | (addr.lo16() as usize - 0x6000))
                                & self.sram_mask,
                        )
                    })
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
                    unsafe {
                        *self.sram.get_unchecked_mut(
                            (((addr.bank() as usize - 0x30) * 0x2000)
                                | (addr.lo16() as usize - 0x6000))
                                & self.sram_mask,
                        ) = data;
                    }
                }
                true
            }
            _ => false,
        }
    }
}
