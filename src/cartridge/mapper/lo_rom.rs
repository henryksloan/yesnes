use super::Mapper;

use crate::u24::u24;

use std::ops::DerefMut;

pub struct LoROM {
    rom: Vec<u8>,
    sram: Box<dyn DerefMut<Target = [u8]>>,
    rom_mask: usize,
    sram_mask: usize,
}

impl LoROM {
    pub fn new(rom: Vec<u8>, sram: Box<dyn DerefMut<Target = [u8]>>) -> Self {
        Self {
            // TODO: This masking isn't robust to non-power-of-two sizes, though those may be impossible?
            rom_mask: rom.len().next_power_of_two() - 1,
            sram_mask: sram.len().next_power_of_two() - 1,
            rom,
            sram,
        }
    }
}

impl Mapper for LoROM {
    fn try_read_u8(&self, addr: u24) -> Option<u8> {
        match addr.bank() {
            // A mirror of the cartridge header and exception vectors (as they are placed in LoROM images)
            0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                Some(self.rom[(0x7F00 | (addr.lo16() & 0xFF)) as usize])
            }
            0x00..=0x7D | 0x80..=0xFF if (0x8000..=0xFFFF).contains(&addr.lo16()) => Some(unsafe {
                *self.rom.get_unchecked(
                    (((addr.bank() as usize & !0x80) * 0x8000) | (addr.lo16() as usize - 0x8000))
                        & self.rom_mask,
                )
            }),
            0x70..=0x7D | 0xF0..=0xFF if (0x0000..=0x7FFF).contains(&addr.lo16()) => {
                let sram_size = self.sram.len();
                if sram_size > 0 {
                    Some(unsafe {
                        *self.sram.get_unchecked(
                            ((((addr.bank() as usize & !0x80) - 0x70) * 0x8000)
                                | addr.lo16() as usize)
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
            0x70..=0x7D | 0xF0..=0xFF if (0x0000..=0x7FFF).contains(&addr.lo16()) => {
                let sram_size = self.sram.len();
                if sram_size > 0 {
                    self.sram[((((addr.bank() as usize & !0x80) - 0x70) * 0x8000)
                        | addr.lo16() as usize)
                        % sram_size] = data;
                }
                true
            }
            _ => false,
        }
    }
}
