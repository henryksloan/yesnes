use super::Mapper;

use crate::u24::u24;

pub struct LoROM {
    rom: Vec<u8>,
    sram: Vec<u8>,
}

impl LoROM {
    pub fn new(rom: Vec<u8>, sram: Vec<u8>) -> Self {
        Self { rom, sram }
    }
}

impl Mapper for LoROM {
    fn try_read_u8(&self, addr: u24) -> Option<u8> {
        match addr.bank() {
            // A mirror of the cartridge header and exception vectors (as they are placed in LoROM images)
            0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                Some(self.rom[(0x7F00 | (addr.lo16() & 0xFF)) as usize])
            }
            0x00..=0x7D | 0x80..=0xFF if (0x8000..=0xFFFF).contains(&addr.lo16()) => Some(
                self.rom[(((addr.bank() as usize & !0x80) * 0x8000)
                    | (addr.lo16() as usize - 0x8000))
                    % self.rom.len()],
            ),
            // DO NOT SUBMIT: Where does this 0xF0..=0xFF come from? Is it correct?
            0x70..=0x7D | 0xF0..=0xFF => {
                let sram_size = self.sram.len();
                if sram_size > 0 {
                    Some(
                        self.sram[((((addr.bank() as usize & !0x80) - 0x70) * 0x8000)
                            | addr.lo16() as usize)
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
