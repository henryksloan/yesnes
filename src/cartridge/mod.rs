mod cartridge_header;

use cartridge_header::CartridgeHeader;

use crate::u24::u24;

pub struct Cartridge {
    header: CartridgeHeader,
    rom: Vec<u8>,
    sram: Vec<u8>,
}

impl Cartridge {
    pub fn new(data: Vec<u8>) -> Self {
        let header = CartridgeHeader::try_read_from(
            // DO NOT SUBMIT: This depends on HiROM or LoROM
            data[0x7FC0..0x7FE0].try_into().unwrap(),
        )
        // TODO: Once the frontend has any error reporting, refactor these panics to Result
        .expect("Invalid ROM");
        match header.cartridge_type().0 {
            0x00 | 0x01 | 0x02 => {}
            cartridge_type => panic!("Unsupported cartridge type 0x{cartridge_type:X}"),
        }
        log::debug!("{}", header.title());
        let sram = vec![0; header.ram_bytes()];
        Self {
            header,
            rom: data,
            sram,
        }
    }

    pub fn try_read_u8(&self, addr: u24) -> Option<u8> {
        // DO NOT SUBMIT: Delegate based on LoROM/HiROM
        match addr.bank() {
            // DO NOT SUBMIT: I believe this is a mirror of the header; verify and add comment
            0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                Some(self.rom[(0x7F00 | (addr.lo16() & 0xFF)) as usize])
            }
            0x00..=0x7D | 0x80..=0xFF if (0x8000..=0xFFFF).contains(&addr.lo16()) => Some(
                self.rom[(((addr.bank() as usize & !0x80) * 0x8000)
                    | (addr.lo16() as usize - 0x8000))
                    % self.rom.len()],
            ),
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

    pub fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
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
