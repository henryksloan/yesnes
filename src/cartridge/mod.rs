mod cartridge_header;
mod mapper;

use cartridge_header::CartridgeHeader;
use mapper::Mapper;

use crate::u24::u24;

pub struct Cartridge {
    header: CartridgeHeader,
    rom: Vec<u8>,
    sram: Vec<u8>,
}

impl Cartridge {
    pub fn new(mut data: Vec<u8>) -> Self {
        // Drop extra headers from copiers (SWC/UFO/etc.)
        if data.len() & 0x3FF == 0x200 {
            data.drain(..0x200);
        }
        // TODO: Once the frontend has any error reporting, refactor these panics to Result
        let (header, mapper) = {
            let lorom_header =
                CartridgeHeader::try_read_from(data[0x7FC0..0x7FE0].try_into().unwrap())
                    .map(|header| (header, Mapper::LoROM));
            let hirom_header =
                CartridgeHeader::try_read_from(data[0xFFC0..0xFFE0].try_into().unwrap())
                    .map(|header| (header, Mapper::HiROM));
            lorom_header.or(hirom_header).expect("Invalid ROM")
        };
        if header.mapper() != mapper {
            // TODO: Guessing the mappign type purely from checksums isn't quite ideal
            panic!(
                "ROM mapping type {:?} mismatches checksum-ascertained type {:?}",
                header.mapper(),
                mapper
            );
        }
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
