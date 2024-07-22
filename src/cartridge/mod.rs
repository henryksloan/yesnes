mod cartridge_header;
mod mapper;

use cartridge_header::CartridgeHeader;
use mapper::{LoROM, Mapper, MapperType};

use crate::u24::u24;

pub struct Cartridge {
    header: CartridgeHeader,
    mapper: Box<dyn Mapper>,
}

impl Cartridge {
    pub fn new(mut data: Vec<u8>) -> Self {
        // Drop extra headers from copiers (SWC/UFO/etc.)
        if data.len() & 0x3FF == 0x200 {
            data.drain(..0x200);
        }
        // TODO: Once the frontend has any error reporting, refactor these panics to Result
        let (header, mapper_type) = {
            let lorom_header =
                CartridgeHeader::try_read_from(data[0x7FC0..0x7FE0].try_into().unwrap())
                    .map(|header| (header, MapperType::LoROM));
            let hirom_header =
                CartridgeHeader::try_read_from(data[0xFFC0..0xFFE0].try_into().unwrap())
                    .map(|header| (header, MapperType::HiROM));
            lorom_header.or(hirom_header).expect("Invalid ROM")
        };
        if header.mapper() != mapper_type {
            // TODO: Guessing the mapping type purely from checksums isn't quite ideal
            panic!(
                "ROM mapping type {:?} mismatches checksum-ascertained type {:?}",
                header.mapper(),
                mapper_type
            );
        }
        match header.cartridge_type().0 {
            0x00 | 0x01 | 0x02 => {}
            cartridge_type => panic!("Unsupported cartridge type 0x{cartridge_type:X}"),
        }
        log::debug!("{}", header.title());
        let sram = vec![0; header.ram_bytes()];
        let mapper = Box::new(match mapper_type {
            MapperType::LoROM => LoROM::new(data, sram),
            _ => unimplemented!("Mapper {mapper_type:?} is not yet implemented"),
        });
        Self { header, mapper }
    }

    pub fn try_read_u8(&self, addr: u24) -> Option<u8> {
        self.mapper.try_read_u8(addr)
    }

    pub fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
        self.mapper.try_write_u8(addr, data)
    }
}
