mod cartridge_header;
mod mapper;

use cartridge_header::CartridgeHeader;
use mapper::{CoprocessorType, HiROM, LoROM, MapperType};

pub use mapper::Mapper;

use crate::coprocessor::sa1::SA1;
use crate::u24::u24;

use memmap::MmapMut;

use std::fs::OpenOptions;
use std::ops::DerefMut;
use std::path::Path;

// TODO: Implement speeds and such
pub struct Cartridge {
    #[expect(unused)]
    header: CartridgeHeader,
    mapper: Box<dyn Mapper>,
    coprocessor: Option<Box<dyn Mapper>>,
}

impl Cartridge {
    pub fn new(mut data: Vec<u8>, cart_path: &Path) -> Self {
        // Drop extra headers from copiers (SWC/UFO/etc.)
        if data.len() & 0x3FF == 0x200 {
            data.drain(..0x200);
        }
        // TODO: Once the frontend has any error reporting, refactor these panics to Result
        let (header, mapper_type) = {
            let lorom_header = (data.len() >= 0x7FE0)
                .then(|| {
                    CartridgeHeader::try_read_from(data[0x7FC0..0x7FE0].try_into().unwrap())
                        .map(|header| (header, MapperType::LoROM))
                })
                .flatten();
            let hirom_header = (data.len() >= 0xFFE0)
                .then(|| {
                    CartridgeHeader::try_read_from(data[0xFFC0..0xFFE0].try_into().unwrap())
                        .map(|header| (header, MapperType::HiROM))
                })
                .flatten();
            if lorom_header.is_some() && hirom_header.is_some() {
                if hirom_header.as_ref().unwrap().0.checksum_valid() {
                    hirom_header.unwrap()
                } else {
                    lorom_header.unwrap()
                }
            } else if lorom_header.is_some() {
                lorom_header.unwrap()
            } else {
                hirom_header.expect("Invalid ROM")
            }
        };
        if header.mapper() != mapper_type {
            // TODO: Guessing the mapping type purely from checksums isn't quite ideal
            panic!(
                "ROM mapping type {:?} mismatches checksum-ascertained type {:?}",
                header.mapper(),
                mapper_type
            );
        }
        // DO NOT SUBMIT:
        // match header.cartridge_type().0 {
        //     0x00..=0x02 => {}
        //     cartridge_type => panic!("Unsupported cartridge type 0x{cartridge_type:X}"),
        // }
        log::debug!("Cartridge title: {}", header.title());
        // TODO: Support configurable SRAM directory
        // TODO: Consider supporting non-file-backed SRAM (just a VEC)
        let sram: Box<dyn DerefMut<Target = [u8]>> = if header.ram_bytes() > 0 {
            let sram_path = cart_path.with_extension("srm");
            let sram_file = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .truncate(false)
                .open(&sram_path)
                .expect("Failed to create or open SRAM file");
            log::debug!("Opened SRAM file at {sram_path:?}");
            sram_file
                .set_len(header.ram_bytes() as u64)
                .expect("Failed to resize SRAM file");
            Box::new(unsafe { MmapMut::map_mut(&sram_file).expect("Failed to mmap SRAM file") })
        } else {
            Box::new(vec![0; header.ram_bytes()])
        };
        let mapper: Box<dyn Mapper> = match mapper_type {
            MapperType::LoROM => Box::new(LoROM::new(data, sram)),
            MapperType::HiROM => Box::new(HiROM::new(data, sram)),
            _ => unimplemented!("Mapper {mapper_type:?} is not yet implemented"),
        };
        let coprocessor: Option<Box<dyn Mapper>> = match header.cartridge_type().coprocessor() {
            CoprocessorType::None => None,
            CoprocessorType::SA1 => Some(Box::new(SA1 {})),
            coprocessor_type => {
                unimplemented!("Unimplemented coprocessor type {coprocessor_type:?}")
            }
        };
        Self {
            header,
            mapper,
            coprocessor,
        }
    }

    pub fn try_read_u8(&self, addr: u24) -> Option<u8> {
        self.coprocessor
            .as_ref()
            .and_then(|coprocessor| coprocessor.try_read_u8(addr))
            .or_else(|| self.mapper.try_read_u8(addr))
    }

    pub fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
        self.coprocessor
            .as_mut()
            .map_or(false, |coprocessor| coprocessor.try_write_u8(addr, data))
            || self.mapper.try_write_u8(addr, data)
    }
}
