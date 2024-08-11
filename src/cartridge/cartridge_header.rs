use super::mapper::MapperType;

use bitfield::bitfield;

pub struct CartridgeHeader {
    // FFC0h - Cartridge title (21 bytes, uppercase ascii, padded with spaces)
    title: [u8; 0x15],
    rom_speed_map_mode: RomSpeedMapMode,
    cartridge_type: CartridgeType,
    // FFD7h - ROM size (1 SHL n) Kbytes (usually 8=256KByte .. 0Ch=4MByte)
    // Values are rounded-up for carts with 10,12,20,24 Mbits
    #[expect(unused)]
    rom_size_shift: u8,
    // FFD8h - RAM size (1 SHL n) Kbytes (usually 1=2Kbyte .. 5=32Kbyte) (0=None)
    ram_size_shift: u8,
    // FFD9h - Country (also implies PAL/NTSC)
    #[expect(unused)]
    country: u8,
    // FFDAh - Developer ID code  (00h=None/Homebrew, 01h=Nintendo, etc.) (33h=New)
    #[expect(unused)]
    developer_id: u8,
    // FFDBh - ROM Version number (00h=First)
    #[expect(unused)]
    rom_version: u8,
    // FFDCh - Checksum complement (same as below, XORed with FFFFh)
    checksum_complement: u16,
    // FFDEh - Checksum (all bytes in ROM added together; assume [FFDC-F]=FF,FF,0,0)
    checksum: u16,
}

impl CartridgeHeader {
    pub fn try_read_from(data: &[u8; 0x20]) -> Option<Self> {
        let checksum_complement = u16::from_le_bytes(data[0x1C..=0x1D].try_into().unwrap());
        let checksum = u16::from_le_bytes(data[0x1E..=0x1F].try_into().unwrap());

        Some(Self {
            title: data[..0x15].try_into().unwrap(),
            rom_speed_map_mode: RomSpeedMapMode(data[0x15]),
            cartridge_type: CartridgeType(data[0x16]),
            rom_size_shift: data[0x17],
            ram_size_shift: data[0x18],
            country: data[0x19],
            developer_id: data[0x1A],
            rom_version: data[0x1B],
            checksum_complement,
            checksum,
        })
    }

    pub fn title(&self) -> String {
        std::str::from_utf8(&self.title)
            .unwrap_or("ERROR")
            .to_owned()
    }

    pub fn ram_bytes(&self) -> usize {
        match self.ram_size_shift {
            0 => 0,
            shift => (1 << shift) * 0x400,
        }
    }

    pub fn cartridge_type(&self) -> CartridgeType {
        self.cartridge_type
    }

    pub fn mapper(&self) -> MapperType {
        self.rom_speed_map_mode.mapper()
    }

    pub fn checksum_valid(&self) -> bool {
        return !self.checksum == self.checksum_complement;
    }
}

bitfield! {
  // FFD5h - Rom Makeup / ROM Speed and Map Mode
  #[derive(Clone, Copy, Default)]
  pub struct RomSpeedMapMode(u8);
  impl Debug;
  pub map_mode, _: 3, 0;
  pub fast, _: 4;
}

impl RomSpeedMapMode {
    pub fn mapper(&self) -> MapperType {
        match self.map_mode() {
            0x0 | 0x2 | 0x3 => MapperType::LoROM,
            0x1 | 0x5 | 0xA => MapperType::HiROM,
            _ => MapperType::Unknown,
        }
    }
}

bitfield! {
  // FFD6h - Cartridge type (ROM/RAM/Co-processors)
  #[derive(Clone, Copy, Default)]
  pub struct CartridgeType(u8);
  impl Debug;
  pub lo4, _: 3, 0;
  pub coprocessor_type, _: 7, 4;
}
