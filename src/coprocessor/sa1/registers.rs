use bitfield::bitfield;

use crate::u24::u24;

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub mmc_bank_controls: [MmcBankControl; 4],
    pub snes_bw_ram_bank_control: SnesBwRamBankControl,
    pub sa1_bw_ram_bank_control: Sa1BwRamBankControl,
}

impl IoRegisters {
    pub fn new() -> Self {
        Default::default()
    }
}

bitfield! {
  /// 2200h SNES CCNT - SA-1 CPU Control (W)
  // Operations and messages from SNES to SA-1.
  #[derive(Clone, Copy, Default)]
  pub struct Sa1CpuControl(u8);
  impl Debug;
  pub u32, message, _: 3, 0;
  pub nmi, _: 4;
  pub reset, _: 5;
  pub wait, _: 6;
  pub irq, _: 7;
}

bitfield! {
  /// 2220h SNES CXB - Set Super MMC Bank C - Hirom C0h-CFh / LoRom 00h-1Fh (W)
  /// 2221h SNES DXB - Set Super MMC Bank D - Hirom D0h-DFh / LoRom 20h-3Fh (W)
  /// 2222h SNES EXB - Set Super MMC Bank E - Hirom E0h-EFh / LoRom 80h-9Fh (W)
  /// 2223h SNES FXB - Set Super MMC Bank F - Hirom F0h-FFh / LoRom A0h-BFh (W)
  /// Controls the mapping of each the four mappable HiROM and LoROM regions.
  #[derive(Clone, Copy, Default)]
  pub struct MmcBankControl(u8);
  impl Debug;
  pub u8, bank, _: 2, 0; // Selects a 1MByte bank
  pub lorom, _: 7;
}

bitfield! {
  /// 2224h SNES BMAPS - SNES CPU BW-RAM Mapping to 6000h-7FFFh (W)
  /// Controls the mapping of the mappable BW-RAM region on the SNES side.
  #[derive(Clone, Copy, Default)]
  pub struct SnesBwRamBankControl(u8);
  impl Debug;
  pub u32, block, _: 4, 0;
}

impl SnesBwRamBankControl {
    pub fn base(self) -> u24 {
        u24(self.block() * 0x2000)
    }
}

bitfield! {
  /// 2225h SA-1 BMAP - SA-1 CPU BW-RAM Mapping to 6000h-7FFFh (W)
  /// Controls the mapping of the mappable BW-RAM region on the SA-1 side.
  #[derive(Clone, Copy, Default)]
  pub struct Sa1BwRamBankControl(u8);
  impl Debug;
  // Depending on the setting of `source`
  // 0: Select one of the 32 8KiB regions in banks 40h-43h
  // 1: Select one of the 128 8KiB regions in banks 60h-6Fh
  pub u32, block_32, _: 4, 0;
  pub u32, block_128, _: 6, 0;
  pub source, _: 7;
}

impl Sa1BwRamBankControl {
    pub fn base(self) -> u24 {
        if self.source() {
            u24(0x60_0000) + u24(self.block_128() * 0x2000)
        } else {
            u24(0x40_0000) + u24(self.block_32() * 0x2000)
        }
    }
}
