use bitfield::bitfield;

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub mmc_bank_controls: [MmcBankControl; 4],
}

impl IoRegisters {
    pub fn new() -> Self {
        Default::default()
    }
}

bitfield! {
  /// 2220h SNES CXB - Set Super MMC Bank C - Hirom C0h-CFh / LoRom 00h-1Fh (W)
  /// 2221h SNES DXB - Set Super MMC Bank D - Hirom D0h-DFh / LoRom 20h-3Fh (W)
  /// 2222h SNES EXB - Set Super MMC Bank E - Hirom E0h-EFh / LoRom 80h-9Fh (W)
  /// 2223h SNES FXB - Set Super MMC Bank F - Hirom F0h-FFh / LoRom A0h-BFh (W)
  /// Controls the mapping of each the four mappable HiROM and LoROM regions.
  #[derive(Clone, Copy, Default)]
  pub struct MmcBankControl(u16);
  impl Debug;
  pub u8, bank, _: 2, 0; // Selects a 1MByte bank
  pub u8, lorom, _: 7;
}
