use bitfield::bitfield;

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub display_control_1: DisplayControl1,
    pub main_layer_enable: LayerEnable,
    pub sub_layer_enable: LayerEnable,
    pub display_control_2: DisplayControl2,
    pub obj_size_base: ObjSizeBase,
    pub oam_addr_priority: OamAddrPriority,
    pub curr_oam_addr: u16,
    // When writing to the first 512 bytes of OAM, writes to even addresses
    // are not committed until an odd address is written.
    pub oam_even_latch: u8,
    pub bg_mode: BackgroundMode,
    pub mosaic: Mosaic,
    pub bg_tilemap_addr_size: [TilemapAddrSize; 4],
    pub bg_chr_addr: BackgroundChrAddr,
    pub bg_scroll: [BackgroundScroll; 4],
    pub vram_addr_incr_mode: VramAddrIncrMode,
    // 2116h - VMADDL - VRAM Address (lower 8bit) (W)
    // 2117h - VMADDH - VRAM Address (upper 8bit) (W)
    pub vram_addr: u16,
    // 2121h - CGADD - Palette CGRAM Address (Color Generator Memory) (W)
    pub cgram_addr: u8,
    pub cgram_access_latch: bool,
}

impl IoRegisters {
    pub fn new() -> Self {
        Default::default()
    }

    /// Updates the VRAM address if byte specified (high or low) of
    /// the data read/write ports were accessed AND that byte matches
    /// the one specified in bit 2115.7
    pub fn update_vram_addr(&mut self, hi_byte: bool) {
        if self.vram_addr_incr_mode.incr_byte() == hi_byte {
            self.vram_addr = self
                .vram_addr
                .wrapping_add(self.vram_addr_incr_mode.step_words());
        }
    }

    pub fn cgram_bits(&self) -> (usize, usize) {
        if self.cgram_access_latch {
            (15, 8)
        } else {
            (7, 0)
        }
    }
}

bitfield! {
  /// 2100h - INIDISP - Display Control 1 (W)
  /// Configures forced blanking and master brightness
  #[derive(Clone, Copy, Default)]
  pub struct DisplayControl1(u8);
  impl Debug;
  pub master_brightness, _: 3, 0;
  pub forced_blank, _: 7;
}

bitfield! {
  /// 212Ch - TM - Main Screen Designation (W)
  /// 212Dh - TS - Sub Screen Designation (W)
  /// Configures enablement of screen main or sub screen layers
  #[derive(Clone, Copy, Default)]
  pub struct LayerEnable(u8);
  impl Debug;
  pub bg1, _: 0;
  pub bg2, _: 1;
  pub bg3, _: 2;
  pub bg4, _: 3;
  pub obj, _: 4;
}

impl LayerEnable {
    pub fn layer_enabled(&self, bg_n: usize) -> bool {
        assert!((1..=4).contains(&bg_n));
        match bg_n {
            1 => self.bg1(),
            2 => self.bg2(),
            3 => self.bg3(),
            4 | _ => self.bg4(),
        }
    }
}

bitfield! {
  /// 2133h - SETINI - Display Control 2 (W)
  /// Additional display configuration, including overscan and interlace
  #[derive(Clone, Copy, Default)]
  pub struct DisplayControl2(u8);
  impl Debug;
  pub interlace, _: 0;
  pub obj_interlace, _: 1;
  pub overscan, _: 2;
  pub extbg, _: 6;
  pub external_sync, _: 7;
}

bitfield! {
  /// 2101h - OBSEL - Object Size and Object Base (W)
  #[derive(Clone, Copy, Default)]
  pub struct ObjSizeBase(u8);
  impl Debug;
  pub chr_base, _: 2, 0; // In 8192-word steps
  pub table_2_gap, _: 4, 3; // in 4096-word steps
  pub obj_size, _: 7, 5;
}

impl ObjSizeBase {
    // Returns the width and height of the large and small sprite sizes selected by obj_size
    pub fn obj_size_large_small(&self) -> [(u16, u16); 2] {
        match self.obj_size() {
            0 => [(8, 8), (16, 16)],
            1 => [(8, 8), (32, 32)],
            2 => [(8, 8), (64, 64)],
            3 => [(16, 16), (32, 32)],
            4 => [(16, 16), (64, 64)],
            5 => [(32, 32), (64, 64)],
            // These two are undocumented.
            6 => [(16, 32), (32, 64)],
            7 | _ => [(16, 32), (32, 32)],
        }
    }

    pub fn obj_width_height(&self, large: bool) -> (u16, u16) {
        self.obj_size_large_small()[large as usize]
    }

    pub fn calculate_vram_addr(&self, table_2: bool, chr_n: u8) -> u16 {
        let base = (self.chr_base() as u16) << 13;
        let table_off = if table_2 {
            (self.table_2_gap() as u16 + 1) << 12
        } else {
            0
        };
        (base + table_off + ((chr_n as u16) << 4)) & 0x7FFF
    }
}

bitfield! {
  /// 2102h/2103h - OAMADDL/OAMADDH - OAM Address and Priority Rotation (W)
  #[derive(Clone, Copy, Default)]
  pub struct OamAddrPriority(u16);
  impl Debug;
  pub addr, _: 8, 0;
  pub priority_obj_n, _: 7, 1;
  pub priority_rotation, _: 15;

  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
}

bitfield! {
  /// 2105h - BGMODE - BG Mode and BG Character Size (W)
  #[derive(Clone, Copy, Default)]
  pub struct BackgroundMode(u8);
  impl Debug;
  pub bg_mode, _: 2, 0;
  pub bg3_priority, _: 3;
  pub bg1_tile_size, _: 4;
  pub bg2_tile_size, _: 5;
  pub bg3_tile_size, _: 6;
  pub bg4_tile_size, _: 7;
}

bitfield! {
  /// 2106h - MOSAIC - Mosaic Size and Mosaic Enable (W)
  #[derive(Clone, Copy, Default)]
  pub struct Mosaic(u8);
  impl Debug;
  pub bg1_mosaic, _: 0;
  pub bg2_mosaic, _: 1;
  pub bg3_mosaic, _: 2;
  pub bg4_mosaic, _: 3;
  pub mosaic_size, _: 7, 4;
}

bitfield! {
  /// 2107h - BG1SC - BG1 Screen Base and Screen Size (W)
  /// 2108h - BG2SC - BG2 Screen Base and Screen Size (W)
  /// 2109h - BG3SC - BG3 Screen Base and Screen Size (W)
  /// 210Ah - BG4SC - BG4 Screen Base and Screen Size (W)
  /// Holds the base address (in 1024-word steps) and screen size of a tilemap
  #[derive(Clone, Copy, Default)]
  pub struct TilemapAddrSize(u8);
  impl Debug;
  pub size, _: 1, 0;
  pub mirror_h, _: 0;
  pub mirror_v, _: 1;

  // TODO: SNESTEK says this goes up to bit 7, but it seems to be incorrect?
  pub base, _: 6, 2;
}

bitfield! {
  /// 210Bh/210Ch - BG12NBA/BG34NBA - BG Character Data Area Designation (W)
  /// Holds the addresses (in 4096-word steps) of character data for each background
  #[derive(Clone, Copy, Default)]
  pub struct BackgroundChrAddr(u16);
  impl Debug;
  pub bg1_base, _: 3, 0;
  pub bg2_base, _: 7, 4;
  pub bg3_base, _: 11, 8;
  pub bg4_base, _: 15, 12;

  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
}

impl BackgroundChrAddr {
    pub fn bg_base(&self, bg_n: usize) -> u16 {
        assert!((1..=4).contains(&bg_n));
        match bg_n {
            1 => self.bg1_base(),
            2 => self.bg2_base(),
            3 => self.bg3_base(),
            4 | _ => self.bg4_base(),
        }
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct BackgroundScrollComponent {
    pub val: u16,
    pub access_latch: bool,
}

impl BackgroundScrollComponent {
    pub fn write_next(&mut self, data: u8) {
        self.val = if self.access_latch {
            (self.val & 0x00FF) | ((data as u16) << 8)
        } else {
            (self.val & 0xFF00) | data as u16
        };
        self.access_latch = !self.access_latch;
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct BackgroundScroll {
    pub h: BackgroundScrollComponent,
    pub v: BackgroundScrollComponent,
}

bitfield! {
  /// 2115h - VMAIN - VRAM Address Increment Mode (W)
  #[derive(Clone, Copy, Default)]
  pub struct VramAddrIncrMode(u8);
  impl Debug;
  pub step, _: 1, 0;
  // TODO: Implement VRAM address translation
  pub translation, _: 3, 2;
  pub incr_byte, _: 7;
}

impl VramAddrIncrMode {
    pub fn step_words(&self) -> u16 {
        match self.step() {
            0b00 => 1,
            0b01 => 32,
            0b10 | 0b11 | _ => 128,
        }
    }
}
