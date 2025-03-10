use bitfield::bitfield;

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub display_control_1: DisplayControl1,
    pub main_layer_enable: LayerEnable,
    pub sub_layer_enable: LayerEnable,
    #[expect(unused)]
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
    pub window_mask: WindowMaskSettings,
    // 2126h - WH0 - Window 1 Left Position (X1) (W)
    // 2127h - WH1 - Window 1 Right Position (X2) (W)
    // 2128h - WH2 - Window 2 Left Position (X1) (W)
    // 2129h - WH3 - Window 2 Right Position (X2) (W)
    pub window_pos: [WindowPosition; 2],
    pub window_bg_logic: WindowBackgroundLogic,
    pub window_obj_math_logic: WindowObjMathLogic,
    // 212Eh - TMW - Window Area Main Screen Disable (W)
    pub window_main_screen_disable: WindowAreaScreenDisable,
    // 212Fh - TSW - Window Area Sub Screen Disable (W)
    pub window_sub_screen_disable: WindowAreaScreenDisable,
    pub color_math_control_a: ColorMathControlA,
    pub color_math_control_b: ColorMathControlB,
    // 2132h - COLDATA - Color Math Sub Screen Backdrop Color (W)
    pub color_math_backdrop_color: [u8; 3],
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
  pub bg1_enable, _: 0;
  pub bg2_enable, _: 1;
  pub bg3_enable, _: 2;
  pub bg4_enable, _: 3;
  pub obj_enable, _: 4;
}

impl LayerEnable {
    pub fn bg_enabled(&self, bg_n: usize) -> bool {
        match bg_n {
            1 => self.bg1_enable(),
            2 => self.bg2_enable(),
            3 => self.bg3_enable(),
            4 => self.bg4_enable(),
            _ => panic!("Invalid background number {bg_n}"),
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
            7 => [(16, 32), (32, 32)],
            _ => unreachable!(),
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

impl OamAddrPriority {
    pub fn oam_priority_iter(&self) -> impl DoubleEndedIterator<Item = usize> {
        let highest_priority = if self.priority_rotation() {
            self.priority_obj_n() as usize
        } else {
            0
        };
        (highest_priority..128).chain(0..highest_priority)
    }
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
        match bg_n {
            1 => self.bg1_base(),
            2 => self.bg2_base(),
            3 => self.bg3_base(),
            4 => self.bg4_base(),
            _ => panic!("Invalid background number {bg_n}"),
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
            0b10 | 0b11 => 128,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct WindowMaskSettings {
    /// 2123h - W12SEL - Window BG1/BG2 Mask Settings (W)
    pub bg1_bg2_masks: WindowMaskPair,
    /// 2124h - W34SEL - Window BG3/BG4 Mask Settings (W)
    pub bg3_bg4_masks: WindowMaskPair,
    /// 2125h - WOBJSEL - Window OBJ/MATH Mask Settings (W)
    pub obj_math_masks: WindowMaskPair,
}

impl WindowMaskSettings {
    pub fn bg_masks(&self, bg_n: usize) -> [WindowMask; 2] {
        match bg_n {
            1 | 2 => self.bg1_bg2_masks.get_masks(bg_n == 2),
            3 | 4 => self.bg3_bg4_masks.get_masks(bg_n == 4),
            _ => panic!("Invalid background number {bg_n}"),
        }
    }

    pub fn obj_masks(&self) -> [WindowMask; 2] {
        self.obj_math_masks.get_masks(false)
    }

    pub fn math_masks(&self) -> [WindowMask; 2] {
        self.obj_math_masks.get_masks(true)
    }
}

#[derive(Clone, Copy, Default)]
pub struct WindowMaskPair(pub u8);

impl WindowMaskPair {
    pub fn get_mask(&self, hi: bool, window_n: usize) -> WindowMask {
        assert!(window_n == 1 || window_n == 2);
        WindowMask::from((self.0 >> (hi as u8 * 4 + (window_n as u8 - 1) * 2)) & 0b11)
    }

    pub fn get_masks(&self, hi: bool) -> [WindowMask; 2] {
        [self.get_mask(hi, 1), self.get_mask(hi, 2)]
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct WindowMask {
    pub enable: bool,
    pub invert: bool,
}

impl From<u8> for WindowMask {
    fn from(value: u8) -> Self {
        Self {
            // enable: value & 0b01 == 0b01,
            // invert: value & 0b10 == 0b10,
            enable: value & 0b10 == 0b10,
            invert: value & 0b01 == 0b01,
        }
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct WindowPosition {
    pub left: u8,
    pub right: u8,
}

bitfield! {
  // 212Ah - WBGLOG - Background Window 1/2 Mask Logic (W)
  #[derive(Clone, Copy, Default)]
  pub struct WindowBackgroundLogic(u8);
  impl Debug;
  pub u8, into WindowLogic, bg1_logic, _: 1, 0;
  pub u8, into WindowLogic, bg2_logic, _: 3, 2;
  pub u8, into WindowLogic, bg3_logic, _: 5, 4;
  pub u8, into WindowLogic, bg4_logic, _: 7, 6;
}

impl WindowBackgroundLogic {
    pub fn bg_logic(&self, bg_n: usize) -> WindowLogic {
        match bg_n {
            1 => self.bg1_logic(),
            2 => self.bg2_logic(),
            3 => self.bg3_logic(),
            4 => self.bg4_logic(),
            _ => panic!("Invalid background number {bg_n}"),
        }
    }
}

bitfield! {
  // 212Bh - WOBJLOG - Obj/Math Window 1/2 Mask Logic (W)
  #[derive(Clone, Copy, Default)]
  pub struct WindowObjMathLogic(u8);
  impl Debug;
  pub u8, into WindowLogic, obj_logic, _: 1, 0;
  pub u8, into WindowLogic, math_logic, _: 3, 2;
}

#[derive(Clone, Copy, Debug)]
pub enum WindowLogic {
    OR,
    AND,
    XOR,
    XNOR,
}

impl WindowLogic {
    pub fn apply(&self, window1: bool, window2: bool) -> bool {
        match *self {
            WindowLogic::OR => window1 || window2,
            WindowLogic::AND => window1 && window2,
            WindowLogic::XOR => window1 ^ window2,
            WindowLogic::XNOR => !(window1 ^ window2),
        }
    }
}

impl From<u8> for WindowLogic {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::OR,
            1 => Self::AND,
            2 => Self::XOR,
            3 => Self::XNOR,
            _ => panic!("Invalid window logic value {value}"),
        }
    }
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct WindowAreaScreenDisable(u8);
  impl Debug;
  pub bg1_disable, _: 0;
  pub bg2_disable, _: 1;
  pub bg3_disable, _: 2;
  pub bg4_disable, _: 3;
  pub obj_disable, _: 4;
}

impl WindowAreaScreenDisable {
    pub fn bg_disable(&self, bg_n: usize) -> bool {
        match bg_n {
            1 => self.bg1_disable(),
            2 => self.bg2_disable(),
            3 => self.bg3_disable(),
            4 => self.bg4_disable(),
            _ => panic!("Invalid background number {bg_n}"),
        }
    }
}

bitfield! {
  // 2130h - CGWSEL - Color Math Control Register A (W)
  #[derive(Clone, Copy, Default)]
  pub struct ColorMathControlA(u8);
  impl Debug;
  pub direct_color, _: 0;
  pub sub_screen_bg_obj, _: 1;
  pub u8, into ColorMathCondition, color_math_condition, _: 5, 4;
  pub u8, into ColorMathCondition, force_main_screen_black, _: 7, 6;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ColorMathCondition {
    Never,
    OutsideMathWindow,
    InsideMathWindow,
    Always,
}

impl From<u8> for ColorMathCondition {
    fn from(value: u8) -> Self {
        // TODO: "Force Main Screen Black" might interpret this differently?
        match value {
            0 => Self::Always,
            1 => Self::InsideMathWindow,
            2 => Self::OutsideMathWindow,
            3 => Self::Never,
            _ => panic!("Invalid color math condition {value}"),
        }
    }
}

bitfield! {
  // 2131h - CGADSUB - Color Math Control Register B (W)
  #[derive(Clone, Copy, Default)]
  pub struct ColorMathControlB(u8);
  impl Debug;
  pub bg1_color_math, _: 0;
  pub bg2_color_math, _: 1;
  pub bg3_color_math, _: 2;
  pub bg4_color_math, _: 3;
  // Only applies to OBJ palettes 4..=7
  pub obj_hipal_color_math, _: 4;
  pub backdrop_color_math, _: 5;
  pub div2_result, _: 6;
  pub subtract, _: 7;
}

impl ColorMathControlB {
    pub fn bg_color_math(&self, bg_n: usize) -> bool {
        match bg_n {
            1 => self.bg1_color_math(),
            2 => self.bg2_color_math(),
            3 => self.bg3_color_math(),
            4 => self.bg4_color_math(),
            _ => panic!("Invalid background number {bg_n}"),
        }
    }
}
