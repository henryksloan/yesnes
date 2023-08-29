use bitfield::bitfield;

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub display_control_1: DisplayControl1,
    pub layer_enable: LayerEnable,
    pub display_control_2: DisplayControl2,
    pub bg_mode: BackgroundMode,
    pub mosiac: Mosaic,
    pub bg_tilemap_addr_size: [TilemapAddrSize; 4],
    pub bg_chr_addr: BackgroundChrAddr,
    pub bg_scroll: [BackgroundScroll; 4],
}

impl IoRegisters {
    pub fn new() -> Self {
        Default::default()
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

  pub base, _: 7, 2;
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
}

#[derive(Clone, Copy, Default, Debug)]
pub struct BackgroundScroll {
    h: u16,
    v: u16,
}
