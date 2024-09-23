pub mod counter;
mod obj;
mod registers;

use crate::scheduler::*;

pub use counter::PpuCounter;
use obj::{OamLoEntry, ObjAttributes};
use registers::{ColorMathCondition, IoRegisters, WindowLogic, WindowMask};

use arrayvec::ArrayVec;
use bitfield::{BitRange, BitRangeMut};

use std::cell::RefCell;
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;

/// Holds the pixels of each layer on this scanline, that they may be combined according to priority
struct ScanlineBuffers {
    // For each bg layer, buffers of high- and low-priority pixels (0=Lower, 1=Higher)
    bg_buff: Box<[[[Option<[u8; 3]>; 256]; 2]; 4]>,
    // A buffer of pixels at each object priority (0-3)
    obj_buff: Box<[[Option<[u8; 3]>; 256]; 4]>,
    // Color math distinguishes between objects with palettes 0..=3 and those with palettes 4..=7.
    // For x-values with a sprite pixel, this stores whether the corresponding object uses a high palette (4..=7).
    obj_hipal_buff: Box<[bool; 256]>,
}

impl Default for ScanlineBuffers {
    fn default() -> Self {
        Self {
            bg_buff: vec![[[None; 256]; 2]; 4].try_into().unwrap(),
            obj_buff: vec![[None; 256]; 4].try_into().unwrap(),
            obj_hipal_buff: vec![false; 256].try_into().unwrap(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Layer {
    Obj,
    Bg(usize), // .0: BG number
    Backdrop,
}

impl Default for Layer {
    fn default() -> Self {
        Self::Backdrop
    }
}

#[derive(Clone, Copy, Debug)]
enum SubLayer {
    Obj(usize),       // Objects with the given priority
    Bg(usize, usize), // .0: BG number, .1: Priority
}

impl SubLayer {
    pub fn to_layer(self) -> Layer {
        match self {
            SubLayer::Obj(_) => Layer::Obj,
            SubLayer::Bg(bg_n, _) => Layer::Bg(bg_n),
        }
    }
}

#[rustfmt::skip]
fn layer_priority_order(bg_mode: u8, mode1_bg3_priority: bool) -> &'static [SubLayer] {
    use SubLayer::*;
    match bg_mode {
        0 => &[Obj(3), Bg(1, 1), Bg(2, 1), Obj(2), Bg(1, 0), Bg(2, 0), Obj(1), Bg(3, 1), Bg(4, 1), Obj(0), Bg(3, 0), Bg(4, 0)],
        1 => if mode1_bg3_priority {
            &[Bg(3, 1), Obj(3), Bg(1, 1), Bg(2, 1), Obj(2), Bg(1, 0), Bg(2, 0), Obj(1), Obj(0), Bg(3, 0)]
        } else {
            &[Obj(3), Bg(1, 1), Bg(2, 1), Obj(2), Bg(1, 0), Bg(2, 0), Obj(1), Bg(3, 1), Obj(0), Bg(3, 0)]
        }
        2..=5 => &[Obj(3), Bg(1, 1), Obj(2), Bg(2, 1), Obj(1), Bg(1, 0), Obj(0), Bg(2, 0)],
        6 => &[Obj(3), Bg(1, 1), Obj(2), Obj(1), Bg(1, 0), Obj(0)],
        7 => &[Obj(3), Obj(2), Bg(2, 1), Obj(1), Bg(1, 0), Obj(0), Bg(2, 0)],
        _ => panic!("Invalid background mode {bg_mode}"),
    }
}

pub struct PPU {
    vram: Box<[u16; 0x8000]>,
    cgram: Box<[u16; 0x100]>,
    // OAM can be seen as a 512-byte low table and a 32-byte upper table.
    // The two tables have different data formats and different write behavior.
    oam_lo: Box<[u8; 0x200]>,
    oam_hi: Box<[u8; 0x20]>,
    io_reg: IoRegisters,
    ppu_counter: Rc<RefCell<PpuCounter>>,
    frame: Box<[[[u8; 3]; 256]; 224]>,
    main_screen: Box<[[([u8; 3], Layer); 256]; 224]>,
    sub_screen: Box<[[([u8; 3], Layer); 256]; 224]>,
    scanline_buffs: ScanlineBuffers,
    ticks_run: u64,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            io_reg: IoRegisters::new(),
            vram: vec![0; 0x8000].try_into().unwrap(),
            cgram: vec![0; 0x100].try_into().unwrap(),
            oam_lo: vec![0; 0x200].try_into().unwrap(),
            oam_hi: vec![0; 0x20].try_into().unwrap(),
            ppu_counter: Rc::new(RefCell::new(PpuCounter::new())),
            frame: vec![[[0; 3]; 256]; 224].try_into().unwrap(),
            main_screen: vec![[Default::default(); 256]; 224].try_into().unwrap(),
            sub_screen: vec![[Default::default(); 256]; 224].try_into().unwrap(),
            scanline_buffs: ScanlineBuffers::default(),
            ticks_run: 0,
        }
    }

    pub fn reset(&mut self) {
        self.vram.fill(0);
        self.cgram.fill(0);
        self.oam_lo.fill(0);
        self.oam_hi.fill(0);
        self.io_reg = IoRegisters::new();
        self.ppu_counter = Rc::new(RefCell::new(PpuCounter::new()));
        self.ticks_run = 0;
        // TODO: Most of these are indeterminate, but it might be good to initialize to 0 for determinism
    }

    pub fn run<'a>(ppu: Rc<RefCell<PPU>>) -> impl DeviceCoroutine + 'a {
        #[coroutine]
        move || loop {
            // TODO: This is scanline-granularity, and does no synchronization below that granularity.
            let mut step_gen = PPU::step(ppu.clone(), 1364);
            while let CoroutineState::Yielded(yield_reason) = Pin::new(&mut step_gen).resume(()) {
                let ticks_to_yield = std::mem::take(&mut ppu.borrow_mut().ticks_run);
                yield (yield_reason, ticks_to_yield)
            }
        }
    }

    fn step<'a>(ppu: Rc<RefCell<PPU>>, n_clocks: u16) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            ppu.borrow_mut().ticks_run += n_clocks as u64;
            let new_scanline = ppu.borrow().ppu_counter.borrow_mut().tick(n_clocks);
            if new_scanline {
                let scanline = ppu.borrow().ppu_counter.borrow().scanline;
                // TODO: Overscan
                if scanline < 225 {
                    ppu.borrow_mut().debug_render_scanline(scanline);
                }
                if scanline == 225 && !ppu.borrow().io_reg.display_control_1.forced_blank() {
                    ppu.borrow_mut().reload_oam_addr();
                }
            }
            // TODO: Once this has accurate timing, consider loosening the syncing events
            // (though the PPU is so dependent on the CPU that we'll probably want this anyway)
            yield YieldReason::Sync(Device::CPU);
        }
    }

    fn palette_entry_to_rgb(&self, palette_entry: u16) -> [u8; 3] {
        [
            ((palette_entry & 0x1F) as u8) << 3,
            (((palette_entry >> 5) & 0x1F) as u8) << 3,
            (((palette_entry >> 10) & 0x1F) as u8) << 3,
        ]
    }

    // TODO: All of these "debug" functions are unrealistic PPU emulation
    fn debug_render_scanline(&mut self, scanline: u16) {
        if scanline == 0 {
            return;
        }
        let draw_line = scanline - 1;
        self.debug_clear_scanline_buffs();
        let backdrop_color = self.palette_entry_to_rgb(self.cgram[0]);
        self.main_screen[draw_line as usize].fill((backdrop_color, Layer::Backdrop));
        self.sub_screen[draw_line as usize]
            .fill((self.io_reg.color_math_backdrop_color, Layer::Backdrop));
        let layer_bpps: &[usize] = match self.io_reg.bg_mode.bg_mode() {
            0 => &[2, 2, 2, 2],
            1 => &[4, 4, 2],
            2 => &[4, 4], // TODO: Offset-per-pixel
            3 => &[8, 4],
            4 => &[8, 2], // TODO: Offset-per-pixel
            5 => &[4, 2], // TODO: Hi-res
            6 => &[4],    // TODO: Hi-res and Offset-per-pixel
            7 => &[8],    // TODO: Rotation/scaling
            _ => unreachable!(),
        };
        if self.io_reg.bg_mode.bg_mode() == 7 {
            self.debug_render_scanline_mode7(draw_line);
        } else {
            for (bg_i, layer_bpp) in layer_bpps.iter().enumerate() {
                self.debug_render_scanline_bpp(bg_i, draw_line, *layer_bpp);
            }
        }
        self.debug_render_sprites(draw_line);

        if self.io_reg.color_math_control_a.sub_screen_bg_obj() {
            for sublayer in layer_priority_order(
                self.io_reg.bg_mode.bg_mode(),
                self.io_reg.bg_mode.bg3_priority(),
            )
            .iter()
            .rev()
            {
                let (line, sub_screen_enable) = match *sublayer {
                    SubLayer::Obj(priority) => (
                        &self.scanline_buffs.obj_buff[priority],
                        self.io_reg.sub_layer_enable.obj_enable(),
                    ),
                    SubLayer::Bg(bg_n, priority) => (
                        &self.scanline_buffs.bg_buff[bg_n - 1][priority],
                        self.io_reg.sub_layer_enable.bg_enabled(bg_n),
                    ),
                };
                if !sub_screen_enable {
                    continue;
                }
                for x in 0..256 {
                    if let Some(color) = line[x] {
                        self.sub_screen[draw_line as usize][x] = (color, sublayer.to_layer());
                    }
                }
            }
        }

        for sublayer in layer_priority_order(
            self.io_reg.bg_mode.bg_mode(),
            self.io_reg.bg_mode.bg3_priority(),
        )
        .iter()
        .rev()
        {
            let (line, main_screen_enable, window_masks, window_logic, window_disable) =
                match *sublayer {
                    SubLayer::Obj(priority) => (
                        &self.scanline_buffs.obj_buff[priority],
                        self.io_reg.main_layer_enable.obj_enable(),
                        self.io_reg.window_mask.obj_masks(),
                        self.io_reg.window_obj_math_logic.obj_logic(),
                        self.io_reg.window_main_screen_disable.obj_disable(),
                    ),
                    SubLayer::Bg(bg_n, priority) => (
                        &self.scanline_buffs.bg_buff[bg_n - 1][priority],
                        self.io_reg.main_layer_enable.bg_enabled(bg_n),
                        self.io_reg.window_mask.bg_masks(bg_n),
                        self.io_reg.window_bg_logic.bg_logic(bg_n),
                        self.io_reg.window_main_screen_disable.bg_disable(bg_n),
                    ),
                };
            if !main_screen_enable {
                continue;
            }
            for x in 0..256 {
                if window_disable && self.window_area_applies(x, &window_masks, &window_logic) {
                    continue;
                }
                if let Some(color) = line[x] {
                    self.main_screen[draw_line as usize][x] = (color, sublayer.to_layer());
                }
            }
        }

        let color_math_condition = self.io_reg.color_math_control_a.color_math_condition();
        let brightness_coefficient: u16 = match self.io_reg.display_control_1.master_brightness() {
            0 => 0,
            n => n as u16 + 1,
        };
        let force_blank =
            self.io_reg.display_control_1.forced_blank() || brightness_coefficient == 0;
        for x in 0..256 {
            // We could skip rendering altogether in these cases, but we want to populate the buffers for debugging.
            if force_blank {
                self.frame[draw_line as usize][x] = [0, 0, 0];
                continue;
            }

            // TODO: implement window "Force Main Screen Black" (does it affect div?)
            let (main_color, main_layer) = self.main_screen[draw_line as usize][x];
            self.frame[draw_line as usize][x] = main_color;

            let do_color_math = match main_layer {
                Layer::Obj => {
                    self.io_reg.color_math_control_b.obj_hipal_color_math()
                        && self.scanline_buffs.obj_hipal_buff[x]
                }
                Layer::Bg(bg_n) => self.io_reg.color_math_control_b.bg_color_math(bg_n),
                Layer::Backdrop => self.io_reg.color_math_control_b.backdrop_color_math(),
            };
            if color_math_condition != ColorMathCondition::Never && do_color_math {
                let (sub_color, sub_layer) = self.sub_screen[draw_line as usize][x];
                let subtract = self.io_reg.color_math_control_b.subtract();
                let div2_result = self.io_reg.color_math_control_b.div2_result();
                let apply_color_math = color_math_condition == ColorMathCondition::Always || {
                    let invert = color_math_condition == ColorMathCondition::OutsideMathWindow;
                    let window_masks = self.io_reg.window_mask.math_masks();
                    let window_logic = self.io_reg.window_obj_math_logic.math_logic();
                    self.window_area_applies(x, &window_masks, &window_logic) != invert
                };
                if apply_color_math {
                    for i in 0..3 {
                        if subtract {
                            self.frame[draw_line as usize][x][i] =
                                self.frame[draw_line as usize][x][i].saturating_sub(sub_color[i]);
                        } else {
                            self.frame[draw_line as usize][x][i] =
                                self.frame[draw_line as usize][x][i].saturating_add(sub_color[i]);
                        }
                        if div2_result && sub_layer != Layer::Backdrop {
                            self.frame[draw_line as usize][x][i] /= 2;
                        }
                    }
                }
            }
            if brightness_coefficient != 16 {
                for i in 0..3 {
                    self.frame[draw_line as usize][x][i] =
                        ((self.frame[draw_line as usize][x][i] as u16 * brightness_coefficient)
                            / 16) as u8;
                }
            }
        }
    }

    fn window_area_applies(
        &self,
        x: usize,
        window_masks: &[WindowMask; 2],
        window_logic: &WindowLogic,
    ) -> bool {
        if !window_masks[0].enable && !window_masks[1].enable {
            return false;
        }
        let overlap_window1 = window_masks[0].enable && {
            let inside = x >= self.io_reg.window_pos[0].left as usize
                && x <= self.io_reg.window_pos[0].right as usize;
            inside != window_masks[0].invert
        };
        let overlap_window2 = window_masks[1].enable && {
            let inside = x >= self.io_reg.window_pos[1].left as usize
                && x <= self.io_reg.window_pos[1].right as usize;
            inside != window_masks[1].invert
        };
        if window_masks[0].enable && window_masks[1].enable {
            window_logic.apply(overlap_window1, overlap_window2)
        } else {
            // Exactly one of the windows is enabled, so just OR the overlaps
            overlap_window1 || overlap_window2
        }
    }

    fn debug_clear_scanline_buffs(&mut self) {
        for line_buffs in &mut *self.scanline_buffs.bg_buff {
            line_buffs[0].fill(None);
            line_buffs[1].fill(None);
        }
        for obj_buff in &mut *self.scanline_buffs.obj_buff {
            obj_buff.fill(None);
        }
        self.scanline_buffs.obj_hipal_buff.fill(false);
    }

    fn debug_render_scanline_bpp(&mut self, bg_i: usize, scanline: u16, bits_per_pixel: usize) {
        assert_eq!(bits_per_pixel % 2, 0);
        // TODO: Doesn't support direct color mode
        // TODO: Doesn't support large tiles
        // TODO: Reading from VRAM takes cycles...
        let bg_addr = (self.io_reg.bg_tilemap_addr_size[bg_i].base() as usize) << 10;
        let chr_addr = (self.io_reg.bg_chr_addr.bg_base(bg_i + 1) as usize) << 12;
        let (screen_cols, screen_rows) = match self.io_reg.bg_tilemap_addr_size[bg_i].size() {
            0 => (32, 32),
            1 => (64, 32),
            2 => (32, 64),
            3 => (64, 64),
            _ => unreachable!(),
        };
        let render_line = scanline as usize + self.io_reg.bg_scroll[bg_i].v.val as usize;
        let row = (render_line / 8) % screen_rows;
        let v_screen = row / 32;
        let line_offset = render_line % 8;
        let start_col = (self.io_reg.bg_scroll[bg_i].h.val / 8) as usize;
        let start_pixel_x = (self.io_reg.bg_scroll[bg_i].h.val % 8) as usize;
        let n_cols = 32 + (start_pixel_x > 0) as usize;
        for col_i in 0..n_cols {
            // TODO: Would be nice to use a bitfield for these
            let col = (start_col + col_i) % screen_cols;
            let h_screen = col / 32;
            let screen_i = match self.io_reg.bg_tilemap_addr_size[bg_i].size() {
                0 => 0,
                1 => h_screen,
                2 => v_screen,
                3 => (v_screen << 1) | h_screen,
                _ => unreachable!(),
            };
            let tile = self.vram
                [(bg_addr + screen_i * 0x400 + ((row % 32) * 32) + (col % 32)) % self.vram.len()];
            let chr_n = tile & 0x3FF;
            let tile_chr_base = chr_addr + (bits_per_pixel * 4) * chr_n as usize;
            let palette_n = (tile >> 10) & 0x7;
            let priority = (tile >> 13) & 0x1;
            let (flip_x, flip_y) = ((tile >> 14) & 1 == 1, (tile >> 15) & 1 == 1);
            let start_bit = if col_i == 0 { start_pixel_x } else { 0 };
            let end_bit = if col_i == 32 { start_pixel_x } else { 8 };
            let tile_line_pixels = self.compute_tile_line(
                tile_chr_base,
                line_offset,
                Some(palette_n),
                bits_per_pixel,
                flip_x,
                flip_y,
                false,
            );
            let line_buffs = &mut self.scanline_buffs.bg_buff[bg_i];
            for bit_i in start_bit..end_bit {
                line_buffs[priority as usize][(col_i * 8 + bit_i) - start_pixel_x] =
                    tile_line_pixels[bit_i];
            }
        }
    }

    fn compute_tile_line(
        &self,
        tile_chr_base: usize,
        line_offset: usize,
        palette_n: Option<u16>,
        bits_per_pixel: usize,
        flip_x: bool,
        flip_y: bool,
        sprite: bool,
    ) -> [Option<[u8; 3]>; 8] {
        let mut result = [None; 8];
        let line = if flip_y { 7 - line_offset } else { line_offset };
        let mut tile_plane_pairs: ArrayVec<u16, 4> = ArrayVec::new();
        for i in 0..(bits_per_pixel / 2) {
            tile_plane_pairs.push(self.vram[(i * 8 + tile_chr_base + line) % self.vram.len()]);
        }
        for bit_i in 0..8 {
            let bit = if flip_x { 7 - bit_i } else { bit_i };
            let palette_i = tile_plane_pairs.iter().rev().fold(0, |acc, word| {
                let pair = (((word >> (15 - bit)) & 1) << 1) | ((word >> (7 - bit)) & 1);
                (acc << 2) | pair
            });
            // Entry 0 in each palette is transparent.
            if palette_i == 0 {
                continue;
            }
            let palette_offset = if sprite { 0x80 } else { 0x00 };
            let palette_entry = if let Some(palette_n) = palette_n {
                self.cgram[palette_offset
                    + ((1 << bits_per_pixel) * (palette_n as usize) + palette_i as usize)
                        % self.cgram.len()]
            } else {
                // TODO: Replace fake palette and add more frontend options (different fake palettes or use cgram)
                [
                    0x0000, 0x7FDD, 0x3A49, 0x428B, 0x4ACD, 0x530F, 0x5B51, 0x6393, 0x7393, 0x0000,
                    0x0CFB, 0x2FEB, 0x7393, 0x0000, 0x7FDD, 0x2D7F,
                ][palette_i as usize % 16]
            };
            let pixel = [
                ((palette_entry & 0x1F) as u8) << 3,
                (((palette_entry >> 5) & 0x1F) as u8) << 3,
                (((palette_entry >> 10) & 0x1F) as u8) << 3,
            ];
            result[bit_i] = Some(pixel);
        }
        result
    }

    fn debug_render_scanline_mode7(&mut self, scanline: u16) {
        // TODO: Reading from VRAM takes cycles...
        // TODO: Rotate, scaling, offset
        let render_line = scanline as usize + self.io_reg.bg_scroll[0].v.val as usize;
        let row = (render_line / 8) % 128;
        let line_offset = render_line % 8;
        let start_col = (self.io_reg.bg_scroll[0].h.val / 8) as usize;
        let start_pixel_x = (self.io_reg.bg_scroll[0].h.val % 8) as usize;
        let n_cols = 32 + (start_pixel_x > 0) as usize;
        for col_i in 0..n_cols {
            let col = (start_col + col_i) % 128;
            let tile = self.vram[(row * 128 + col) % self.vram.len()];
            let chr_n = tile & 0xFF;
            let start_bit = if col_i == 0 { start_pixel_x } else { 0 };
            let end_bit = if col_i == 32 { start_pixel_x } else { 8 };
            let mut tile_line_pixels = [None; 8];
            for bit_i in 0..8 {
                let chr_data = (self.vram
                    [(64 * chr_n as usize + 8 * line_offset + bit_i) % self.vram.len()]
                    >> 8) as u8;
                let palette_entry = self.cgram[chr_data as usize];
                let pixel = [
                    ((palette_entry & 0x1F) as u8) << 3,
                    (((palette_entry >> 5) & 0x1F) as u8) << 3,
                    (((palette_entry >> 10) & 0x1F) as u8) << 3,
                ];
                tile_line_pixels[bit_i] = Some(pixel);
            }
            let line_buffs = &mut self.scanline_buffs.bg_buff[0];
            for bit_i in start_bit..end_bit {
                line_buffs[0][(col_i * 8 + bit_i) - start_pixel_x] = tile_line_pixels[bit_i];
            }
        }
    }

    pub fn debug_compute_tile(
        &self,
        tile_chr_base: usize,
        bits_per_pixel: usize,
    ) -> [[Option<[u8; 3]>; 8]; 8] {
        let mut result = [[None; 8]; 8];
        for line_offset in 0..8 {
            result[line_offset] = self.compute_tile_line(
                tile_chr_base,
                line_offset,
                None, // TODO: Frontend should probably be able to select from CGRAM palettes
                bits_per_pixel,
                false,
                false,
                false,
            );
        }
        result
    }

    fn debug_render_sprites(&mut self, scanline: u16) {
        for sprite_i in self.io_reg.oam_addr_priority.oam_priority_iter().rev() {
            let oam_lo_entry = {
                let oam_off = sprite_i * 4;
                OamLoEntry(u32::from_le_bytes(
                    self.oam_lo[oam_off..oam_off + 4].try_into().unwrap(),
                ))
            };
            let (x_hi1, large) = {
                let oam_hi_entry = self.oam_hi[sprite_i / 4];
                let offset = 2 * (sprite_i % 4);
                (
                    (oam_hi_entry >> offset) & 1,
                    (oam_hi_entry >> (offset + 1)) & 1 == 1,
                )
            };
            let (width, height) = self.io_reg.obj_size_base.obj_width_height(large);
            // The rendering of sprites is offset by +1 from backgrounds, but since scanline 0 is invisible,
            // it cancels out. i.e. a sprite at y=0 will be drawn starting on the first visible scanline.
            let y = oam_lo_entry.y() as u16 + 1;
            if scanline < y || scanline >= (y + height) {
                continue;
            }
            let x_lo8 = oam_lo_entry.x_lo8();
            let chr_n = oam_lo_entry.chr_n();
            let attr = oam_lo_entry.attr();
            self.debug_render_sprite(scanline, x_hi1, x_lo8, y, width, height, chr_n, attr);
        }
    }

    fn debug_render_sprite(
        &mut self,
        scanline: u16,
        x_hi1: u8,
        x_lo8: u8,
        y: u16,
        width: u16,
        height: u16,
        first_chr_n: u8,
        attr: ObjAttributes,
    ) {
        let line = {
            let line_offset = scanline - y;
            if attr.flip_y() {
                (height - 1) - line_offset
            } else {
                line_offset
            }
        };
        // Each of the two sprite nametables is laid out in VRAM as a 2D 16x16 grid of 8x8 tiles.
        // The sprite's 8-bit tile number can be seen as two nybbles representing Y (row) and X (col)
        // in this grid. These X and Y values both wrap without carry.
        let vram_row = ((first_chr_n as u16) >> 4).wrapping_add(line / 8) & 0xF;
        let palette_n = attr.palette_n();
        let n_cols = width / 8;
        for col_i in 0..n_cols {
            let col = if attr.flip_x() {
                (n_cols - 1) - col_i
            } else {
                col_i
            };
            let chr_n = {
                let vram_col = (first_chr_n as u16 & 0xF).wrapping_add(col) & 0xF;
                (vram_row << 4) | vram_col
            };
            let tile_chr_base = self
                .io_reg
                .obj_size_base
                .calculate_vram_addr(attr.nametable_select(), chr_n as u8);
            let line_offset = line % 8;
            let tile_line_pixels = self.compute_tile_line(
                tile_chr_base as usize,
                line_offset as usize,
                Some(palette_n as u16),
                4,
                attr.flip_x(),
                false, // If needed, we already flipped vertically based on height
                true,
            );
            for bit_i in 0..8 {
                let pixel_x = {
                    let x = (x_lo8 as u16 + col_i * 8 + bit_i as u16) as i16 - (x_hi1 as i16 * 256);
                    if !(0..255).contains(&x) {
                        continue;
                    }
                    x as usize
                };
                // Explicitly ignore transparent pixels, so as not to override other sprites with one's transparency
                if tile_line_pixels[bit_i].is_some() {
                    self.scanline_buffs.obj_buff[attr.priority() as usize][pixel_x] =
                        tile_line_pixels[bit_i];
                    self.scanline_buffs.obj_hipal_buff[pixel_x] = palette_n >= 4;
                }
            }
        }
    }

    pub fn debug_get_frame(&self) -> [[[u8; 3]; 256]; 224] {
        *self.frame
    }

    pub fn io_peak(&self, _addr: u16) -> u8 {
        // TODO
        0
    }

    pub fn io_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2138 => {
                let oam_addr = self.io_reg.curr_oam_addr as usize;
                self.io_reg.curr_oam_addr = self.io_reg.curr_oam_addr.wrapping_add(1);
                if oam_addr < 0x200 {
                    self.oam_lo[oam_addr % self.oam_lo.len()]
                } else {
                    self.oam_hi[(oam_addr - 0x200) % self.oam_hi.len()]
                }
            }
            0x2139 => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(false);
                let len_tmp = self.vram.len();
                self.vram[vram_addr % len_tmp].bit_range(7, 0)
            }
            0x213A => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(true);
                let len_tmp = self.vram.len();
                self.vram[vram_addr % len_tmp].bit_range(15, 8)
            }
            0x213B => {
                let cgram_addr = self.io_reg.cgram_addr as usize;
                let read_bits = self.io_reg.cgram_bits();
                self.io_reg.cgram_access_latch = !self.io_reg.cgram_access_latch;
                self.io_reg.cgram_addr = self.io_reg.cgram_addr.wrapping_add(1);
                self.cgram[cgram_addr].bit_range(read_bits.0, read_bits.1)
            }
            // TODO: Open bus?
            _ => {
                log::debug!("TODO: PPU IO read {addr:04X}");
                0
            } // TODO: Remove this fallback
              // _ => panic!("Invalid IO read of PPU at {addr:#04X}"),
        }
    }

    pub fn io_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2100 => self.io_reg.display_control_1.0 = data,
            0x2101 => self.io_reg.obj_size_base.0 = data,
            0x2102 => {
                self.io_reg.oam_addr_priority.set_lo_byte(data);
                self.reload_oam_addr();
            }
            0x2103 => {
                self.io_reg.oam_addr_priority.set_hi_byte(data);
                self.reload_oam_addr();
            }
            0x2104 => {
                let oam_addr = self.io_reg.curr_oam_addr as usize;
                self.io_reg.curr_oam_addr = self.io_reg.curr_oam_addr.wrapping_add(1);
                let even_byte = (oam_addr & 1) == 0;
                // Writes to even bytes are latched, even if they're to the upper 32 bytes of OAM
                if even_byte {
                    self.io_reg.oam_even_latch = data;
                }
                if oam_addr >= 0x200 {
                    // Writes to the high 32 bytes of OAM are committed directly
                    self.oam_hi[oam_addr & 0x1F] = data;
                } else if !even_byte {
                    // Writes to the lower 512 bytes of OAM are committed in pairs when an odd address is written
                    self.oam_lo[oam_addr - 1] = self.io_reg.oam_even_latch;
                    self.oam_lo[oam_addr] = data;
                }
            }
            0x2105 => self.io_reg.bg_mode.0 = data,
            0x2106 => self.io_reg.mosaic.0 = data,
            0x2107..=0x210A => self.io_reg.bg_tilemap_addr_size[addr as usize - 0x2107].0 = data,
            0x210B => self.io_reg.bg_chr_addr.set_lo_byte(data),
            0x210C => self.io_reg.bg_chr_addr.set_hi_byte(data),
            // TODO: 0x210D and 0x210E are also mode7 registers; consider storing them separately?
            0x210D..=0x2114 => {
                let (bg_i, component_i) = {
                    let reg_offset = addr - 0x210D;
                    (reg_offset / 2, reg_offset % 2)
                };
                if component_i == 0 {
                    self.io_reg.bg_scroll[bg_i as usize].h.write_next(data);
                } else {
                    self.io_reg.bg_scroll[bg_i as usize].v.write_next(data);
                }
            }
            0x2115 => self.io_reg.vram_addr_incr_mode.0 = data,
            0x2116 => self.io_reg.vram_addr.set_bit_range(7, 0, data),
            0x2117 => self.io_reg.vram_addr.set_bit_range(15, 8, data),
            0x2118 => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(false);
                let len_tmp = self.vram.len();
                self.vram[vram_addr % len_tmp].set_bit_range(7, 0, data);
            }
            0x2119 => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(true);
                let len_tmp = self.vram.len();
                self.vram[vram_addr % len_tmp].set_bit_range(15, 8, data);
            }
            0x211A..=0x2120 => {} // TODO: Rotation/scaling
            0x2121 => {
                self.io_reg.cgram_addr = data;
                self.io_reg.cgram_access_latch = false;
            }
            0x2122 => {
                let cgram_addr = self.io_reg.cgram_addr as usize;
                let write_bits = self.io_reg.cgram_bits();
                self.io_reg.cgram_access_latch = !self.io_reg.cgram_access_latch;
                if !self.io_reg.cgram_access_latch {
                    self.io_reg.cgram_addr = self.io_reg.cgram_addr.wrapping_add(1);
                }
                self.cgram[cgram_addr].set_bit_range(write_bits.0, write_bits.1, data);
            }
            0x2123 => self.io_reg.window_mask.bg1_bg2_masks.0 = data,
            0x2124 => self.io_reg.window_mask.bg3_bg4_masks.0 = data,
            0x2125 => self.io_reg.window_mask.obj_math_masks.0 = data,
            0x2126 => self.io_reg.window_pos[0].left = data,
            0x2127 => self.io_reg.window_pos[0].right = data,
            0x2128 => self.io_reg.window_pos[1].left = data,
            0x2129 => self.io_reg.window_pos[1].right = data,
            0x212A => self.io_reg.window_bg_logic.0 = data,
            0x212B => self.io_reg.window_obj_math_logic.0 = data,
            0x212C => self.io_reg.main_layer_enable.0 = data,
            0x212D => self.io_reg.sub_layer_enable.0 = data,
            0x212E => self.io_reg.window_main_screen_disable.0 = data,
            0x212F => self.io_reg.window_sub_screen_disable.0 = data,
            0x2130 => self.io_reg.color_math_control_a.0 = data,
            0x2131 => self.io_reg.color_math_control_b.0 = data,
            0x2132 => {
                let intensity = data & 0x1F;
                // Apply intensity to red/green/blue depending on bits 5/6/7
                for color_i in 0..3 {
                    if (data >> (5 + color_i)) & 1 == 1 {
                        self.io_reg.color_math_backdrop_color[color_i] = intensity << 3;
                    }
                }
            }
            // TODO: 2133h - SETINI - Display Control 2 (W)
            _ => log::debug!("TODO: PPU IO write {addr:04X}: {data:02X}"), // TODO: Remove this fallback
                                                                           // _ => panic!("Invalid IO write of PPU at {addr:#04X}"),
        }
    }

    pub fn reload_oam_addr(&mut self) {
        self.io_reg.curr_oam_addr = self.io_reg.oam_addr_priority.addr() << 1;
    }
}
