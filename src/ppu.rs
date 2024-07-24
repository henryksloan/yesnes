pub mod counter;
mod obj;
mod registers;

pub use counter::PpuCounter;

use bitfield::{BitRange, BitRangeMut};
use obj::{OamLoEntry, ObjAttributes};
use registers::IoRegisters;

use crate::cpu::yield_ticks;
use crate::scheduler::*;

use std::cell::RefCell;
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;

/// Holds the pixels of each layer on this scanline, that they may be combined according to priority
struct ScanlineBuffers {
    // For each bg layer, buffers of high- and low-priority pixels (0=Lower, 1=Higher)
    bg_buff: [[[Option<[u8; 3]>; 256]; 2]; 4],
    // A buffer of pixels at each object priority (0-3)
    obj_buff: [[Option<[u8; 3]>; 256]; 4],
}

impl Default for ScanlineBuffers {
    fn default() -> Self {
        Self {
            bg_buff: [[[None; 256]; 2]; 4],
            obj_buff: [[None; 256]; 4],
        }
    }
}

enum SubLayer {
    Obj(usize),       // Objects with the given priority
    Bg(usize, usize), // .0: BG index, .1: Priority
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
        2 | 3 | 4 | 5 => &[Obj(3), Bg(1, 1), Obj(2), Bg(2, 1), Obj(1), Bg(1, 0), Obj(0), Bg(2, 0)],
        6 => &[Obj(3), Bg(1, 1), Obj(2), Obj(1), Bg(1, 0), Obj(0)],
        7 | _ => &[Obj(3), Obj(2), Bg(2, 1), Obj(1), Bg(1, 0), Obj(0), Bg(2, 0)],
    }
}

pub struct PPU {
    vram: Vec<u16>,
    cgram: Vec<u16>,
    // OAM can be seen as a 512-byte low table and a 32-byte upper table.
    // The two tables have different data formats and different write behavior.
    oam_lo: Vec<u8>,
    oam_hi: Vec<u8>,
    io_reg: IoRegisters,
    ppu_counter: Rc<RefCell<PpuCounter>>,
    frame: [[[u8; 3]; 256]; 224],
    scanline_buffs: ScanlineBuffers,
    ticks_run: u64,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            io_reg: IoRegisters::new(),
            vram: vec![0; 0x8000],
            cgram: vec![0; 0x100],
            oam_lo: vec![0; 0x200],
            oam_hi: vec![0; 0x20],
            ppu_counter: Rc::new(RefCell::new(PpuCounter::new())),
            frame: [[[0; 3]; 256]; 224],
            scanline_buffs: ScanlineBuffers::default(),
            ticks_run: 0,
        }
    }

    pub fn reset(&mut self) {
        self.vram.fill(0);
        self.ticks_run = 0;
        // TODO: Most of these are indeterminate, but it might be good to initialize to 0 for determinism
        self.io_reg = IoRegisters::new();
    }

    pub fn run<'a>(ppu: Rc<RefCell<PPU>>) -> impl DeviceCoroutine + 'a {
        #[coroutine]
        move || loop {
            // TODO: This is scanline-granularity, and does no synchronization below that granularity.
            yield_ticks!(ppu, PPU::step(ppu.clone(), 1364));
        }
    }

    fn step<'a>(ppu: Rc<RefCell<PPU>>, n_clocks: u64) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            ppu.borrow_mut().ticks_run += n_clocks;
            let new_scanline = yield_all!(PpuCounter::tick(
                ppu.borrow().ppu_counter.clone(),
                n_clocks as u16
            ));
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
        if self.io_reg.display_control_1.forced_blank() {
            self.frame[draw_line as usize].fill([0, 0, 0]);
            return;
        }
        // The color at CGRAM[0] is drawn if all other layers are transparent; we fill it in first
        // to be overdrawn by any non-transparent pixels.
        let backdrop_color = self.palette_entry_to_rgb(self.cgram[0]);
        self.frame[draw_line as usize].fill(backdrop_color);
        self.debug_clear_scanline_buffs();
        // TODO: Support layer disablement (in which case, no need to draw some layers)
        let layer_bpps: &[usize] = match self.io_reg.bg_mode.bg_mode() {
            0 => &[2, 2, 2, 2],
            1 => &[4, 4, 2],
            2 => &[4, 4], // TODO: Offset-per-pixel
            3 => &[8, 4],
            4 => &[8, 2],  // TODO: Offset-per-pixel
            5 => &[4, 2],  // TODO: Hi-res
            6 => &[4],     // TODO: Hi-res and Offset-per-pixel
            7 | _ => &[8], // TODO: Rotation/scaling
        };
        for (bg_i, layer_bpp) in layer_bpps.iter().enumerate() {
            if self.io_reg.main_layer_enable.layer_enabled(bg_i + 1) {
                self.debug_render_scanline_bpp(bg_i, draw_line, *layer_bpp);
            }
        }
        if self.io_reg.main_layer_enable.obj() {
            self.debug_render_sprites(draw_line);
        }
        for sublayer in layer_priority_order(
            self.io_reg.bg_mode.bg_mode(),
            self.io_reg.bg_mode.bg3_priority(),
        )
        .iter()
        .rev()
        {
            let line = match *sublayer {
                SubLayer::Obj(priority) => &self.scanline_buffs.obj_buff[priority],
                SubLayer::Bg(bg_n, priority) => &self.scanline_buffs.bg_buff[bg_n - 1][priority],
            };
            for x in 0..256 {
                if let Some(color) = line[x] {
                    self.frame[draw_line as usize][x] = color;
                }
            }
        }
    }

    fn debug_clear_scanline_buffs(&mut self) {
        for line_buffs in &mut self.scanline_buffs.bg_buff {
            line_buffs[0].fill(None);
            line_buffs[1].fill(None);
        }
        for obj_buff in &mut self.scanline_buffs.obj_buff {
            obj_buff.fill(None);
        }
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
            3 | _ => (64, 64),
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
                3 | _ => (v_screen << 1) | h_screen,
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
                palette_n,
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
        palette_n: u16,
        bits_per_pixel: usize,
        flip_x: bool,
        flip_y: bool,
        sprite: bool,
    ) -> [Option<[u8; 3]>; 8] {
        let mut result = [None; 8];
        let line = if flip_y { 7 - line_offset } else { line_offset };
        let tile_plane_pairs: Vec<u16> = (0..(bits_per_pixel / 2))
            .map(|i| self.vram[(i * 8 + tile_chr_base + line) % self.vram.len()])
            .collect();
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
            let palette_entry = self.cgram[palette_offset
                + ((1 << bits_per_pixel) * (palette_n as usize) + palette_i as usize)
                    % self.cgram.len()];
            let pixel = [
                ((palette_entry & 0x1F) as u8) << 3,
                (((palette_entry >> 5) & 0x1F) as u8) << 3,
                (((palette_entry >> 10) & 0x1F) as u8) << 3,
            ];
            result[bit_i] = Some(pixel);
        }
        result
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
                0, // TODO: Frontend should probably be able to select from CGRAM palettes
                bits_per_pixel,
                false,
                false,
                false,
            );
        }
        result
    }

    fn debug_render_sprites(&mut self, scanline: u16) {
        for sprite_i in (0..128).rev() {
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
            let y = oam_lo_entry.y() as u16;
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
        // The rendering of sprites is offset by +1 from backgrounds, but since scanline 0 is invisible,
        // it cancels out. i.e. a sprite at y=0 will be drawn starting on the first visible scanline.
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
                palette_n as u16,
                4,
                attr.flip_x(),
                attr.flip_y(),
                true,
            );
            for bit_i in 0..8 {
                let pixel_x = {
                    let x = (x_lo8 as u16 + col_i * 8 + bit_i as u16) as i16 - (x_hi1 as i16 * 256);
                    if x >= 255 || x < 0 {
                        continue;
                    }
                    x as usize
                };
                // Explicitly ignore transparent pixels, so as not to override other sprites with one's transparency
                if tile_line_pixels[bit_i].is_some() {
                    self.scanline_buffs.obj_buff[attr.priority() as usize][pixel_x] =
                        tile_line_pixels[bit_i];
                }
            }
        }
    }

    pub fn debug_get_frame(&self) -> [[[u8; 3]; 256]; 224] {
        self.frame
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
                    self.oam_hi[oam_addr as usize & 0x1F] = data;
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
            // TODO: Should reads of these work? What is the program tries to OR or something?
            0x212C => self.io_reg.main_layer_enable.0 = data,
            0x212D => self.io_reg.sub_layer_enable.0 = data,
            0x2123..=0x212B | 0x212E..=0x212F => {} // TODO: Window
            _ => log::debug!("TODO: PPU IO write {addr:04X}: {data:02X}"), // TODO: Remove this fallback
                                                                           // _ => panic!("Invalid IO write of PPU at {addr:#04X}"),
        }
    }

    pub fn reload_oam_addr(&mut self) {
        self.io_reg.curr_oam_addr = self.io_reg.oam_addr_priority.addr() << 1;
    }
}
