pub mod counter;
mod registers;

use bitfield::{BitRange, BitRangeMut};
pub use counter::PpuCounter;
use registers::IoRegisters;

use crate::cpu::yield_ticks;
use crate::scheduler::*;

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

pub struct PPU {
    vram: Vec<u16>,
    cgram: Vec<u16>,
    io_reg: IoRegisters,
    ppu_counter: Rc<RefCell<PpuCounter>>,
    frame: [[[u8; 3]; 256]; 224],
    ticks_run: u64,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            io_reg: IoRegisters::new(),
            vram: vec![0; 0x8000],
            cgram: vec![0; 0x100],
            ppu_counter: Rc::new(RefCell::new(PpuCounter::new())),
            frame: [[[0; 3]; 256]; 224],
            ticks_run: 0,
        }
    }

    pub fn reset(&mut self) {
        self.vram.fill(0);
        self.ticks_run = 0;
        // TODO: Most of these are indeterminate, but it might be good to initialize to 0 for determinism
        self.io_reg = IoRegisters::new();
    }

    pub fn run<'a>(ppu: Rc<RefCell<PPU>>) -> impl DeviceGenerator + 'a {
        move || loop {
            yield_ticks!(ppu, PPU::step(ppu.clone(), 1364));
        }
    }

    fn step<'a>(ppu: Rc<RefCell<PPU>>, n_clocks: u64) -> impl Yieldable<()> + 'a {
        move || {
            ppu.borrow_mut().ticks_run += n_clocks;
            let new_scanline = yield_all!(PpuCounter::tick(
                ppu.borrow().ppu_counter.clone(),
                n_clocks as u16
            ));
            if new_scanline {
                let scanline = ppu.borrow().ppu_counter.borrow().scanline;
                if scanline < 225 {
                    ppu.borrow_mut().debug_render_scanline(scanline);
                }
            }
            yield YieldReason::Sync(Device::CPU);
        }
    }

    fn debug_render_scanline(&mut self, scanline: u16) {
        if scanline == 0 {
            return;
        }
        let draw_line = scanline - 1;
        match self.io_reg.bg_mode.bg_mode() {
            // TODO: These are currently just the first layers
            0 => self.debug_render_scanline_bpp(draw_line, 2),
            1 => self.debug_render_scanline_bpp(draw_line, 4),
            2 => self.debug_render_scanline_bpp(draw_line, 4),
            3 => self.debug_render_scanline_bpp(draw_line, 8),
            5 => self.debug_render_scanline_bpp(draw_line, 4),
            6 => self.debug_render_scanline_bpp(draw_line, 4),
            // TODO
            _ => {}
        }
    }

    fn debug_render_scanline_bpp(&mut self, scanline: u16, bits_per_pixel: usize) {
        assert_eq!(bits_per_pixel % 2, 0);
        // TODO: Doesn't support direct color mode
        let bg_addr = (self.io_reg.bg_tilemap_addr_size[0].base() as usize) << 10;
        let chr_addr = (self.io_reg.bg_chr_addr.bg1_base() as usize) << 12;
        let (screen_cols, screen_rows) = match self.io_reg.bg_tilemap_addr_size[0].size() {
            0 => (32, 32),
            1 => (64, 32),
            2 => (32, 64),
            3 | _ => (64, 64),
        };
        let render_line = scanline as usize + self.io_reg.bg_scroll[0].v.val as usize;
        let row = (render_line / 8) % screen_rows;
        let v_screen = row / 32;
        let line = render_line % 8;
        let start_col = (self.io_reg.bg_scroll[0].h.val / 8) as usize;
        let start_pixel_x = (self.io_reg.bg_scroll[0].h.val % 8) as usize;
        let n_cols = 32 + (start_pixel_x > 0) as usize;
        for col_i in 0..n_cols {
            // TODO: Would be nice to use a bitfield for these
            let col = (start_col + col_i) % screen_cols;
            let h_screen = col / 32;
            let screen_i = match self.io_reg.bg_tilemap_addr_size[0].size() {
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
            let tile_plane_pairs: Vec<u16> = (0..(bits_per_pixel / 2))
                .map(|i| self.vram[i * 8 + tile_chr_base + line])
                .collect();
            let start_bit = if col_i == 0 { start_pixel_x } else { 0 };
            let end_bit = if col_i == 32 { start_pixel_x } else { 8 };
            for bit in start_bit..end_bit {
                let palette_i = tile_plane_pairs.iter().rev().fold(0, |acc, word| {
                    let pair = (((word >> (15 - bit)) & 1) << 1) | ((word >> (7 - bit)) & 1);
                    (acc << 2) | pair
                });
                let palette_entry =
                    self.cgram[(1 << bits_per_pixel) * (palette_n as usize) + palette_i as usize];
                let pixel = &mut self.frame[scanline as usize][(col_i * 8 + bit) - start_pixel_x];
                pixel[0] = ((palette_entry & 0x1F) as u8) << 3;
                pixel[1] = (((palette_entry >> 5) & 0x1F) as u8) << 3;
                pixel[2] = (((palette_entry >> 10) & 0x1F) as u8) << 3;
            }
        }
    }

    pub fn debug_get_frame(&self) -> [[[u8; 3]; 256]; 224] {
        self.frame
    }

    pub fn io_peak(&self, addr: u16) -> u8 {
        // TODO
        0
    }

    pub fn io_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2139 => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(false);
                self.vram[vram_addr].bit_range(7, 0)
            }
            0x213A => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(true);
                self.vram[vram_addr].bit_range(15, 8)
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
            _ => panic!("Invalid IO read of PPU at {addr:#04X}"),
        }
    }

    pub fn io_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2100 => self.io_reg.display_control_1.0 = data,
            0x2102 => {} // TODO: OAM addr lo byte
            0x2103 => {} // TODO: OAM addr upper bit and priority, rotation
            0x2104 => {} // TODO: OAM data
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
                self.vram[vram_addr].set_bit_range(7, 0, data);
            }
            0x2119 => {
                let vram_addr = self.io_reg.vram_addr as usize;
                self.io_reg.update_vram_addr(true);
                self.vram[vram_addr].set_bit_range(15, 8, data);
            }
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
            0x2123..=0x212B | 0x212E..=0x212F => {} // TODO: Window
            _ => log::debug!("TODO: PPU IO write {addr:04X}: {data:02X}"), // TODO: Remove this fallback
            _ => panic!("Invalid IO write of PPU at {addr:#04X}"),
        }
    }
}
