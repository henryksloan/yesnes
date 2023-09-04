pub mod counter;
mod registers;

use bitfield::{BitRange, BitRangeMut};
pub use counter::PpuCounter;
use registers::IoRegisters;

use crate::scheduler::{Device, DeviceGenerator, YieldReason};

pub struct PPU {
    vram: Vec<u16>,
    cgram: Vec<u16>,
    io_reg: IoRegisters,
    ticks_run: u64,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            io_reg: IoRegisters::new(),
            vram: vec![0; 0x8000],
            cgram: vec![0; 0x100],
            ticks_run: 0,
        }
    }

    pub fn reset(&mut self) {
        self.vram.fill(0);
        self.ticks_run = 0;
        // TODO: Most of these are indeterminate, but it might be good to initialize to 0 for determinism
        self.io_reg = IoRegisters::new();
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("PPU");
            yield (YieldReason::Sync(Device::CPU), 5);
        }
    }

    fn debug_get_frame_mode0(&self, frame: &mut [[[u8; 3]; 256]; 224]) {
        let bg_addr = (self.io_reg.bg_tilemap_addr_size[0].base() as usize) << 10;
        let chr_addr = (self.io_reg.bg_chr_addr.bg1_base() as usize) << 12;
        for row in 0..28 {
            for col in 0..32 {
                // TODO: Would be nice to use a bitfield for these
                let tile = self.vram[bg_addr + (row * 32) + col];
                let chr_n = tile & 0x3FF;
                let tile_chr_base = chr_addr + 8 * chr_n as usize;
                let palette_n = (tile >> 10) & 0x7;
                for line in 0..8 {
                    let tile_data = self.vram[tile_chr_base + line];
                    for bit in 0..8 {
                        let palette_i =
                            (((tile_data >> (15 - bit)) & 1) << 1) | ((tile_data >> (7 - bit)) & 1);
                        let palette_entry =
                            self.cgram[4 * (palette_n as usize) + palette_i as usize];
                        let pixel = &mut frame[row * 8 + line][col * 8 + bit];
                        pixel[0] = ((palette_entry & 0x1F) as u8) << 3;
                        pixel[1] = (((palette_entry >> 5) & 0x1F) as u8) << 3;
                        pixel[2] = (((palette_entry >> 10) & 0x1F) as u8) << 3;
                    }
                }
            }
        }
    }

    fn debug_get_frame_mode1(&self, frame: &mut [[[u8; 3]; 256]; 224]) {
        let bg_addr = (self.io_reg.bg_tilemap_addr_size[0].base() as usize) << 10;
        let chr_addr = (self.io_reg.bg_chr_addr.bg1_base() as usize) << 12;
        for row in 0..28 {
            for col in 0..32 {
                // TODO: Would be nice to use a bitfield for these
                let tile = self.vram[bg_addr + (row * 32) + col];
                let chr_n = tile & 0x3FF;
                let tile_chr_base = chr_addr + 16 * chr_n as usize;
                let palette_n = (tile >> 10) & 0x7;
                for line in 0..8 {
                    let tile_data_lo = self.vram[tile_chr_base + line];
                    let tile_data_hi = self.vram[8 + tile_chr_base + line];
                    for bit in 0..8 {
                        let palette_i = {
                            let lo = (((tile_data_lo >> (15 - bit)) & 1) << 1)
                                | ((tile_data_lo >> (7 - bit)) & 1);
                            let hi = (((tile_data_hi >> (15 - bit)) & 1) << 1)
                                | ((tile_data_hi >> (7 - bit)) & 1);
                            (hi << 2) | lo
                        };
                        let palette_entry =
                            self.cgram[16 * (palette_n as usize) + palette_i as usize];
                        let pixel = &mut frame[row * 8 + line][col * 8 + bit];
                        pixel[0] = ((palette_entry & 0x1F) as u8) << 3;
                        pixel[1] = (((palette_entry >> 5) & 0x1F) as u8) << 3;
                        pixel[2] = (((palette_entry >> 10) & 0x1F) as u8) << 3;
                    }
                }
            }
        }
    }

    pub fn debug_get_frame(&self) -> [[[u8; 3]; 256]; 224] {
        let mut frame = [[[0; 3]; 256]; 224];
        match self.io_reg.bg_mode.bg_mode() {
            0 => self.debug_get_frame_mode0(&mut frame),
            1 => self.debug_get_frame_mode1(&mut frame),
            // TODO
            _ => {}
        }
        frame
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
