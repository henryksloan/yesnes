pub mod counter;
mod registers;

pub use counter::PpuCounter;
use registers::IoRegisters;

use crate::scheduler::{Device, DeviceGenerator, YieldReason};

pub struct PPU {
    io_reg: IoRegisters,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            io_reg: IoRegisters::new(),
        }
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("PPU");
            yield (YieldReason::Sync(Device::CPU), 5);
        }
    }

    pub fn io_read(&mut self, addr: u16) -> u8 {
        match addr {
            // TODO: Open bus?
            _ => {
                log::debug!("TODO: PPU IO read {addr}");
                0
            } // TODO: Remove this fallback
            _ => panic!("Invalid IO read of PPU at {addr:#06X}"),
        }
    }

    pub fn io_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2100 => self.io_reg.display_control_1.0 = data,
            0x2105 => self.io_reg.bg_mode.0 = data,
            0x2106 => self.io_reg.mosaic.0 = data,
            0x2107..=0x210A => self.io_reg.bg_tilemap_addr_size[addr as usize - 0x2701].0 = data,
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
            _ => log::debug!("TODO: PPU IO write {addr}: {data:02X}"), // TODO: Remove this fallback
            _ => panic!("Invalid IO write of PPU at {addr:#06X}"),
        }
    }
}
