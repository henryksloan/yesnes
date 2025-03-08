mod registers;

use registers::IoRegisters;

use crate::cartridge::Mapper;
use crate::u24::u24;

use std::ops::DerefMut;

pub struct SA1 {
    rom: Vec<u8>,
    sram: Box<dyn DerefMut<Target = [u8]>>,
    iram: Box<[u8; 0x800]>,
    io_reg: IoRegisters,
}

impl SA1 {
    pub fn new(rom: Vec<u8>, sram: Box<dyn DerefMut<Target = [u8]>>) -> Self {
        Self {
            rom,
            sram,
            iram: vec![0; 0x800].try_into().unwrap(),
            io_reg: IoRegisters::new(),
        }
    }
}

impl SA1 {
    // DO NOT SUBMIT: Might have to make this mut at some point, and hence try_read_u8...
    pub fn io_read(&self, addr: u24) -> u8 {
        match addr.lo16() {
            _ => {
                // DO NOT SUBMIT: Still log, but add a `peek` function that doesn't
                // log::debug!("TODO: SA-1 IO read {addr}"); // TODO: Remove this fallback
                0
            } // _ => panic!("Invalid IO read of SA-1 at {addr:#06X}"),
        }
    }

    pub fn io_write(&mut self, addr: u24, data: u8) {
        match addr.lo16() {
            0x2220..=0x2223 => {
                self.io_reg.mmc_bank_controls[addr.lo16() as usize - 0x2220].0 = data;
            }
            0x2224 => self.io_reg.snes_bw_ram_bank_control.0 = data,
            // DO NOT SUBMIT: Some of these are from the SA-1 side
            0x2225 => self.io_reg.sa1_bw_ram_bank_control.0 = data,
            // _ => {}
            _ => log::debug!("TODO: SA-1 IO write {addr}: {data:02X}"), // TODO: Remove this fallback
                                                                        // _ => panic!("Invalid IO write of SA-1 at {addr:#06X} with data {data:#02X}"),
        }
    }
}

impl Mapper for SA1 {
    fn reset(&mut self) {
        self.io_reg.mmc_bank_controls[0].0 = 0;
        self.io_reg.mmc_bank_controls[1].0 = 1;
        self.io_reg.mmc_bank_controls[2].0 = 2;
        self.io_reg.mmc_bank_controls[3].0 = 3;
        self.io_reg.snes_bw_ram_bank_control.0 = 0;
        self.io_reg.sa1_bw_ram_bank_control.0 = 0;
    }

    // DO NOT SUBMIT: Differentiate CPU and SA-1 perspective
    fn try_read_u8(&self, addr: u24) -> Option<u8> {
        match addr.bank() {
            0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                // DO NOT SUBMIT: Fix these. e.g. Super Metroid does weird mirroring stuff in banks 60h+
                Some(self.rom[(0x7F00 | (addr.lo16() & 0xFF)) as usize % self.rom.len()])
            }
            0x00..=0x3F | 0x80..=0xBF => match addr.lo16() {
                0x2200..=0x23FF => Some(self.io_read(addr)),
                0x3000..=0x37FF => Some(self.iram[addr.lo16() as usize - 0x3000]),
                // DO NOT SUBMIT: This is different for CPU and SA-1
                0x6000..=0x7FFF => Some(
                    self.sram[(self.io_reg.snes_bw_ram_bank_control.base()
                        + (addr.lo16() - 0x6000))
                        .raw()
                        % self.sram.len()],
                ),
                // 0x6000..=0x7FFF => Some(
                //     self.sram[(self.io_reg.sa1_bw_ram_bank_control.base() + (addr.lo16() - 0x6000))
                //         .raw()
                //         % self.sram.len()],
                // ),
                0x8000..=0xFFFF => match addr.bank() {
                    0x00..=0x1F => {
                        let area = if self.io_reg.mmc_bank_controls[0].lorom() {
                            self.io_reg.mmc_bank_controls[0].bank() as usize
                        } else {
                            // DO NOT SUBMIT: Not clear to me if this is mirrored
                            0
                        };
                        Some(
                            self.rom[(area * 0x10_0000
                                + (addr.bank() as usize - 0x00) * 0x8000
                                + (addr.lo16() as usize - 0x8000))
                                % self.rom.len()],
                        )
                    }
                    0x20..=0x3F => {
                        let area = if self.io_reg.mmc_bank_controls[1].lorom() {
                            self.io_reg.mmc_bank_controls[1].bank() as usize
                        } else {
                            1
                        };
                        Some(
                            self.rom[(area * 0x10_0000
                                + (addr.bank() as usize - 0x20) * 0x8000
                                + (addr.lo16() as usize - 0x8000))
                                % self.rom.len()],
                        )
                    }
                    0x80..=0x9F => {
                        let area = if self.io_reg.mmc_bank_controls[2].lorom() {
                            self.io_reg.mmc_bank_controls[2].bank() as usize
                        } else {
                            2
                        };
                        Some(
                            self.rom[(area * 0x10_0000
                                + (addr.bank() as usize - 0x80) * 0x8000
                                + (addr.lo16() as usize - 0x8000))
                                % self.rom.len()],
                        )
                    }
                    0xA0..=0xBF | _ => {
                        let area = if self.io_reg.mmc_bank_controls[3].lorom() {
                            self.io_reg.mmc_bank_controls[3].bank() as usize
                        } else {
                            3
                        };
                        Some(
                            self.rom[(area * 0x10_0000
                                + (addr.bank() as usize - 0xA0) * 0x8000
                                + (addr.lo16() as usize - 0x8000))
                                % self.rom.len()],
                        )
                    }
                },
                _ => None,
            },
            0x40..=0x4F => Some(
                self.sram[(((addr.bank() as usize - 0x40) % 4) * 0x10000 + addr.lo16() as usize)
                    % self.sram.len()],
            ),
            0xC0..=0xCF => Some(
                self.rom[(self.io_reg.mmc_bank_controls[0].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xC0_0000))
                    % self.rom.len()],
            ),
            0xD0..=0xDF => Some(
                self.rom[(self.io_reg.mmc_bank_controls[1].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xD0_0000))
                    % self.rom.len()],
            ),
            0xE0..=0xEF => Some(
                self.rom[(self.io_reg.mmc_bank_controls[2].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xE0_0000))
                    % self.rom.len()],
            ),
            0xF0..=0xFF => Some(
                self.rom[(self.io_reg.mmc_bank_controls[3].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xF0_0000))
                    % self.rom.len()],
            ),
            _ => None,
        }
    }

    // DO NOT SUBMIT: Differentiate CPU and SA-1 perspective
    fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
        match addr.bank() {
            0x00..=0x3F | 0x80..=0xBF => match addr.lo16() {
                0x2200..=0x23FF => {
                    self.io_write(addr, data);
                    true
                }
                0x3000..=0x37FF => {
                    self.iram[addr.lo16() as usize - 0x3000] = data;
                    true
                }
                // DO NOT SUBMIT: This is different for CPU and SA-1
                0x6000..=0x7FFF => {
                    let len = self.sram.len();
                    self.sram[(self.io_reg.snes_bw_ram_bank_control.base()
                        + (addr.lo16() - 0x6000))
                        .raw()
                        % len] = data;
                    true
                }
                // 0x6000..=0x7FFF => {
                //     let len = self.sram.len();
                //     self.sram[(self.io_reg.sa1_bw_ram_bank_control.base() + (addr.lo16() - 0x6000))
                //         .raw()
                //         % len] = data;
                //     true
                // }
                _ => false,
            },
            0x40..=0x4F => {
                let len = self.sram.len();
                self.sram[(((addr.bank() - 0x40) as usize % 4) * 0x10000 + addr.lo16() as usize)
                    % len] = data;
                true
            }
            _ => false,
        }
    }
}
