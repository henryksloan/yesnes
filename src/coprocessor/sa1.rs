mod registers;

use registers::IoRegisters;

use crate::cartridge::Mapper;
use crate::u24::u24;

use std::ops::DerefMut;

pub struct SA1 {
    rom: Vec<u8>,
    sram: Box<dyn DerefMut<Target = [u8]>>,
    io_reg: IoRegisters,
}

impl SA1 {
    pub fn new(rom: Vec<u8>, sram: Box<dyn DerefMut<Target = [u8]>>) -> Self {
        Self {
            rom,
            sram,
            io_reg: IoRegisters::new(),
        }
    }
}

impl Mapper for SA1 {
    fn reset(&mut self) {
        self.io_reg.mmc_bank_controls[0].0 = 0;
        self.io_reg.mmc_bank_controls[1].0 = 1;
        self.io_reg.mmc_bank_controls[2].0 = 2;
        self.io_reg.mmc_bank_controls[3].0 = 3;
    }

    fn try_read_u8(&self, addr: u24) -> Option<u8> {
        match addr.bank() {
            0x00..=0x3F | 0x80..=0xBF => match addr.lo16() {
                0x2200..=0x23FF => todo!("read IO {addr}"),
                0x3000..=0x37FF => todo!("read I-RAM {addr}"),
                0x6000..=0x7FFF => todo!("read one mappable 8Kbyte BW-RAM block {addr}"),
                0x8000..=0xFFFF => match addr.bank() {
                    0x00..=0x1F => {
                        let area = if self.io_reg.mmc_bank_controls[0].lorom() {
                            self.io_reg.mmc_bank_controls[0].bank() as usize
                        } else {
                            0
                        };
                        Some(
                            self.rom[area * 0x10_0000
                                + (addr.bank() as usize - 0x00) * 0x8000
                                + addr.lo16() as usize],
                        )
                    }
                    0x20..=0x3F => {
                        let area = if self.io_reg.mmc_bank_controls[1].lorom() {
                            self.io_reg.mmc_bank_controls[1].bank() as usize
                        } else {
                            1
                        };
                        Some(
                            self.rom[area * 0x10_0000
                                + (addr.bank() as usize - 0x20) * 0x8000
                                + addr.lo16() as usize],
                        )
                    }
                    0x80..=0x9F => {
                        let area = if self.io_reg.mmc_bank_controls[2].lorom() {
                            self.io_reg.mmc_bank_controls[2].bank() as usize
                        } else {
                            1
                        };
                        Some(
                            self.rom[area * 0x10_0000
                                + (addr.bank() as usize - 0x80) * 0x8000
                                + addr.lo16() as usize],
                        )
                    }
                    0xA0..=0xBF | _ => {
                        let area = if self.io_reg.mmc_bank_controls[3].lorom() {
                            self.io_reg.mmc_bank_controls[3].bank() as usize
                        } else {
                            1
                        };
                        Some(
                            self.rom[area * 0x10_0000
                                + (addr.bank() as usize - 0xA0) * 0x8000
                                + addr.lo16() as usize],
                        )
                    }
                },
                _ => None,
            },
            0x40..=0x4F => todo!("read entire 256Kbyte BW-RAM (mirrors in 44h-4Fh) {addr}"),
            0xC0..=0xCF => Some(
                self.rom[self.io_reg.mmc_bank_controls[0].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xC0_0000)],
            ),
            0xD0..=0xDF => Some(
                self.rom[self.io_reg.mmc_bank_controls[1].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xD0_0000)],
            ),
            0xE0..=0xEF => Some(
                self.rom[self.io_reg.mmc_bank_controls[2].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xE0_0000)],
            ),
            0xF0..=0xFF => Some(
                self.rom[self.io_reg.mmc_bank_controls[3].bank() as usize * 0x10_0000
                    + (addr.raw() - 0xF0_0000)],
            ),
            _ => None,
        }
    }

    fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
        match addr.bank() {
            0x00..=0x3F => match addr.lo16() {
                0x2200..=0x23FF => todo!("write IO {addr}"),
                0x3000..=0x37FF => todo!("write I-RAM {addr}"),
                0x6000..=0x7FFF => todo!("write one mappable 8Kbyte BW-RAM block {addr}"),
                _ => false,
            },
            0x40..=0x4F => todo!("write entire 256Kbyte BW-RAM (mirrors in 44h-4Fh) {addr}"),
            _ => false,
        }
    }
}
