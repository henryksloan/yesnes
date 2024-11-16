use crate::cartridge::Mapper;
use crate::u24::u24;

pub struct SA1 {}

impl Mapper for SA1 {
    fn try_read_u8(&self, addr: u24) -> Option<u8> {
        match addr.bank() {
            0x00..=0x3F => match addr.lo16() {
                0x2200..=0x23FF => todo!("read IO {addr}"),
                0x3000..=0x37FF => todo!("read I-RAM {addr}"),
                0x6000..=0x7FFF => todo!("read one mappable 8Kbyte BW-RAM block {addr}"),
                0x8000..=0xFFFF => {
                    // DO NOT SUBMIT: Hmmm, how to reroute/rewrite ROM accesses? Ah! Maybe pass a reference to the mapper... or make coprocessor own the mapper?
                    // Actually, is SA-1 actually orthogonal to LoROM/HiROM? Is it just a mapper in itself?
                    todo!("read four mappable 1MByte LoROM blocks (max 8Mbyte) {addr}")
                }
                _ => None,
            },
            0x40..=0x4F => todo!("read entire 256Kbyte BW-RAM (mirrors in 44h-4Fh) {addr}"),
            0xC0..=0xFF => todo!("read four mappable 1MByte HiROM blocks (max 8Mbyte) {addr}"),
            _ => None,
        }
    }

    fn try_write_u8(&mut self, addr: u24, data: u8) -> bool {
        match addr.bank() {
            0x00..=0x3F => match addr.lo16() {
                0x2200..=0x23FF => todo!("write IO {addr}"),
                0x3000..=0x37FF => todo!("write I-RAM {addr}"),
                0x6000..=0x7FFF => todo!("write one mappable 8Kbyte BW-RAM block {addr}"),
                0x8000..=0xFFFF => {
                    todo!("write four mappable 1MByte LoROM blocks (max 8Mbyte) {addr}")
                }
                _ => false,
            },
            0x40..=0x4F => todo!("write entire 256Kbyte BW-RAM (mirrors in 44h-4Fh) {addr}"),
            0xC0..=0xFF => todo!("write four mappable 1MByte HiROM blocks (max 8Mbyte) {addr}"),
            _ => false,
        }
    }
}
