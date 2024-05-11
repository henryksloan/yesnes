use crate::apu::SMP;
use crate::cpu::CPU;
use crate::ppu::PPU;
use crate::scheduler::*;
use crate::u24::u24;

use std::cell::RefCell;
use std::fs;
use std::ops::CoroutineState;
use std::pin::Pin;
use std::rc::{Rc, Weak};

pub struct Bus {
    cpu: Weak<RefCell<CPU>>,
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,
    // TODO: Probably want Box<[u8; 0x20000]>, but need to somehow avoid allocating on stack first
    wram: Vec<u8>,
    wram_port_addr: u24,
    // TODO: These should eventually be encapsulated,
    // which should include only allocating according to the ROM header
    cart_test: Vec<u8>,
    sram: Vec<u8>,
    // TODO: These arithmetic ops take place in the CPU (ALU) and take cycle (mult=8, div=16)
    // TODO: Refactor these CPU IOs to a new struct or something
    // 4202h - WRMPYA for IO multiplication
    multiplicand_a: u8,
    // 4204h - WRDIVL and 4205h - WRDIVH; unsigned dividend
    dividend: u16,
    // 4214h - RDDIVL and 4215h - RDDIVH; unsigned division result
    quotient: u16,
    // 4216h - RDMPYL and 4217h - RDMPYH; result of IO multiplication or division
    product_or_remainder: u16,
}

impl Bus {
    pub fn new(ppu: Rc<RefCell<PPU>>, smp: Rc<RefCell<SMP>>) -> Self {
        Self {
            cpu: Weak::new(),
            ppu,
            smp,
            wram: vec![0; 0x20000],
            wram_port_addr: u24(0),
            cart_test: Vec::new(),
            sram: vec![0; 0x80000],
            multiplicand_a: 0,
            dividend: 0,
            quotient: 0,
            product_or_remainder: 0,
        }
    }

    pub fn connect_cpu(&mut self, cpu: Weak<RefCell<CPU>>) {
        self.cpu = cpu;
    }

    pub fn reset(&mut self) {
        self.wram.fill(0);
        self.multiplicand_a = 0;
        self.product_or_remainder = 0;
    }

    pub fn load_cart(&mut self, cart_path: &str) {
        self.cart_test = fs::read(cart_path).unwrap();
    }

    // TODO: I have FORGOTTEN why these don't take &self. Look into why.
    // edit: Because we can't have multiple mutable references. Alternative is wrapping all internal data in RefCells.
    // TODO: It would probably be very beneficial to start embracing &Rc<RefCell<T>> to cut down on clones
    pub fn peak_u8(bus: Rc<RefCell<Bus>>, addr: u24) -> u8 {
        // TODO: Some generalized mapper logic
        // TODO: HiROM: https://snes.nesdev.org/wiki/Memory_map
        match addr.bank() {
            0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
            }
            0x00..=0x7D | 0x80..=0xFF if (0x8000..=0xFFFF).contains(&addr.lo16()) => {
                let len = bus.borrow().cart_test.len();
                bus.borrow().cart_test[(((addr.bank() as usize & !0x80) * 0x8000)
                    | (addr.lo16() as usize - 0x8000))
                    % len]
            }
            0x00..=0x3F | 0x80..=0xBF => {
                match addr.lo16() {
                    // TODO: System area
                    0x0000..=0x1FFF => bus.borrow().wram[addr.lo16() as usize],
                    0x2100..=0x213F => bus.borrow().ppu.borrow().io_peak(addr.lo16()),
                    0x2140..=0x217F => {
                        let port = (addr.lo16() - 0x2140) % 4;
                        bus.borrow().smp.borrow().io_peak(0x2140 + port)
                    }
                    0x2180 => {
                        let wram_port_addr = bus.borrow().wram_port_addr;
                        bus.borrow().wram[wram_port_addr.0 as usize]
                    }
                    0x4214 => bus.borrow().quotient as u8,
                    0x4215 => (bus.borrow().quotient >> 8) as u8,
                    0x4216 => bus.borrow().product_or_remainder as u8,
                    0x4217 => (bus.borrow().product_or_remainder >> 8) as u8,
                    0x4200..=0x421F | 0x4300..=0x437F => bus
                        .borrow_mut()
                        .cpu
                        .upgrade()
                        .unwrap()
                        .borrow_mut()
                        .io_peak(addr),
                    _ => 0,
                }
            }
            0x70..=0x7D | 0xF0..=0xFF => {
                bus.borrow().sram
                    [(((addr.bank() as usize & !0x80) - 0x70) * 0x8000) | addr.lo16() as usize]
            }
            0x7E..=0x7F => {
                bus.borrow().wram[0x10000 * (addr.bank() as usize - 0x7E) + addr.lo16() as usize]
            }
            _ => 0,
        }
    }

    pub fn peak_u16(bus: Rc<RefCell<Bus>>, addr: u24) -> u16 {
        let lo = Bus::peak_u8(bus.clone(), addr) as u16;
        let hi = Bus::peak_u8(bus.clone(), addr + 1u32) as u16;
        (hi << 8) | lo
    }

    pub fn read_u8<'a>(bus: Rc<RefCell<Bus>>, addr: u24) -> impl Yieldable<u8> + 'a {
        #[coroutine]
        move || {
            // TODO: Some generalized mapper logic
            // TODO: HiROM: https://snes.nesdev.org/wiki/Memory_map
            match addr.bank() {
                0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                    bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
                }
                0x00..=0x7D | 0x80..=0xFF if (0x8000..=0xFFFF).contains(&addr.lo16()) => {
                    let len = bus.borrow().cart_test.len();
                    bus.borrow().cart_test[(((addr.bank() as usize & !0x80) * 0x8000)
                        | (addr.lo16() as usize - 0x8000))
                        % len]
                }
                0x00..=0x3F | 0x80..=0xBF => {
                    match addr.lo16() {
                        // TODO: System area
                        0x0000..=0x1FFF => bus.borrow().wram[addr.lo16() as usize],
                        0x2100..=0x213F => {
                            // TODO: Uncomment once PPU actually does stuff
                            // yield YieldReason::Sync(Device::PPU);
                            bus.borrow().ppu.borrow_mut().io_read(addr.lo16())
                        }
                        0x2140..=0x217F => {
                            yield YieldReason::Sync(Device::SMP);
                            let port = (addr.lo16() - 0x2140) % 4;
                            bus.borrow().smp.borrow_mut().io_read(0x2140 + port)
                        }
                        0x2180 => {
                            let wram_port_addr = bus.borrow().wram_port_addr;
                            bus.borrow().wram[wram_port_addr.0 as usize]
                        }
                        0x4016 => {
                            log::debug!("TODO: Read {addr} Joypad Input Register A (R)");
                            0
                        }
                        0x4017 => {
                            log::debug!("TODO: Read {addr} Joypad Input Register B (R)");
                            0
                        }
                        // TODO: Move these math registers to the CPU
                        0x4214 => bus.borrow_mut().quotient as u8,
                        0x4215 => (bus.borrow_mut().quotient >> 8) as u8,
                        0x4216 => bus.borrow_mut().product_or_remainder as u8,
                        0x4217 => (bus.borrow_mut().product_or_remainder >> 8) as u8,
                        0x4200..=0x421F | 0x4300..=0x437F => bus
                            .borrow_mut()
                            .cpu
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .io_read(addr),
                        _ => {
                            yield YieldReason::Debug(DebugPoint::UnimplementedAccess(Access {
                                access_type: AccessType::Read,
                                addr,
                            }));
                            0
                        }
                    }
                }
                0x70..=0x7D | 0xF0..=0xFF => {
                    bus.borrow().sram
                        [(((addr.bank() as usize & !0x80) - 0x70) * 0x8000) | addr.lo16() as usize]
                }
                0x7E..=0x7F => {
                    bus.borrow().wram
                        [0x10000 * (addr.bank() as usize - 0x7E) + addr.lo16() as usize]
                }
                _ => {
                    yield YieldReason::Debug(DebugPoint::UnimplementedAccess(Access {
                        access_type: AccessType::Read,
                        addr,
                    }));
                    0
                }
            }
        }
    }

    pub fn read_u16<'a>(bus: Rc<RefCell<Bus>>, addr: u24) -> impl Yieldable<u16> + 'a {
        #[coroutine]
        move || {
            let lo = yield_all!(Bus::read_u8(bus.clone(), addr)) as u16;
            let hi = yield_all!(Bus::read_u8(bus.clone(), addr + 1u32)) as u16;
            (hi << 8) | lo
        }
    }

    pub fn write_u8<'a>(bus: Rc<RefCell<Bus>>, addr: u24, data: u8) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            // TODO: Some generalized mapper logic
            // TODO: HiROM: https://snes.nesdev.org/wiki/Memory_map
            match addr.bank() {
                0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                    // bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
                }
                0x00..=0x3F | 0x80..=0xBF => {
                    match addr.lo16() {
                        // TODO: System area
                        0x0000..=0x1FFF => bus.borrow_mut().wram[addr.lo16() as usize] = data,
                        0x2100..=0x213F => {
                            // TODO: Uncomment once PPU actually does stuff
                            // yield YieldReason::Sync(Device::PPU);
                            bus.borrow().ppu.borrow_mut().io_write(addr.lo16(), data);
                        }
                        0x2140..=0x217F => {
                            yield YieldReason::Sync(Device::SMP);
                            let port = (addr.lo16() - 0x2140) % 4;
                            bus.borrow().smp.borrow_mut().io_write(0x2140 + port, data);
                        }
                        0x2180 => {
                            let wram_port_addr = bus.borrow().wram_port_addr;
                            bus.borrow_mut().wram[wram_port_addr.0 as usize] = data;
                        }
                        0x2181 => bus.borrow_mut().wram_port_addr.set_lo_byte(data),
                        0x2182 => bus.borrow_mut().wram_port_addr.set_hi_byte(data),
                        0x2183 => bus.borrow_mut().wram_port_addr.set_bank(data & 1),
                        0x4016 => {
                            log::debug!("TODO: Write Joypad Output (W): {data:02X}");
                        }
                        // TODO: Move these math registers to the CPU
                        0x4202 => {
                            bus.borrow_mut().multiplicand_a = data;
                        }
                        0x4203 => {
                            let multiplicand_a = bus.borrow_mut().multiplicand_a as u16;
                            bus.borrow_mut().product_or_remainder = multiplicand_a * data as u16;
                        }
                        0x4204 => {
                            bus.borrow_mut().dividend &= 0xFF00;
                            bus.borrow_mut().dividend |= data as u16;
                        }
                        0x4205 => {
                            bus.borrow_mut().dividend &= 0x00FF;
                            bus.borrow_mut().dividend |= (data as u16) << 8;
                        }
                        0x4206 => {
                            let dividend = bus.borrow().dividend;
                            let divisor = data as u16;
                            if divisor == 0 {
                                bus.borrow_mut().quotient = 0xFFFF;
                                bus.borrow_mut().product_or_remainder = dividend;
                            } else {
                                bus.borrow_mut().quotient = dividend / divisor;
                                bus.borrow_mut().product_or_remainder = dividend % divisor;
                            }
                        }
                        0x4200..=0x421F | 0x4300..=0x437F => {
                            bus.borrow_mut()
                                .cpu
                                .upgrade()
                                .unwrap()
                                .borrow_mut()
                                .io_write(addr, data);
                        }
                        _ => {
                            yield YieldReason::Debug(DebugPoint::UnimplementedAccess(Access {
                                access_type: AccessType::Write,
                                addr,
                            }));
                        }
                    }
                }
                0x70..=0x7D | 0xF0..=0xFF if (0x0000..=0x7FFF).contains(&addr.lo16()) => {
                    bus.borrow_mut().sram[(((addr.bank() as usize & !0x80) - 0x70) * 0x8000)
                        | addr.lo16() as usize] = data
                }
                0x7E..=0x7F => {
                    bus.borrow_mut().wram
                        [0x10000 * (addr.bank() as usize - 0x7E) + addr.lo16() as usize] = data
                }
                _ => {
                    yield YieldReason::Debug(DebugPoint::UnimplementedAccess(Access {
                        access_type: AccessType::Write,
                        addr,
                    }));
                }
            }
        }
    }

    // TODO: Remove debug function
    pub fn debug_get_frame(&self) -> [[[u8; 3]; 256]; 224] {
        self.ppu.borrow().debug_get_frame()
    }
}
