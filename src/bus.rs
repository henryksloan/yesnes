use crate::apu::SMP;
use crate::ppu::PPU;
use crate::scheduler::{
    dummy_yield, Access, AccessType, DebugPoint, Device, YieldReason, Yieldable,
};
use crate::u24::u24;

use std::cell::RefCell;
use std::fs;
use std::ops::GeneratorState;
use std::pin::Pin;
use std::rc::Rc;

pub struct Bus {
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,
    cart_test: Vec<u8>,
    wram: Vec<u8>,
}

impl Bus {
    pub fn new(ppu: Rc<RefCell<PPU>>, smp: Rc<RefCell<SMP>>) -> Self {
        Self {
            ppu,
            smp,
            cart_test: fs::read(
                &std::env::args()
                    .collect::<Vec<String>>()
                    .get(1)
                    .expect("Expected a rom file"),
            )
            .unwrap(),
            wram: vec![0; 0x20000],
        }
    }

    pub fn reset(&mut self) {
        self.wram.fill(0);
    }

    // TODO: I have FORGOTTEN why these don't take &self. Look into why.
    // edit: I think it's because we're not allowed to hold references across yields.
    pub fn peak_u8(bus: Rc<RefCell<Bus>>, addr: u24) -> u8 {
        // TODO: Some generalized mapper logic
        match addr.hi8() {
            0x00..=0x3F | 0x80..=0xBF => {
                if addr.hi8() == 0x00 && (0xFF00..=0xFFFF).contains(&addr.lo16()) {
                    bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
                } else {
                    match addr.lo16() {
                        // TODO: System area
                        0x0000..=0x1FFF => bus.borrow().wram[addr.lo16() as usize],
                        0x2140..=0x217F => {
                            let port = (addr.lo16() - 0x2140) % 4;
                            bus.borrow().smp.borrow_mut().io_peak(0x2140 + port)
                        }
                        0x8000.. => {
                            bus.borrow().cart_test[((addr.hi8() as usize & !0x80) * 0x8000)
                                | (addr.lo16() as usize - 0x8000)]
                        }
                        _ => 0,
                    }
                }
            }
            0x7E..=0x7F => {
                bus.borrow().wram[0x10000 * (addr.hi8() as usize - 0x7E) + addr.lo16() as usize]
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
        move || {
            dummy_yield!();
            // TODO: Some generalized mapper logic
            // TODO: HiROM
            match addr.hi8() {
                0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                    bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
                }
                0x00..=0x3F | 0x80..=0xBF => {
                    match addr.lo16() {
                        // TODO: System area
                        0x0000..=0x1FFF => bus.borrow().wram[addr.lo16() as usize],
                        0x2140..=0x217F => {
                            yield YieldReason::Sync(Device::SMP);
                            let port = (addr.lo16() - 0x2140) % 4;
                            bus.borrow().smp.borrow_mut().io_read(0x2140 + port)
                        }
                        0x8000.. => {
                            bus.borrow().cart_test[((addr.hi8() as usize & !0x80) * 0x8000)
                                | (addr.lo16() as usize - 0x8000)]
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
                0x7E..=0x7F => {
                    bus.borrow().wram[0x10000 * (addr.hi8() as usize - 0x7E) + addr.lo16() as usize]
                }
                _ => 0,
            }
        }
    }

    pub fn read_u16<'a>(bus: Rc<RefCell<Bus>>, addr: u24) -> impl Yieldable<u16> + 'a {
        move || {
            let lo = {
                let mut gen = Bus::read_u8(bus.clone(), addr);
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u16,
                        GeneratorState::Yielded(yielded) => yield yielded,
                    }
                }
            };
            let hi = {
                let mut gen = Bus::read_u8(bus.clone(), addr + 1u32);
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u16,
                        GeneratorState::Yielded(yielded) => yield yielded,
                    }
                }
            };
            (hi << 8) | lo
        }
    }

    pub fn write_u8<'a>(bus: Rc<RefCell<Bus>>, addr: u24, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            dummy_yield!();
            // TODO: Some generalized mapper logic
            // TODO: HiROM
            match addr.hi8() {
                0x00 if (0xFF00..=0xFFFF).contains(&addr.lo16()) => {
                    // bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
                }
                0x00..=0x3F | 0x80..=0xBF => {
                    match addr.lo16() {
                        // TODO: System area
                        0x0000..=0x1FFF => bus.borrow_mut().wram[addr.lo16() as usize] = data,
                        0x2140..=0x217F => {
                            yield YieldReason::Sync(Device::SMP);
                            let port = (addr.lo16() - 0x2140) % 4;
                            bus.borrow().smp.borrow_mut().io_write(0x2140 + port, data);
                        }
                        _ => {
                            yield YieldReason::Debug(DebugPoint::UnimplementedAccess(Access {
                                access_type: AccessType::Write,
                                addr,
                            }));
                        }
                    }
                }
                0x7E..=0x7F => {
                    bus.borrow_mut().wram
                        [0x10000 * (addr.hi8() as usize - 0x7E) + addr.lo16() as usize] = data
                }
                _ => {}
            }
        }
    }
}
