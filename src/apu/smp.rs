pub mod registers;

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

pub use registers::{Registers, StatusRegister};

use crate::scheduler::*;
use crate::u24::u24;

pub const RESET_VECTOR: u24 = u24(0xFFFE);

/// The SMP, i.e. the smp700 audio coprocessor
pub struct SMP {
    reg: Registers,
    ticks_run: u64,
}

impl SMP {
    pub fn new() -> Self {
        Self {
            reg: Registers::new(),
            ticks_run: 0,
        }
    }

    pub fn registers(&self) -> &Registers {
        &self.reg
    }

    pub fn registers_mut(&mut self) -> &mut Registers {
        &mut self.reg
    }

    pub fn reset(smp: Rc<RefCell<SMP>>) {
        smp.borrow_mut().ticks_run = 0;
        // TODO: Simplify
        smp.borrow_mut().reg.pc = {
            let lo = {
                let mut gen = Self::read_u8(smp.clone(), RESET_VECTOR);
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u16,
                        _ => {}
                    }
                }
            };
            let hi = {
                let mut gen = Self::read_u8(smp.clone(), RESET_VECTOR + 1u32);
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u16,
                        _ => {}
                    }
                }
            };
            (hi << 8) | lo
        };
        // self.reg.set_psw(0x34);
        // self.reg.set_sp(0x1FF);
    }

    pub fn run<'a>(smp: Rc<RefCell<SMP>>) -> impl DeviceGenerator + 'a {
        move || loop {
            println!("SMP");
            yield (YieldReason::Sync(Device::CPU), 5);
        }
    }

    fn step(&mut self, n_clocks: u64) {
        self.ticks_run += n_clocks;
    }

    fn read_u8<'a>(smp: Rc<RefCell<SMP>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            dummy_yield!();
            // TODO: Some clock cycles before the read, depending on region
            smp.borrow_mut().step(1);
            0
        }
    }

    fn write_u8<'a>(smp: Rc<RefCell<SMP>>, addr: u24, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            dummy_yield!();
            // TODO: Some clock cycles before the write, depending on region
            smp.borrow_mut().step(4);
        }
    }
}
