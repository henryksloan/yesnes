pub mod registers;

use crate::{bus::Bus, cpu::registers::Registers, scheduler::*};

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

/// The 65816 microprocessor, the main CPU of the SNES
pub struct CPU {
    reg: Registers,
    bus: Bus,
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        Self {
            reg: Registers::new(),
            bus,
        }
    }

    pub fn run<'a>(cpu: Rc<RefCell<CPU>>) -> impl DeviceGenerator + 'a {
        move || loop {
            println!("CPU");
            let mut instr = CPU::instr1(cpu.clone()); // cpu.borrow_mut().instr1();
            while let GeneratorState::Yielded(yield_reason) = Pin::new(&mut instr).resume(()) {
                yield yield_reason;
            }
        }
    }

    fn instr1<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            if cpu.borrow().reg.a == 0 {
                yield YieldReason::SyncPPU(5);
            }
            yield YieldReason::SyncSMP(5);
        }
    }
}
