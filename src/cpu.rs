pub mod registers;

use crate::{cpu::registers::Registers, scheduler::*};

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

/// The 65816 microprocessor, the main CPU of the SNES
pub struct CPU {
    reg: Registers,
}

impl<'a> CPU {
    pub fn new() -> Self {
        Self {
            reg: Registers::new(),
        }
    }

    pub fn run(&'a mut self) -> impl DeviceGenerator + 'a {
        move || loop {
            let mut instr = self.instr1();
            while let GeneratorState::Yielded(yield_reason) = Pin::new(&mut instr).resume(()) {
                yield yield_reason;
            }
            println!("Continue");
        }
    }

    fn instr1(&'a mut self) -> impl InstructionGenerator + 'a {
        move || {
            if self.reg.a == 0 {
                yield YieldReason::SyncPPU(5);
            }
            yield YieldReason::SyncSMP(5);
        }
    }
}
