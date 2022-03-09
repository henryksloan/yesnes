pub mod registers;

use crate::{bus::Bus, cpu::registers::Registers, scheduler::*};

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use paste::paste;

/// The 65816 microprocessor, the main CPU of the SNES
pub struct CPU {
    reg: Registers,
    bus: Bus,
}

macro_rules! yield_all {
    ($gen:ident) => {
        loop {
            match Pin::new(&mut $gen).resume(()) {
                GeneratorState::Yielded(yield_reason) => yield yield_reason,
                GeneratorState::Complete(out) => break out,
            }
        }
    };
}

macro_rules! pull_instrs {
    ($($reg:ident),*) => {
        paste! {
            $(
            fn [<pull_ $reg _u8>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    let mut pull_gen = CPU::stack_pull_u8(cpu.clone());
                    let data = yield_all!(pull_gen);
                    cpu.borrow_mut().reg.[<set_ $reg _lo>](data);
                }
            }
            )*
        }
    };
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
            // let mut instr = CPU::instr1(cpu.clone()); // cpu.borrow_mut().instr1();
            let mut instr = CPU::pull_a_u8(cpu.clone()); // cpu.borrow_mut().instr1();
            while let GeneratorState::Yielded(yield_reason) = Pin::new(&mut instr).resume(()) {
                yield yield_reason;
            }
        }
    }

    fn stack_pull_u8<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u8> + 'a {
        // TODO: Implement this in terms of some new read() fn
        move || {
            if false {
                yield YieldReason::SyncPPU(5);
            }
            5
        }
    }

    pull_instrs!(a, b, d, x, y);
}
