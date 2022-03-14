pub mod registers;

use crate::{
    bus::Bus,
    cpu::registers::{u24, Registers},
    scheduler::*,
};

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
    ($gen_expr:expr) => {{
        let mut gen = $gen_expr;
        loop {
            match Pin::new(&mut gen).resume(()) {
                GeneratorState::Yielded(yield_reason) => yield yield_reason,
                GeneratorState::Complete(out) => break out,
            }
        }
    }};
}

macro_rules! pull_instrs {
    (u8, $($reg:ident),*) => {
        paste! {
            $(
            fn [<pull_ $reg _u8>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(CPU::stack_pull_u8(cpu.clone()));
                    cpu.borrow_mut().reg.[<set_ $reg _lo>](data);
                }
            }
            )*
        }
    };
    (u16, $($reg:ident),*) => {
        paste! {
            $(
            fn [<pull_ $reg _u16>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(CPU::stack_pull_u16(cpu.clone()));
                    cpu.borrow_mut().reg.$reg = data;
                }
            }
            )*
        }
    };
    ($($reg:ident),*) => {
        pull_instrs!(u8, $($reg),*);
        pull_instrs!(u16, $($reg),*);
    }
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
            let opcode = yield_all!(CPU::read_u8(cpu.clone(), cpu.borrow().reg.pc));
            println!("CPU");
            yield_all!(CPU::pull_a_u8(cpu.clone()));
        }
    }

    fn read_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            if false {
                yield YieldReason::SyncPPU(5);
            }
            5
        }
    }

    fn stack_pull_u8<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u8> + 'a {
        // TODO
        move || yield_all!(CPU::read_u8(cpu.clone(), u24(0x100)))
    }

    fn stack_pull_u16<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        // TODO
        move || {
            let hi = yield_all!(CPU::read_u8(cpu.clone(), u24(0x101)));
            let lo = yield_all!(CPU::read_u8(cpu.clone(), u24(0x100)));
            ((hi as u16) << 8) | lo as u16
        }
    }

    pull_instrs!(a, d, x, y);
    pull_instrs!(u8, b);
}
