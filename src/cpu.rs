pub mod registers;

use crate::{bus::Bus, cpu::registers::Registers, scheduler::*, u24::u24};

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
    // kind decides whether the bit-width depends on some flag (X or M),
    // or is unconditional
    (kind: $kind:ident, $($reg:ident),*) => {
        paste! {
            $(
            fn [<pull_ $reg>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(CPU::[<stack_pull_ $kind>](cpu.clone()));
                    cpu.borrow_mut().reg.[<set_ $reg>](data);
                    // TODO: Set flags
                }
            }
            )*
        }
    };
    () => {
        pull_instrs!(kind: m, a);
        pull_instrs!(kind: x, x, y);
        pull_instrs!(kind: u16, d);
        pull_instrs!(kind: u8, b);
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
            yield_all!(CPU::pull_x(cpu.clone()));
        }
    }

    fn step(&mut self, n_clocks: u32) {}

    fn read_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: Some clock cycles before the read, depending on region
            let data = yield_all!(cpu.borrow_mut().bus.read_u8(addr));
            cpu.borrow_mut().step(4);
            data
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

    // Calls either the _u8 or _u16 variant of stack_pull, depending on the X flag
    fn stack_pull_x<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        move || {
            if cpu.borrow().reg.p.x() {
                yield_all!(CPU::stack_pull_u8(cpu)) as u16
            } else {
                yield_all!(CPU::stack_pull_u16(cpu))
            }
        }
    }

    // Calls either the _u8 or _u16 variant of stack_pull, depending on the M flag
    fn stack_pull_m<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        move || {
            if cpu.borrow().reg.p.m() {
                yield_all!(CPU::stack_pull_u8(cpu)) as u16
            } else {
                yield_all!(CPU::stack_pull_u16(cpu))
            }
        }
    }

    pull_instrs!();
}
