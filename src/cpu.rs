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

macro_rules! pull_instrs {
    // kind decides whether the bit-width depends on some flag (X or M),
    // or is unconditional (u8 or u16)
    (kind: $kind:ident, $($reg:ident),*) => {
        paste! {
            $(
            fn [<pull_ $reg>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(CPU::[<stack_pull_ $kind>](cpu.clone()));
                    cpu.borrow_mut().reg.[<set_ $reg>](data);
                    let n_bits = std::mem::size_of_val(&data);
                    cpu.borrow_mut().reg.p.n = ((data >> (n_bits - 1)) == 1);
                    cpu.borrow_mut().reg.p.z = (data == 0);
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

macro_rules! instr {
    ($cpu_rc: ident, $instr_f:ident) => {
        yield_all!(CPU::$instr_f($cpu_rc.clone()))
    };
    ($cpu_rc: ident, $instr_f:ident, $addr_mode_f:ident) => {
        let data = yield_all!($addr_mode_f(cpu.clone()));
        yield_all!($instr_f($cpu_rc.clone(), data))
    };
}

macro_rules! fetch {
    ($cpu_rc: ident) => {{
        let data = yield_all!(CPU::read_u8($cpu_rc.clone(), $cpu_rc.borrow().reg.pc));
        // FIXME: Fix after implementing u24
        // cpu.borrow_mut().reg.pc += 1;
        data
    }};
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
            let opcode = yield_all!(CPU::read_u8(cpu.clone(), cpu.borrow().reg.pc));
            let opcode = 0x68; // PLACEHOLDER

            match opcode {
                0x2B => instr!(cpu, pull_d),
                0x68 => instr!(cpu, pull_a),
                0x7A => instr!(cpu, pull_y),
                0xAB => instr!(cpu, pull_b),
                0xFA => instr!(cpu, pull_x),
                _ => panic!("Invalid opcode {}", opcode),
            };
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
            if cpu.borrow().reg.p.x_or_b {
                yield_all!(CPU::stack_pull_u8(cpu)) as u16
            } else {
                yield_all!(CPU::stack_pull_u16(cpu))
            }
        }
    }

    // Calls either the _u8 or _u16 variant of stack_pull, depending on the M flag
    fn stack_pull_m<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        move || {
            if cpu.borrow().reg.p.m {
                yield_all!(CPU::stack_pull_u8(cpu)) as u16
            } else {
                yield_all!(CPU::stack_pull_u16(cpu))
            }
        }
    }

    // FIXME: Issue! Whether this is 8- or 16-bit can depend on either (or neither) flag
    // OH! The macro can have extra info to dispatch it correctly
    // fn immediate<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
    //     move || {
    //         let lo = fetch!(cpu) as u16;
    //     }
    // }

    fn and<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl InstructionGenerator + 'a {
        move || {
            // TODO: Factor this out to a macro, like "dummy_yield"
            if false {
                yield YieldReason::SyncCPU(5);
            }
            let val = cpu.borrow().reg.get_a() & data;
            cpu.borrow_mut().reg.set_a(val);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            cpu.borrow_mut().reg.p.n = (val >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    pull_instrs!();
}
