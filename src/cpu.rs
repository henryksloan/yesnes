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
    bus: Rc<RefCell<Bus>>,
    ticks_run: u64,
}

macro_rules! yield_ticks {
    ($gen_expr:expr) => {{
        let ticks_to_yield = cpu.borrow().ticks_run;
        cpu.borrow_mut().ticks_run = 0;
        yield_all!($gen_expr, ticks_to_yield);
    }};
    ($cpu_rc:ident, $gen_expr:expr) => {{
        let ticks_to_yield = $cpu_rc.borrow().ticks_run;
        $cpu_rc.borrow_mut().ticks_run = 0;
        let mut gen = $gen_expr;
        loop {
            match Pin::new(&mut gen).resume(()) {
                GeneratorState::Yielded(yield_reason) => yield (yield_reason, ticks_to_yield),
                GeneratorState::Complete(out) => break out,
            }
        }
    }};
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
        yield_ticks!($cpu_rc, CPU::$instr_f($cpu_rc.clone()))
    };
    ($cpu_rc: ident, $instr_f:ident, $addr_mode_f:ident) => {{
        let data = yield_ticks!($cpu_rc, CPU::$addr_mode_f($cpu_rc.clone(), false));
        yield_ticks!($cpu_rc, CPU::$instr_f($cpu_rc.clone(), data))
    }};
    // If the instruction depends on the X or M flag,
    // we pass true if the 16-bit variant should be used, else false
    ($cpu_rc: ident, $instr_f:ident, flag: $flag: ident, $addr_mode_f:ident) => {{
        let data = yield_ticks!(
            $cpu_rc,
            CPU::$addr_mode_f($cpu_rc.clone(), !$cpu_rc.borrow().reg.p.$flag)
        );
        yield_ticks!($cpu_rc, CPU::$instr_f($cpu_rc.clone(), data))
    }};
    ($cpu_rc: ident, $instr_f:ident, XFlag, $addr_mode_f:ident) => {
        instr!($cpu_rc, $instr_f, flag: x_or_b, $addr_mode_f)
    };
    ($cpu_rc: ident, $instr_f:ident, MFlag, $addr_mode_f:ident) => {
        instr!($cpu_rc, $instr_f, flag: m, $addr_mode_f)
    };
}

macro_rules! fetch {
    ($cpu_rc: ident) => {{
        let data = yield_all!(CPU::read_u8($cpu_rc.clone(), $cpu_rc.borrow().reg.pc));
        $cpu_rc.borrow_mut().reg.pc += 1u32;
        data
    }};
}

impl CPU {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self {
            reg: Registers::new(),
            bus,
            ticks_run: 0,
        }
    }

    pub fn reset(&mut self) {
        self.ticks_run = 0;
        self.reg.pc = {
            let lo = u24({
                let mut gen = Bus::read_u8(self.bus.clone(), u24(0xFFFC));
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u32,
                        _ => {}
                    }
                }
            });
            let hi = u24({
                let mut gen = Bus::read_u8(self.bus.clone(), u24(0xFFFD));
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u32,
                        _ => {}
                    }
                }
            });
            (hi << 8) | lo
        };
    }

    pub fn run<'a>(cpu: Rc<RefCell<CPU>>) -> impl DeviceGenerator + 'a {
        move || loop {
            print!("CPU {:#04X}", cpu.borrow().reg.pc.raw());
            let opcode = yield_ticks!(cpu, CPU::read_u8(cpu.clone(), cpu.borrow().reg.pc));
            cpu.borrow_mut().reg.pc += 1u32;
            println!(": {:#02X}", opcode);

            // TODO: Make a new macro that generates this, taking a list like
            // (and, MFlag, 0x29:immediate, 0x2D:absolute, ...) etc.
            match opcode {
                0x18 => instr!(cpu, clc),
                0x58 => instr!(cpu, cli),
                0xD8 => instr!(cpu, cld),
                0xB8 => instr!(cpu, clv),
                0x38 => instr!(cpu, sec),
                0x78 => instr!(cpu, sei),
                0xF8 => instr!(cpu, sed),
                0xC2 => instr!(cpu, rep, immediate),
                0xFB => instr!(cpu, xce),

                0x21 => instr!(cpu, and, MFlag, indexed_indirect),
                0x25 => instr!(cpu, and, MFlag, direct),
                0x29 => instr!(cpu, and, MFlag, immediate),
                0x2D => instr!(cpu, and, MFlag, absolute),
                0x31 => instr!(cpu, and, MFlag, indirect_indexed),
                0x32 => instr!(cpu, and, MFlag, indirect),
                0x35 => instr!(cpu, and, MFlag, direct_x),
                0x39 => instr!(cpu, and, MFlag, absolute_y),
                0x3D => instr!(cpu, and, MFlag, absolute_x),

                0xA1 => instr!(cpu, lda, MFlag, indexed_indirect),
                // 0xA3 => instr!(cpu, lda, MFlag, stack_relative),
                0xA5 => instr!(cpu, lda, MFlag, direct),
                // 0xA7 => instr!(cpu, lda, MFlag, direct_indirect_long),
                0xA9 => instr!(cpu, lda, MFlag, immediate),
                0xAD => instr!(cpu, lda, MFlag, absolute),
                // 0xAF => instr!(cpu, lda, MFlag, absolute_long),
                0xB1 => instr!(cpu, lda, MFlag, indirect_indexed),
                0xB2 => instr!(cpu, lda, MFlag, indirect),
                // 0xB3 => instr!(cpu, lda, MFlag, stack_relative_indirect_indexed),
                0xB5 => instr!(cpu, lda, MFlag, direct_x),
                // 0xB7 => instr!(cpu, lda, MFlag, direct_indirect_long_y),
                0xB9 => instr!(cpu, lda, MFlag, absolute_y),
                0xBD => instr!(cpu, lda, MFlag, absolute_x),
                // 0xBF => instr!(cpu, lda, MFlag, absolute_long_x),
                //
                0xA2 => instr!(cpu, ldx, MFlag, indexed_indirect),
                0xA6 => instr!(cpu, ldx, MFlag, direct),
                0xAE => instr!(cpu, ldx, MFlag, absolute),
                0xB6 => instr!(cpu, ldx, MFlag, direct_y),
                0xBE => instr!(cpu, ldx, MFlag, absolute_y),

                0xA0 => instr!(cpu, ldy, MFlag, indexed_indirect),
                0xA4 => instr!(cpu, ldy, MFlag, direct),
                0xAC => instr!(cpu, ldy, MFlag, absolute),
                0xB4 => instr!(cpu, ldy, MFlag, direct_x),
                0xBC => instr!(cpu, ldy, MFlag, absolute_x),

                0x2B => instr!(cpu, pull_d),
                0x68 => instr!(cpu, pull_a),
                0x7A => instr!(cpu, pull_y),
                0xAB => instr!(cpu, pull_b),
                0xFA => instr!(cpu, pull_x),
                _ => panic!("Invalid opcode {:#02X}", opcode),
            };
        }
    }

    fn step(&mut self, n_clocks: u64) {
        self.ticks_run += n_clocks;
    }

    fn read_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: Some clock cycles before the read, depending on region
            // let bus = &mut cpu.borrow_mut().bus;
            // let data = yield_all!(bus.read_u8(addr));
            let data = yield_all!(Bus::read_u8(cpu.borrow_mut().bus.clone(), addr));
            // TODO: This is like yield_all, but avoids holding the borrow across a yield; maybe
            // factor out to a macro or somehow generalize yield_all
            // let data = loop {
            //     // TODO: Oh wait, this is totally wrong, because it re-creates the generator every
            //     // time :(
            //     let result = Pin::new(&mut cpu.borrow_mut().bus.read_u8(addr)).resume(());
            //     match result {
            //         GeneratorState::Yielded(yield_reason) => yield yield_reason,
            //         GeneratorState::Complete(out) => break out,
            //     }
            // };
            cpu.borrow_mut().step(4);
            data
        }
    }

    fn read_direct_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            let addr = if cpu.borrow().reg.p.e && (cpu.borrow().reg.d & 0xFF == 0) {
                u24(cpu.borrow().reg.d.into()) | (addr & 0xFFu8)
            } else {
                (u24(cpu.borrow().reg.d.into()) + addr) & 0xFFFFu32
            };
            yield_all!(CPU::read_u8(cpu.clone(), addr))
        }
    }

    fn read_bank_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            let addr = u24((cpu.borrow().reg.b as u32) << 16) | addr;
            yield_all!(CPU::read_u8(cpu.clone(), addr))
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

    fn immediate<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let mut data = fetch!(cpu) as u16;
            if long {
                data |= (fetch!(cpu) as u16) << 8;
            }
            data
        }
    }

    // TODO: Reduce code duplication across these three
    fn direct<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let addr = u24(fetch!(cpu) as u32);
            let mut data = yield_all!(CPU::read_direct_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_direct_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn direct_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.x as u32);
            let mut data = yield_all!(CPU::read_direct_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_direct_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn direct_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.y as u32);
            let mut data = yield_all!(CPU::read_direct_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_direct_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    // TODO: Reduce code duplication across these three
    fn absolute<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let addr = u24(fetch!(cpu) as u32 | fetch!(cpu) as u32);
            let mut data = yield_all!(CPU::read_bank_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_bank_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn absolute_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let addr = u24((fetch!(cpu) as u32 | fetch!(cpu) as u32) + cpu.borrow().reg.x as u32);
            let mut data = yield_all!(CPU::read_bank_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_bank_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn absolute_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let addr = u24((fetch!(cpu) as u32 | fetch!(cpu) as u32) + cpu.borrow().reg.y as u32);
            let mut data = yield_all!(CPU::read_bank_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_bank_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    // TODO: Reduce code duplication across these three
    fn indirect<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            };
            let mut data = yield_all!(CPU::read_bank_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_bank_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn indexed_indirect<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let offset = cpu.borrow().reg.x as u32;
            let indirect_addr = u24(fetch!(cpu) as u32 + offset);
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            };
            let mut data = yield_all!(CPU::read_bank_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_bank_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn indirect_indexed<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<u16> + 'a {
        move || {
            let offset = cpu.borrow().reg.y as u32;
            let indirect_addr = u24(fetch!(cpu) as u32);
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            } + offset;
            let mut data = yield_all!(CPU::read_bank_u8(cpu.clone(), addr)) as u16;
            if long {
                data |= (yield_all!(CPU::read_bank_u8(cpu.clone(), addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn and<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let val = cpu.borrow().reg.get_a() & data;
            cpu.borrow_mut().reg.set_a(val);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            cpu.borrow_mut().reg.p.n = (val >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    // TODO: Make a macro for these flag instructions?
    fn clc<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.c = false;
        }
    }

    fn cli<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.i = false;
        }
    }

    fn cld<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.d = false;
        }
    }

    fn clv<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.v = false;
        }
    }

    fn sec<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.c = true;
        }
    }

    fn sei<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.i = true;
        }
    }

    fn sed<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.p.d = true;
        }
    }

    fn rep<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let p = &mut cpu.borrow_mut().reg.p;
            p.set(p.get() & !data as u8);
        }
    }

    fn xce<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let p = &mut cpu.borrow_mut().reg.p;
            std::mem::swap(&mut p.c, &mut p.e);
        }
    }

    // TODO: Factor out load instructions (macro?)
    fn lda<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.set_a(data);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn ldx<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.set_x(data);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn ldy<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.set_y(data);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    pull_instrs!();
}
