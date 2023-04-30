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

macro_rules! n_bits {
    ($cpu_rc:ident, u8) => {
        8
    };
    ($cpu_rc:ident, u16) => {
        16
    };
    ($cpu_rc:ident, m) => {
        if $cpu_rc.borrow().reg.p.m || $cpu_rc.borrow().reg.p.e {
            8
        } else {
            16
        }
    };
    ($cpu_rc:ident, x) => {
        if $cpu_rc.borrow().reg.p.x_or_b || $cpu_rc.borrow().reg.p.e {
            8
        } else {
            16
        }
    };
    ($cpu_rc:ident, e) => {
        if $cpu_rc.borrow().reg.p.e {
            8
        } else {
            16
        }
    };
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
                    cpu.borrow_mut().reg.p.n = ((data >> (n_bits!(cpu, $kind) - 1)) == 1);
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

macro_rules! transfer_instrs {
    (kind: $kind:ident, $from:ident => sp) => {
        paste! {
            fn [<transfer_ $from _sp>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    let data = cpu.borrow().reg.[<get_ $from>]();
                    cpu.borrow_mut().reg.set_sp(data);
                }
            }
        }
    };
    (kind: $kind:ident, $from:ident => $to:ident) => {
        paste! {
            fn [<transfer_ $from _ $to>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    let data = cpu.borrow().reg.[<get_ $from>]();
                    cpu.borrow_mut().reg.[<set_ $to>](data);
                    cpu.borrow_mut().reg.p.n = ((data >> (n_bits!(cpu, $kind) - 1)) == 1);
                    cpu.borrow_mut().reg.p.z = (data == 0);
                }
            }
        }
    };
    () => {
        transfer_instrs!(kind: x, a => y);
        transfer_instrs!(kind: x, a => x);
        transfer_instrs!(kind: x, sp => x);
        transfer_instrs!(kind: m, y => a);
        transfer_instrs!(kind: m, x => a);
        transfer_instrs!(kind: e, x => sp);
        transfer_instrs!(kind: x, x => y);
        transfer_instrs!(kind: x, y => x);
        transfer_instrs!(kind: u16, d => a);
        transfer_instrs!(kind: u16, a => d);
        transfer_instrs!(kind: u16, sp => a);
        transfer_instrs!(kind: e, a => sp);
    }
}

macro_rules! branch_instrs {
    ($flag:ident => $val:expr, $set_clear:ident) => {
        paste! {
            fn [<branch_ $flag _ $set_clear>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
                move || {
                    let source_pc = cpu.borrow().reg.pc + 1u16;
                    let dest_pc = source_pc + u24(fetch!(cpu) as i8 as i32 as u32);
                    if cpu.borrow_mut().reg.p.$flag == $val {
                        cpu.borrow_mut().reg.pc = dest_pc;
                        cpu.borrow_mut().step(1);
                        // TODO: Is this right? Maybe only for emulation mode...
                        if (source_pc & 0x100u16).raw() != (dest_pc & 0x100u16).raw() {
                            cpu.borrow_mut().step(1);
                        }
                    }
                }
            }
        }
    };
    ($($flag:ident),+) => {
        $(
        branch_instrs!($flag => true, set);
        branch_instrs!($flag => false, clear);
        )+
    };
    () => {
        branch_instrs!(n, v, c, z);
    };
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
    ($cpu_rc: ident, $instr_f:ident, NoFlag, implied) => {
        instr!($cpu_rc, $instr_f)
    };
    ($cpu_rc: ident, $instr_f:ident, NoFlag, $addr_mode_f:ident) => {
        instr!($cpu_rc, $instr_f, $addr_mode_f)
    };
}

macro_rules! instrs {
    ($cpu_rc:ident, $opcode_expr:expr, $(($instr_f:ident , $flag:tt; $($opcode:expr => $addr_mode_f:tt),+))+) => {
        let opcode_val = $opcode_expr;
        match opcode_val {
            $(
                $($opcode => instr!($cpu_rc, $instr_f, $flag, $addr_mode_f),)+
            )+
            _ => panic!("Invalid opcode {:#02X}", opcode_val),
        }
    };
}

macro_rules! fetch {
    ($cpu_rc: ident) => {{
        let data = yield_all!(CPU::read_u8($cpu_rc.clone(), $cpu_rc.borrow().reg.pc));
        $cpu_rc.borrow_mut().reg.pc += 1u32;
        data
    }};
}

#[derive(Copy, Clone)]
struct Pointer {
    pub addr: u24,
    pub long: bool,
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

            instrs!(cpu, opcode,
                (clc, NoFlag; 0x18=>implied)
                (cli, NoFlag; 0x58=>implied)
                (cld, NoFlag; 0xD8=>implied)
                (clv, NoFlag; 0xB8=>implied)
                (sec, NoFlag; 0x38=>implied)
                (sei, NoFlag; 0x78=>implied)
                (sed, NoFlag; 0xF8=>implied)
                (rep, NoFlag; 0xC2=>immediate)
                (sep, NoFlag; 0xE2=>immediate)
                (xce, NoFlag; 0xFB=>implied)
                (branch_n_clear, NoFlag; 0x10=>implied)
                (branch_n_set, NoFlag; 0x30=>implied)
                (branch_v_clear, NoFlag; 0x50=>implied)
                (branch_v_set, NoFlag; 0x70=>implied)
                (branch_c_clear, NoFlag; 0x90=>implied)
                (branch_c_set, NoFlag; 0xB0=>implied)
                (branch_z_clear, NoFlag; 0xD0=>implied)
                (branch_z_set, NoFlag; 0xF0=>implied)
                (jsr, NoFlag; 0x20=>absolute, 0xFC=>absolute_indirect_indexed)
                (jsl, NoFlag; 0x22=>absolute_long)
                (and, MFlag; 0x21=>indexed_indirect, 0x25=>direct,
                 0x29=>immediate, 0x2D=>absolute, 0x31=>indirect_indexed,
                 0x32=>indirect, 0x35=>direct_x, 0x39=>absolute_y, 0x3D=>absolute_x)
                (lda, MFlag; 0xA1=>indexed_indirect, 0xA3=>stack_relative,
                 0xA5=>direct, 0xA7=>indirect_long, 0xA9=>immediate,
                 0xAD=>absolute, 0xAF=>absolute_long, 0xB1=>indirect_indexed,
                 0xB2=>indirect,0xB3=>stack_relative_indirect_indexed,
                 0xB5=>direct_x, 0xB7=>indirect_long_y, 0xB9=>absolute_y,
                 0xBD=>absolute_x, 0xBF=>absolute_long_x)
                (ldx, XFlag; 0xA2=>immediate, 0xA6=>direct, 0xAE=>absolute,
                 0xB6=>direct_y, 0xBE=>absolute_y)
                (ldy, XFlag; 0xA0=>immediate, 0xA4=>direct, 0xAC=>absolute,
                 0xB4=>direct_x, 0xBC=>absolute_x)
                (stz, MFlag; 0x64=>direct, 0x74=>direct_x, 0x9C=>absolute,
                 0x9E=>absolute_x)
                (sta, MFlag; 0x81=>indexed_indirect, 0x83=>stack_relative,
                 0x85=>direct, 0x87=>indirect_long, 0x8D=>absolute,
                 0x8F=>absolute_long, 0x91=>indirect_indexed,
                 0x92=>indirect,0x93=>stack_relative_indirect_indexed,
                 0x95=>direct_x, 0x97=>indirect_long_y, 0x99=>absolute_y,
                 0x9D=>absolute_x, 0x9F=>absolute_long_x)
                (stx, XFlag; 0x86=>direct, 0x8E=>absolute, 0x96=>direct_y)
                (sty, XFlag; 0x84=>direct, 0x8C=>absolute, 0x94=>direct_x)
                (inc, MFlag; 0xE6=>direct, 0xEE=>absolute, 0xF6=>direct_x,
                 0xFE=>absolute_x)
                (ina, NoFlag; 0x1A=>implied)
                (inx, NoFlag; 0xE8=>implied)
                (iny, NoFlag; 0xC8=>implied)
                (adc, MFlag; 0x61=>indexed_indirect, 0x63=>stack_relative,
                 0x65=>direct, 0x67=>indirect_long, 0x69=>immediate,
                 0x6D=>absolute, 0x6F=>absolute_long, 0x71=>indirect_indexed,
                 0x72=>indirect,0x73=>stack_relative_indirect_indexed,
                 0x75=>direct_x, 0x77=>indirect_long_y, 0x79=>absolute_y,
                 0x7D=>absolute_x, 0x7F=>absolute_long_x)
                (sbc, MFlag; 0xE1=>indexed_indirect, 0xE3=>stack_relative,
                 0xE5=>direct, 0xE7=>indirect_long, 0xE9=>immediate,
                 0xED=>absolute, 0xEF=>absolute_long, 0xF1=>indirect_indexed,
                 0xF2=>indirect,0xF3=>stack_relative_indirect_indexed,
                 0xF5=>direct_x, 0xF7=>indirect_long_y, 0xF9=>absolute_y,
                 0xFD=>absolute_x, 0xFF=>absolute_long_x)
                (cmp, MFlag; 0xC1=>indexed_indirect, 0xC3=>stack_relative,
                 0xC5=>direct, 0xC7=>indirect_long, 0xC9=>immediate,
                 0xCD=>absolute, 0xCF=>absolute_long, 0xD1=>indirect_indexed,
                 0xD2=>indirect,0xD3=>stack_relative_indirect_indexed,
                 0xD5=>direct_x, 0xD7=>indirect_long_y, 0xD9=>absolute_y,
                 0xDD=>absolute_x, 0xDF=>absolute_long_x)
                (cpx, XFlag; 0xE0=>immediate, 0xE4=>direct, 0xEC=>absolute)
                (cpy, XFlag; 0xC0=>immediate, 0xC4=>direct, 0xCC=>absolute)
                (pull_d, NoFlag; 0x2B=>implied)
                (pull_a, NoFlag; 0x68=>implied)
                (pull_y, NoFlag; 0x7A=>implied)
                (pull_b, NoFlag; 0xAB=>implied)
                (pull_x, NoFlag; 0xFA=>implied)
                (transfer_a_x, NoFlag; 0xAA=>implied)
                (transfer_a_y, NoFlag; 0xA8=>implied)
                (transfer_a_d, NoFlag; 0x5B=>implied)
                (transfer_a_sp, NoFlag; 0x1B=>implied)
                (transfer_d_a, NoFlag; 0x7B=>implied)
                (transfer_sp_a, NoFlag; 0x3B=>implied)
                (transfer_sp_x, NoFlag; 0xBA=>implied)
                (transfer_x_a, NoFlag; 0x8A=>implied)
                (transfer_x_sp, NoFlag; 0x9A=>implied)
                (transfer_x_y, NoFlag; 0x9B=>implied)
                (transfer_y_a, NoFlag; 0x98=>implied)
                (transfer_y_x, NoFlag; 0xBB=>implied)
            );
        }
    }

    fn step(&mut self, n_clocks: u64) {
        self.ticks_run += n_clocks;
    }

    fn read_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: Some clock cycles before the read, depending on region
            let data = yield_all!(Bus::read_u8(cpu.borrow_mut().bus.clone(), addr));
            cpu.borrow_mut().step(1);
            data
        }
    }

    fn write_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            // TODO: Some clock cycles before the write, depending on region
            yield_all!(Bus::write_u8(cpu.borrow_mut().bus.clone(), addr, data));
            cpu.borrow_mut().step(4);
        }
    }

    fn read_pointer<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl Yieldable<u16> + 'a {
        move || {
            let mut data = yield_all!(CPU::read_u8(cpu.clone(), pointer.addr)) as u16;
            if pointer.long {
                data |= (yield_all!(CPU::read_u8(cpu.clone(), pointer.addr + 1u32)) as u16) << 8;
            }
            data
        }
    }

    fn write_pointer<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
        data: u16,
    ) -> impl Yieldable<()> + 'a {
        move || {
            yield_all!(CPU::write_u8(
                cpu.clone(),
                pointer.addr,
                (data & 0xFF) as u8
            ));
            if pointer.long {
                yield_all!(CPU::write_u8(
                    cpu.clone(),
                    pointer.addr + 1u32,
                    (data >> 8) as u8
                ))
            }
        }
    }

    fn direct_addr(cpu: Rc<RefCell<CPU>>, addr: u24) -> u24 {
        if cpu.borrow().reg.p.e && (cpu.borrow().reg.d & 0xFF == 0) {
            u24(cpu.borrow().reg.d.into()) | (addr & 0xFFu8)
        } else {
            (u24(cpu.borrow().reg.d.into()) + addr) & 0xFFFFu32
        }
    }

    fn read_direct_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            yield_all!(CPU::read_u8(
                cpu.clone(),
                CPU::direct_addr(cpu.clone(), addr)
            ))
        }
    }

    fn bank_addr(cpu: Rc<RefCell<CPU>>, addr: u24) -> u24 {
        u24((cpu.borrow().reg.b as u32) << 16) | addr
    }

    fn read_bank_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || yield_all!(CPU::read_u8(cpu.clone(), CPU::bank_addr(cpu.clone(), addr)))
    }

    fn stack_pull_u8<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u8> + 'a {
        // TODO: Does 0x100 ever get used, namely in emulation mode?
        move || {
            cpu.borrow_mut().reg.sp += 1;
            let val = yield_all!(CPU::read_u8(cpu.clone(), u24(cpu.borrow().reg.sp as u32)));
            val
        }
    }

    fn stack_pull_u16<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        move || {
            let lo = yield_all!(CPU::stack_pull_u8(cpu.clone()));
            let hi = yield_all!(CPU::stack_pull_u8(cpu.clone()));
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

    fn stack_push_u8<'a>(cpu: Rc<RefCell<CPU>>, data: u8) -> impl Yieldable<()> + 'a {
        // TODO: Does 0x100 ever get used, namely in emulation mode?
        move || {
            yield_all!(CPU::write_u8(
                cpu.clone(),
                u24(cpu.borrow().reg.sp as u32),
                data
            ));
            cpu.borrow_mut().reg.sp -= 1;
        }
    }

    fn stack_push_u16<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl Yieldable<()> + 'a {
        move || {
            yield_all!(CPU::stack_push_u8(cpu.clone(), (data >> 8) as u8));
            yield_all!(CPU::stack_push_u8(cpu.clone(), (data & 0xFF) as u8));
        }
    }

    fn immediate<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            dummy_yield!();
            let addr = cpu.borrow().reg.pc;
            cpu.borrow_mut().reg.pc += if long { u24(2) } else { u24(1) };
            Pointer { addr, long }
        }
    }

    // TODO: Reduce code duplication across these three
    fn direct<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let addr = CPU::direct_addr(cpu.clone(), indirect_addr);
            Pointer { addr, long }
        }
    }

    fn direct_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.get_x() as u32);
            let addr = CPU::direct_addr(cpu.clone(), indirect_addr);
            Pointer { addr, long }
        }
    }

    fn direct_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.get_y() as u32);
            let addr = CPU::direct_addr(cpu.clone(), indirect_addr);
            Pointer { addr, long }
        }
    }

    // TODO: Reduce code duplication across these three
    fn absolute<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                u24((fetch!(cpu) << 8) as u32 | addr_lo)
            };
            Pointer { addr, long }
        }
    }

    fn absolute_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                u24(((fetch!(cpu) << 8) as u32 | addr_lo) + cpu.borrow().reg.get_x() as u32)
            };
            Pointer { addr, long }
        }
    }

    fn absolute_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                u24(((fetch!(cpu) << 8) as u32 | addr_lo) + cpu.borrow().reg.get_y() as u32)
            };
            Pointer { addr, long }
        }
    }

    fn absolute_long<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                let addr_mid = fetch!(cpu) as u32;
                u24(((fetch!(cpu) as u32) << 16) | (addr_mid << 8) | addr_lo)
            };
            Pointer { addr, long }
        }
    }

    fn absolute_long_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                let addr_mid = fetch!(cpu) as u32;
                u24((((fetch!(cpu) as u32) << 16) | (addr_mid << 8) | addr_lo)
                    + cpu.borrow().reg.get_x() as u32)
            };
            Pointer { addr, long }
        }
    }

    fn absolute_indirect_indexed<'a>(
        cpu: Rc<RefCell<CPU>>,
        _: bool,
    ) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = {
                let pb = cpu.borrow().reg.pc.hi8();
                let addr_lo = fetch!(cpu) as u32;
                u24(((pb << 16) as u32 | (fetch!(cpu) << 8) as u32 | addr_lo)
                    + cpu.borrow().reg.get_x() as u32)
            };
            let addr = u24({
                let addr_lo = yield_all!(CPU::read_u8(cpu.clone(), indirect_addr)) as u32;
                ((yield_all!(CPU::read_u8(cpu.clone(), indirect_addr + 1u32)) as u32) << 8)
                    | addr_lo
            });
            Pointer { addr, long: true }
        }
    }

    // TODO: Reduce code duplication across these three
    fn indirect<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            };
            Pointer { addr, long }
        }
    }

    fn indexed_indirect<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let offset = cpu.borrow().reg.get_x() as u32;
            let indirect_addr = u24(fetch!(cpu) as u32 + offset);
            let bank_addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            };
            let addr = CPU::bank_addr(cpu.clone(), bank_addr);
            Pointer { addr, long }
        }
    }

    fn indirect_indexed<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let offset = cpu.borrow().reg.get_y() as u32;
            let indirect_addr = u24(fetch!(cpu) as u32);
            let bank_addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            } + offset;
            let addr = CPU::bank_addr(cpu.clone(), bank_addr);
            Pointer { addr, long }
        }
    }

    fn indirect_long<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let bank_addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            };
            let addr = CPU::bank_addr(cpu.clone(), bank_addr);
            Pointer { addr, long }
        }
    }

    fn indirect_long_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let bank_addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                u24((hi << 8) | lo)
            } + cpu.borrow().reg.get_y() as u32;
            // TODO: Check the above logic; namely, the overflow of adding y
            let addr = CPU::bank_addr(cpu.clone(), bank_addr);
            Pointer { addr, long }
        }
    }

    fn stack_relative<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        move || {
            let bank_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.sp as u32);
            let addr = CPU::bank_addr(cpu.clone(), bank_addr);
            Pointer { addr, long }
        }
    }

    fn stack_relative_indirect_indexed<'a>(
        cpu: Rc<RefCell<CPU>>,
        long: bool,
    ) -> impl Yieldable<Pointer> + 'a {
        move || {
            let stack_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.sp as u32);
            let addr_lo = yield_all!(CPU::read_u8(cpu.clone(), stack_addr)) as u32;
            let bank_addr = ((yield_all!(CPU::read_u8(cpu.clone(), stack_addr + 1u32)) as u32)
                << 8)
                | addr_lo + cpu.borrow().reg.get_y() as u32;
            let addr = CPU::bank_addr(cpu.clone(), u24(bank_addr));
            Pointer { addr, long }
        }
    }

    fn and<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
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

    fn rep<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let p = &mut cpu.borrow_mut().reg.p;
            p.set(p.get() & !data as u8);
        }
    }

    fn sep<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let p = &mut cpu.borrow_mut().reg.p;
            p.set(p.get() | data as u8);
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
    fn lda<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            cpu.borrow_mut().reg.set_a(data);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn ldx<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            cpu.borrow_mut().reg.set_x(data);
            // TODO: Need to take into account e flag; probably factor out (maybe to
            // StatusRegister)
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn ldy<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            cpu.borrow_mut().reg.set_y(data);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn stz<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, 0));
        }
    }

    fn sta<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = cpu.borrow().reg.get_a();
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
        }
    }

    fn stx<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = cpu.borrow().reg.get_x();
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
        }
    }

    fn sty<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = cpu.borrow().reg.get_y();
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
        }
    }

    fn ina<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_a().wrapping_add(1);
            cpu.borrow_mut().reg.set_a(data);
            let n_bits = if cpu.borrow().reg.p.m || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn inx<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_x().wrapping_add(1);
            cpu.borrow_mut().reg.set_x(data);
            let n_bits = if cpu.borrow().reg.p.x_or_b || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn iny<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_y().wrapping_add(1);
            cpu.borrow_mut().reg.set_y(data);
            let n_bits = if cpu.borrow().reg.p.x_or_b || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn inc<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer)).wrapping_add(1);
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
            let n_bits = if cpu.borrow().reg.p.m || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = data == 0;
        }
    }

    fn arithmetic_op<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
        subtract: bool,
    ) -> impl InstructionGenerator + 'a {
        move || {
            // TODO: Really need to check the 8- and 16-bit flag logic
            let data = {
                let mem_val = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
                if subtract {
                    ((mem_val as i16).wrapping_neg().wrapping_sub(1)) as u16
                } else {
                    mem_val
                }
            };

            let carry = cpu.borrow().reg.p.c as u16;
            let result = if cpu.borrow().reg.p.d {
                // let temp = bcd_to_bin(self.a).unwrap() + bcd_to_bin(data).unwrap() + carry as u8;
                // self.p.set(StatusRegister::CARRY, temp > 99);
                // bin_to_bcd(temp % 100).unwrap()
                unimplemented!("BCD Decimal mode is not yet");
            } else {
                let temp = cpu.borrow().reg.get_a() as i32 + data as i32 + carry as i32;
                cpu.borrow_mut().reg.p.c = temp > 0xFFFF;
                temp as u16
            };

            cpu.borrow_mut().reg.p.v =
                ((cpu.borrow().reg.get_a() ^ result) & (data ^ result) & 0x80) != 0;
            cpu.borrow_mut().reg.set_a(result);
            let n_bits = if cpu.borrow().reg.p.m || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };
            cpu.borrow_mut().reg.p.n = (result >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = result == 0;
        }
    }

    fn adc<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        return CPU::arithmetic_op(cpu, pointer, false);
    }

    fn sbc<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        return CPU::arithmetic_op(cpu, pointer, true);
    }

    fn compare_op<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
        reg_val: u16,
        flag: bool,
    ) -> impl InstructionGenerator + 'a {
        move || {
            // TODO: Really need to check the 8- and 16-bit flag logic

            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let result = reg_val as i32 - data as i32;

            let n_bits = if flag || cpu.borrow().reg.p.e { 8 } else { 16 };
            cpu.borrow_mut().reg.p.c = result >= 0;
            cpu.borrow_mut().reg.p.n = (result >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = result == 0;
        }
    }

    fn cmp<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        let reg_val = cpu.borrow().reg.get_a();
        let flag = cpu.borrow().reg.p.m;
        return CPU::compare_op(cpu, pointer, reg_val, flag);
    }

    fn cpx<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        let reg_val = cpu.borrow().reg.get_x();
        let flag = cpu.borrow().reg.p.x_or_b;
        return CPU::compare_op(cpu, pointer, reg_val, flag);
    }

    fn cpy<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        let reg_val = cpu.borrow().reg.get_y();
        let flag = cpu.borrow().reg.p.x_or_b;
        return CPU::compare_op(cpu, pointer, reg_val, flag);
    }

    fn jsr<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let return_pc = cpu.borrow().reg.pc.lo16();
            yield_all!(CPU::stack_push_u16(cpu.clone(), return_pc));
            cpu.borrow_mut().reg.pc &= 0xFF_0000u32;
            cpu.borrow_mut().reg.pc |= pointer.addr.lo16();
        }
    }

    fn jsl<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionGenerator + 'a {
        move || {
            let return_pb = cpu.borrow().reg.pc.hi8();
            let return_pc = cpu.borrow().reg.pc.lo16();
            yield_all!(CPU::stack_push_u8(cpu.clone(), return_pb));
            yield_all!(CPU::stack_push_u16(cpu.clone(), return_pc));
            cpu.borrow_mut().reg.pc = pointer.addr;
        }
    }

    pull_instrs!();
    transfer_instrs!();
    branch_instrs!();
}
