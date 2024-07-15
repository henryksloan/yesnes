pub mod registers;

pub use registers::{IoRegisters, Registers, StatusRegister};

use crate::{bus::Bus, ppu::PpuCounter, scheduler::*, u24::u24};
// DO NOT SUBMIT
use crate::frontend::frame_history::FrameHistory;

use std::cell::RefCell;
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;
use std::time::Instant;

use paste::paste;

// Emulation mode (6502) vectors
pub const RESET_VECTOR: u24 = u24(0xFFFC);
pub const NMI_VECTOR_E: u24 = u24(0xFFFA);
pub const IRQ_VECTOR_E: u24 = u24(0xFFFE);
pub const ABORT_VECTOR_E: u24 = u24(0xFFF8);
pub const BRK_VECTOR_E: u24 = u24(0xFFFE);
pub const COP_VECTOR_E: u24 = u24(0xFFF4);

// 65C816 mode vectors
pub const NMI_VECTOR: u24 = u24(0xFFEA);
pub const IRQ_VECTOR: u24 = u24(0xFFEE);
pub const ABORT_VECTOR: u24 = u24(0xFFF8);
pub const BRK_VECTOR: u24 = u24(0xFFE6);
pub const COP_VECTOR: u24 = u24(0xFFE4);

macro_rules! yield_ticks {
    ($cpu_rc:ident, $gen_expr:expr) => {{
        let mut gen = $gen_expr;
        loop {
            match Pin::new(&mut gen).resume(()) {
                CoroutineState::Yielded(yield_reason) => {
                    let ticks_to_yield = $cpu_rc.borrow().ticks_run;
                    $cpu_rc.borrow_mut().ticks_run = 0;
                    yield (yield_reason, ticks_to_yield)
                }
                CoroutineState::Complete(out) => break out,
            }
        }
    }};
}

pub(crate) use yield_ticks;

// TODO: Replace some of these with e.g. index_reg_16_bits
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
            fn [<pull_ $reg>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
                #[coroutine] move || {
                    yield_all!(CPU::idle(cpu.clone()));
                    yield_all!(CPU::idle(cpu.clone()));
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

macro_rules! push_instrs {
    (kind: $kind:ident, $($reg:ident),*) => {
        paste! {
            $(
            fn [<push_ $reg>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
                #[coroutine] move || {
                    yield_all!(CPU::idle(cpu.clone()));
                    let data = cpu.borrow_mut().reg.[<get_ $reg>]();
                    yield_all!(CPU::[<stack_push_ $kind>](cpu.clone(), data));
                }
            }
            )*
        }
    };
    () => {
        push_instrs!(kind: m, a);
        push_instrs!(kind: x, x, y);
        push_instrs!(kind: u16, d);
        push_instrs!(kind: u8, p, b);
    }
}

macro_rules! transfer_instrs {
    (kind: $kind:ident, $from:ident => $to:ident) => {
        paste! {
            fn [<transfer_ $from _ $to>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
                #[coroutine] move || {
                    yield_all!(CPU::idle(cpu.clone()));
                    let data = cpu.borrow().reg.$from & ((1u32 << n_bits!(cpu, $kind)) - 1) as u16;
                    cpu.borrow_mut().reg.[<set_ $to>](data);
                    if n_bits!(cpu, $kind) == 16 {
                        cpu.borrow_mut().reg.$to = data;
                    } else {
                        cpu.borrow_mut().reg.$to &= 0xFF00;
                        cpu.borrow_mut().reg.$to |= data & 0xFF;
                    }
                    let result = cpu.borrow().reg.$to & ((1u32 << n_bits!(cpu, $kind)) - 1) as u16;
                    cpu.borrow_mut().reg.p.n = (result >> (n_bits!(cpu, $kind) - 1)) == 1;
                    cpu.borrow_mut().reg.p.z = result == 0;
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
        transfer_instrs!(kind: x, x => y);
        transfer_instrs!(kind: x, y => x);
        transfer_instrs!(kind: u16, d => a);
        transfer_instrs!(kind: u16, a => d);
        transfer_instrs!(kind: u16, sp => a);
    }
}

macro_rules! branch_instrs {
    ($flag:ident => $val:expr, $set_clear:ident) => {
        paste! {
            fn [<branch_ $flag _ $set_clear>]<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
                #[coroutine] move || {
                    // let source_pc = cpu.borrow().reg.pc + 1u16;
                    // let dest_pc = u24((source_pc.raw() as i32 + (fetch!(cpu) as i8 as i32)) as u32);
                    // TODO: NOTE: This is a critical bug fix
                    let source_pc = cpu.borrow().reg.pc;
                    let offset = fetch!(cpu) as i8 as i16;
                    let dest_pc = source_pc.wrapping_add_lo16(offset + 1);
                    if cpu.borrow_mut().reg.p.$flag == $val {
                        cpu.borrow_mut().reg.pc = dest_pc;
                        yield_all!(CPU::idle(cpu.clone()));
                        // TODO: Is this right? Maybe only for emulation mode...
                        if (source_pc & 0x100u16).raw() != (dest_pc & 0x100u16).raw() {
                            yield_all!(CPU::idle(cpu.clone()));
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
            _ => panic!("Invalid CPU opcode {:#04X} at {:#08X}", opcode_val, $cpu_rc.borrow().reg.pc.raw().wrapping_sub(1)),
        }
    };
}

macro_rules! fetch {
    ($cpu_rc: ident) => {{
        let data = yield_all!(CPU::read_u8($cpu_rc.clone(), $cpu_rc.borrow().reg.pc));
        $cpu_rc.borrow_mut().progress_pc(1);
        data
    }};
}

macro_rules! fetch_u16 {
    ($cpu_rc: ident) => {{
        let lo = yield_all!(CPU::read_u8($cpu_rc.clone(), $cpu_rc.borrow().reg.pc));
        $cpu_rc.borrow_mut().progress_pc(1);
        let hi = yield_all!(CPU::read_u8($cpu_rc.clone(), $cpu_rc.borrow().reg.pc));
        $cpu_rc.borrow_mut().progress_pc(1);
        ((hi as u16) << 8) | lo as u16
    }};
}

/// Translates a binary integer to a "Binary Coded Decimal"
/// i.e. decimal(49) => 0x49
fn bin_to_bcd(x: u16) -> u16 {
    let ones = x % 10;
    let tens = (x / 10) % 10;
    let hundreds = (x / 100) % 10;
    let thousands = (x / 1000) % 10;
    (thousands << 12) | (hundreds << 8) | (tens << 4) | ones
}

/// Translates a "Binary Coded Decimal" to a binary integer
/// i.e. 0x49 => decimal(49)
fn bcd_to_bin(x: u16) -> u16 {
    let ones = x & 0x000F;
    let tens = (x & 0x00F0) >> 4;
    let hundreds = (x & 0x0F00) >> 8;
    let thousands = (x & 0xF000) >> 12;
    1000 * thousands + 100 * hundreds + 10 * tens + ones
}

#[derive(Copy, Clone)]
struct Pointer {
    pub addr: u24,
    pub long: bool,
}

/// The 65816 microprocessor, the main CPU of the SNES
pub struct CPU {
    reg: Registers,
    io_reg: IoRegisters,
    bus: Rc<RefCell<Bus>>,
    pub ppu_counter: Rc<RefCell<PpuCounter>>,
    dmas_enqueued: Option<u8>,
    hdmas_enqueued: Option<u8>,
    hdmas_ongoing: Option<u8>,
    hdma_triggered_this_line: bool,
    hdma_setup_triggered_this_line: bool,
    hdma_pending_this_line: bool,
    hdmas_complete_this_frame: [bool; 8],
    do_hdmas_this_line: [bool; 8],
    nmi_enqueued: bool,
    irq_enqueued: bool,
    stopped: bool,
    // TODO: Should change most instances of "tick" to "clock"
    ticks_run: u64,
    // Some events like interrupt polling happen every 4 ticks. This tracks the
    // remainder ticks from `step()`, e.g. if `2` or `5` ticks elapse.
    // DO NOT SUBMIT: Consider encapsulating this (and much else) into some interrupt controller abstraction.
    ticks_mod_4: u8,
    // TODO: Remove debug variable once there's a real way to alert frontend of frames
    pub debug_frame: Option<[[[u8; 3]; 256]; 224]>,
    pub controller_states: [u16; 4],
    // DO NOT SUBMIT
    frame_history: FrameHistory,
    previous_frame_instant: Option<Instant>,
    print_timer: u64,
    debug_latch: bool,
}

impl CPU {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self {
            reg: Registers::new(),
            io_reg: IoRegisters::new(),
            bus,
            ppu_counter: Rc::new(RefCell::new(PpuCounter::new())),
            dmas_enqueued: None,
            hdmas_enqueued: None,
            hdmas_ongoing: None,
            hdma_triggered_this_line: false,
            hdma_setup_triggered_this_line: false,
            hdma_pending_this_line: false,
            hdmas_complete_this_frame: [false; 8],
            do_hdmas_this_line: [false; 8],
            nmi_enqueued: false,
            irq_enqueued: false,
            stopped: false,
            ticks_run: 0,
            ticks_mod_4: 0,
            debug_frame: None,
            controller_states: [0; 4],
            frame_history: FrameHistory::new(),
            previous_frame_instant: None,
            print_timer: 0,
            debug_latch: false,
        }
    }

    pub fn registers(&self) -> &Registers {
        &self.reg
    }

    pub fn registers_mut(&mut self) -> &mut Registers {
        &mut self.reg
    }

    pub fn reset(&mut self) {
        self.ticks_run = 0;
        self.reg = Registers::new();
        self.reg.pc = u24(ignore_yields!(Bus::read_u16(self.bus.clone(), RESET_VECTOR)) as u32);
        self.reg.set_p(0x34);
        self.reg.p.e = true;
        self.reg.set_sp(0x1FF);

        // TODO: DO NOT SUBMIT: untested resetting; more stuff is definitely msising
        self.nmi_enqueued = false;
        self.irq_enqueued = false;
        self.stopped = false;

        self.io_reg.waitstate_control.0 = 0;
        self.io_reg.interrupt_control.0 = 0;
        for channel_regs in self.io_reg.dma_channels.iter_mut() {
            channel_regs.setup.0 = 0xFF;
            channel_regs.io_reg = 0xFF;
            channel_regs.addr.0 = u24(0xFFFFFF);
            channel_regs.indirect_addr_or_byte_count.0 = u24(0xFFFFFF);
            channel_regs.hdma_table_curr_addr.0 = 0xFF;
            channel_regs.hdma_line_counter.0 = 0xFF;
            channel_regs.unused_byte = 0xFF;
        }
    }

    pub fn run<'a>(cpu: Rc<RefCell<CPU>>) -> impl DeviceCoroutine + 'a {
        // TODO: How to handle interrupts from e.g. scanlines? [[yesnes Interrupts]]
        #[coroutine]
        move || loop {
            // if cpu.borrow().stopped {
            //     // TODO: DO NOT SUBMIT: Fix this
            //     yield (YieldReason::Sync(Device::SMP), 1);
            //     yield (YieldReason::Sync(Device::PPU), 1);
            //     yield (YieldReason::FinishedInstruction(Device::CPU), 0);
            //     continue;
            // }
            // print!("CPU {:#010X}", cpu.borrow().reg.pc.raw());
            let opcode = yield_ticks!(cpu, CPU::read_u8(cpu.clone(), cpu.borrow().reg.pc));
            // TODO: Need to go through and use wrapping arithmetic where appropriate
            cpu.borrow_mut().progress_pc(1);
            // {
            //     let reg = &cpu.borrow().reg;
            //     println!(
            //         ": {opcode:#04X}    A:{:04X} X:{:04X} Y:{:04X} SP:{:04X} P:{:02X} E:{}",
            //         reg.get_a(),
            //         reg.get_x(),
            //         reg.get_y(),
            //         reg.get_sp(),
            //         reg.get_p(),
            //         if reg.p.e { 1 } else { 0 },
            //     );
            // }

            // TODO: Add COP (coprocessor interrupt)
            instrs!(cpu, opcode,
                (brk, NoFlag; 0x0=>implied)
                (clc, NoFlag; 0x18=>implied)
                (cli, NoFlag; 0x58=>implied)
                (cld, NoFlag; 0xD8=>implied)
                (clv, NoFlag; 0xB8=>implied)
                (sec, NoFlag; 0x38=>implied)
                (sei, NoFlag; 0x78=>implied)
                (sed, NoFlag; 0xF8=>implied)
                (rep, NoFlag; 0xC2=>immediate)
                (sep, NoFlag; 0xE2=>immediate)
                (xba, NoFlag; 0xEB=>implied)
                (xce, NoFlag; 0xFB=>implied)
                (branch_n_clear, NoFlag; 0x10=>implied)
                (branch_n_set, NoFlag; 0x30=>implied)
                (branch_v_clear, NoFlag; 0x50=>implied)
                (branch_v_set, NoFlag; 0x70=>implied)
                (branch_c_clear, NoFlag; 0x90=>implied)
                (branch_c_set, NoFlag; 0xB0=>implied)
                (branch_z_clear, NoFlag; 0xD0=>implied)
                (branch_z_set, NoFlag; 0xF0=>implied)
                (bra, NoFlag; 0x80=>implied)
                (brl, NoFlag; 0x82=>implied)
                (jsr, NoFlag; 0x20=>absolute, 0xFC=>absolute_indirect_indexed)
                (jmp, NoFlag; 0x4C=>absolute, 0x6C=>absolute_indirect,
                 0x7C=>absolute_indirect_indexed)
                (jml, NoFlag; 0x5C=>absolute_long, 0xDC=>absolute_indirect_long)
                (jsl, NoFlag; 0x22=>absolute_long)
                (rtl, NoFlag; 0x6B=>implied)
                (rts, NoFlag; 0x60=>implied)
                (rti, NoFlag; 0x40=>implied)
                (ora, MFlag; 0x01=>indexed_indirect, 0x03=>stack_relative,
                 0x05=>direct, 0x07=>indirect_long, 0x09=>immediate,
                 0x0D=>absolute, 0x0F=>absolute_long, 0x11=>indirect_indexed,
                 0x12=>indirect, 0x13=>stack_relative_indirect_indexed,
                 0x15=>direct_x, 0x17=>indirect_long_y, 0x19=>absolute_y,
                 0x1D=>absolute_x, 0x1F=>absolute_long_x)
                (and, MFlag; 0x21=>indexed_indirect, 0x23=>stack_relative,
                 0x25=>direct, 0x27=>indirect_long, 0x29=>immediate,
                 0x2D=>absolute, 0x2F=>absolute_long, 0x31=>indirect_indexed,
                 0x32=>indirect, 0x33=>stack_relative_indirect_indexed,
                 0x35=>direct_x, 0x37=>indirect_long_y, 0x39=>absolute_y,
                 0x3D=>absolute_x, 0x3F=>absolute_long_x)
                (eor, MFlag; 0x41=>indexed_indirect, 0x43=>stack_relative,
                 0x45=>direct, 0x47=>indirect_long, 0x49=>immediate,
                 0x4D=>absolute, 0x4F=>absolute_long, 0x51=>indirect_indexed,
                 0x52=>indirect, 0x53=>stack_relative_indirect_indexed,
                 0x55=>direct_x, 0x57=>indirect_long_y, 0x59=>absolute_y,
                 0x5D=>absolute_x, 0x5F=>absolute_long_x)
                (bit_immediate, MFlag; 0x89=>immediate)
                (bit, MFlag; 0x24=>direct, 0x2C=>absolute, 0x34=>direct_x,
                 0x3C=>absolute_x)
                (tsb, MFlag; 0x04=>direct, 0x0C=>absolute)
                (trb, MFlag; 0x14=>direct, 0x1C=>absolute)
                (lda, MFlag; 0xA1=>indexed_indirect, 0xA3=>stack_relative,
                 0xA5=>direct, 0xA7=>indirect_long, 0xA9=>immediate,
                 0xAD=>absolute, 0xAF=>absolute_long, 0xB1=>indirect_indexed,
                 0xB2=>indirect, 0xB3=>stack_relative_indirect_indexed,
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
                 0x92=>indirect, 0x93=>stack_relative_indirect_indexed,
                 0x95=>direct_x, 0x97=>indirect_long_y, 0x99=>absolute_y,
                 0x9D=>absolute_x, 0x9F=>absolute_long_x)
                (stx, XFlag; 0x86=>direct, 0x8E=>absolute, 0x96=>direct_y)
                (sty, XFlag; 0x84=>direct, 0x8C=>absolute, 0x94=>direct_x)
                (mvp, NoFlag; 0x44=>implied)
                (mvn, NoFlag; 0x54=>implied)
                (inc, MFlag; 0xE6=>direct, 0xEE=>absolute, 0xF6=>direct_x,
                 0xFE=>absolute_x)
                (ina, NoFlag; 0x1A=>implied)
                (inx, NoFlag; 0xE8=>implied)
                (iny, NoFlag; 0xC8=>implied)
                (dec, MFlag; 0xC6=>direct, 0xCE=>absolute, 0xD6=>direct_x,
                 0xDE=>absolute_x)
                (dea, NoFlag; 0x3A=>implied)
                (dex, NoFlag; 0xCA=>implied)
                (dey, NoFlag; 0x88=>implied)
                (adc, MFlag; 0x61=>indexed_indirect, 0x63=>stack_relative,
                 0x65=>direct, 0x67=>indirect_long, 0x69=>immediate,
                 0x6D=>absolute, 0x6F=>absolute_long, 0x71=>indirect_indexed,
                 0x72=>indirect, 0x73=>stack_relative_indirect_indexed,
                 0x75=>direct_x, 0x77=>indirect_long_y, 0x79=>absolute_y,
                 0x7D=>absolute_x, 0x7F=>absolute_long_x)
                (sbc, MFlag; 0xE1=>indexed_indirect, 0xE3=>stack_relative,
                 0xE5=>direct, 0xE7=>indirect_long, 0xE9=>immediate,
                 0xED=>absolute, 0xEF=>absolute_long, 0xF1=>indirect_indexed,
                 0xF2=>indirect, 0xF3=>stack_relative_indirect_indexed,
                 0xF5=>direct_x, 0xF7=>indirect_long_y, 0xF9=>absolute_y,
                 0xFD=>absolute_x, 0xFF=>absolute_long_x)
                (cmp, MFlag; 0xC1=>indexed_indirect, 0xC3=>stack_relative,
                 0xC5=>direct, 0xC7=>indirect_long, 0xC9=>immediate,
                 0xCD=>absolute, 0xCF=>absolute_long, 0xD1=>indirect_indexed,
                 0xD2=>indirect, 0xD3=>stack_relative_indirect_indexed,
                 0xD5=>direct_x, 0xD7=>indirect_long_y, 0xD9=>absolute_y,
                 0xDD=>absolute_x, 0xDF=>absolute_long_x)
                (cpx, XFlag; 0xE0=>immediate, 0xE4=>direct, 0xEC=>absolute)
                (cpy, XFlag; 0xC0=>immediate, 0xC4=>direct, 0xCC=>absolute)
                (asl_a, NoFlag; 0x0A=>implied)
                (asl, MFlag; 0x06=>direct,  0x0E=>absolute, 0x16=>direct_x,
                 0x1E=>absolute_x)
                (lsr_a, NoFlag; 0x4A=>implied)
                (lsr, MFlag; 0x46=>direct,  0x4E=>absolute, 0x56=>direct_x,
                 0x5E=>absolute_x)
                (rol_a, NoFlag; 0x2A=>implied)
                (rol, MFlag; 0x26=>direct, 0x2E=>absolute, 0x36=>direct_x,
                 0x3E=>absolute_x)
                (ror_a, NoFlag; 0x6A=>implied)
                (ror, MFlag; 0x66=>direct, 0x6E=>absolute, 0x76=>direct_x,
                 0x7E=>absolute_x)
                (nop, NoFlag; 0xEA=>implied)
                (push_a, NoFlag; 0x48=>implied)
                (push_b, NoFlag; 0x8B=>implied)
                (push_d, NoFlag; 0x0B=>implied)
                (push_pb, NoFlag; 0x4B=>implied)
                (push_p, NoFlag; 0x08=>implied)
                (push_x, NoFlag; 0xDA=>implied)
                (push_y, NoFlag; 0x5A=>implied)
                (push_addr, NoFlag; 0xD4=>indirect, 0xF4=>absolute)
                (push_relative_addr, NoFlag; 0x62=>implied)
                (pull_a, NoFlag; 0x68=>implied)
                (pull_b, NoFlag; 0xAB=>implied)
                (pull_d, NoFlag; 0x2B=>implied)
                (pull_p, NoFlag; 0x28=>implied)
                (pull_x, NoFlag; 0xFA=>implied)
                (pull_y, NoFlag; 0x7A=>implied)
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
                // TODO: DO NOT SUBMIT: untested STP, definitely breaks frontend (e.g. trace)
                (stp, NoFlag; 0xDB=>implied)
            );

            // TODO: Proper DMA activation and pause timing
            let dmas_enqueued = cpu.borrow_mut().dmas_enqueued.take();
            if let Some(dmas_enqueued) = dmas_enqueued {
                yield_ticks!(cpu, CPU::run_dma(cpu.clone(), dmas_enqueued));
            }

            if !cpu.borrow().hdma_setup_triggered_this_line {
                let hdmas_ongoing = cpu.borrow_mut().hdmas_ongoing;
                if let Some(hdmas_ongoing) = hdmas_ongoing {
                    cpu.borrow_mut().hdma_setup_triggered_this_line = true;
                    yield_ticks!(cpu, CPU::setup_hdma(cpu.clone(), hdmas_ongoing));
                }
            }

            if cpu.borrow().hdma_pending_this_line {
                cpu.borrow_mut().hdma_pending_this_line = false;
                let hdmas_ongoing = cpu.borrow_mut().hdmas_ongoing;
                if let Some(hdmas_ongoing) = hdmas_ongoing {
                    yield_ticks!(cpu, CPU::run_hdma(cpu.clone(), hdmas_ongoing));
                }
            }

            if cpu.borrow().nmi_enqueued {
                cpu.borrow_mut().nmi_enqueued = false;
                let vector = if cpu.borrow().reg.p.e {
                    NMI_VECTOR_E
                } else {
                    NMI_VECTOR
                };
                yield_ticks!(cpu, CPU::interrupt(cpu.clone(), vector));
            }

            if cpu.borrow().irq_enqueued {
                cpu.borrow_mut().irq_enqueued = false;
                let vector = if cpu.borrow().reg.p.e {
                    IRQ_VECTOR_E
                } else {
                    IRQ_VECTOR
                };
                yield_ticks!(cpu, CPU::interrupt(cpu.clone(), vector));
            }

            // TODO: I HATE this. Somehow want to yield ticks if we're doing a sync, but not for events.
            // But I think it's good enough if we just attach ticks_run to whatever this function yield (like yield_all).
            // In fact, this is wrong, as we're returning from the device generator without flushing our cycles.
            yield (YieldReason::FinishedInstruction(Device::CPU), 0);
        }
    }

    fn progress_pc(&mut self, bytes: i16) {
        self.reg
            .pc
            .set_lo16(self.reg.pc.lo16().wrapping_add_signed(bytes));
    }

    fn on_scanline_start<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<()> + 'a {
        // TODO: Possibly resync all threads at this time
        // TODO: How expensive are nested generators for function calls? Are macros much cheaper
        // by avoiding the nesting? If so, it would be worth finding a middle-ground.
        #[coroutine]
        move || {
            dummy_yield!();
            // TODO: This is required to prevent deadlocks on the frontend. Consider a more elegent way
            // to resynchronize.
            yield YieldReason::Sync(Device::SMP);
            yield YieldReason::Sync(Device::PPU);
            // TODO: Overscan mode
            if cpu.borrow().ppu_counter.borrow().scanline < 225 {
                cpu.borrow_mut().hdma_setup_triggered_this_line = false;
                cpu.borrow_mut().hdma_triggered_this_line = false;
            }
            if cpu.borrow().ppu_counter.borrow().scanline == 0 {
                let hdmas_enqueued = cpu.borrow_mut().hdmas_enqueued.take();
                if let Some(hdmas_enqueued) = hdmas_enqueued {
                    cpu.borrow_mut().hdmas_ongoing = Some(hdmas_enqueued);
                }
                let hdmas_ongoing = cpu.borrow().hdmas_ongoing;
                if let Some(hdmas_ongoing) = hdmas_ongoing {
                    for channel_i in 0..=7 {
                        if (hdmas_ongoing >> channel_i) & 1 != 1 {
                            continue;
                        }
                        let table_base_addr =
                            cpu.borrow().io_reg.dma_channels[channel_i].addr.0.lo16();
                        cpu.borrow_mut().io_reg.dma_channels[channel_i]
                            .hdma_table_curr_addr
                            .0 = table_base_addr;
                        cpu.borrow_mut().io_reg.dma_channels[channel_i]
                            .hdma_line_counter
                            .0 = 0;
                    }
                }
                cpu.borrow_mut().hdmas_complete_this_frame.fill(false);
                cpu.borrow_mut().do_hdmas_this_line.fill(true);
            } else if cpu.borrow().ppu_counter.borrow().scanline == 225 {
                // TODO: Overscan mode
                if cpu.borrow().io_reg.interrupt_control.vblank_nmi_enable() {
                    cpu.borrow_mut().nmi_enqueued = true;
                }
                let frame = cpu.borrow().bus.borrow().debug_get_frame();
                cpu.borrow_mut().debug_frame = Some(frame);
                // DO NOT SUBMIT
                let now = Instant::now();
                let delta = cpu
                    .borrow()
                    .previous_frame_instant
                    .map(|previous| (now - previous).as_secs_f32());
                let prev = cpu.borrow().previous_frame_instant;
                if let Some(previous_frame_instant) = prev {
                    let elapsed = previous_frame_instant.elapsed();
                    cpu.borrow_mut()
                        .frame_history
                        .on_new_frame(elapsed.as_secs_f64(), delta);
                }
                cpu.borrow_mut().previous_frame_instant = Some(now);
                let print_timer = cpu.borrow().print_timer;
                cpu.borrow_mut().print_timer = (print_timer + 1) % 120;
                if print_timer == 0 {
                    log::debug!("{}", cpu.borrow().frame_history.mean_frame_time());
                }
                yield YieldReason::FrameReady;
            }
        }
    }

    fn step<'a>(cpu: Rc<RefCell<CPU>>, n_clocks: u64) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            let (aligned_clocks, poll_offset) = {
                let mut cpu = cpu.borrow_mut();
                cpu.ticks_run += n_clocks;
                let ticks_mod_4 = cpu.ticks_mod_4;
                cpu.ticks_mod_4 = ticks_mod_4.wrapping_add(n_clocks as u8) % 4;
                let aligned_clocks = n_clocks.saturating_sub((4 - ticks_mod_4 as u64) % 2);
                // If the first of two PPU ticks for this poll cycle has already happened
                // (i.e. on clocks 1 and 2 of the poll cycle) then the next PPU tick should not trigger a poll.
                let poll_offset = (ticks_mod_4 == 1 || ticks_mod_4 == 2) as u64;
                (aligned_clocks, poll_offset)
            };
            // We tick the PPU counter by 2's for efficiency; all PPU timing events occur on even clocks.
            for ppu_tick in 0..((aligned_clocks + 1) / 2) {
                let scanline = yield_all!(PpuCounter::tick(cpu.borrow().ppu_counter.clone(), 2));
                if scanline {
                    yield_all!(CPU::on_scanline_start(cpu.clone()));
                }
                if ppu_tick % 2 == poll_offset {
                    let mut cpu = cpu.borrow_mut();
                    let irq_mode = cpu.io_reg.interrupt_control.h_v_irq();
                    let v_scan_count = cpu.io_reg.v_scan_count.timer_value();
                    let h_scan_count = cpu.io_reg.h_scan_count.timer_value();
                    let h_dot = cpu.ppu_counter.borrow().h_ticks / 4;
                    let scanline = cpu.ppu_counter.borrow().scanline;
                    cpu.irq_enqueued |= match irq_mode {
                        0 => false,
                        1 => h_dot == h_scan_count,
                        2 => scanline == v_scan_count && h_dot == 0,
                        3 | _ => scanline == v_scan_count && h_dot == h_scan_count,
                    };
                }
            }
            let mut cpu = cpu.borrow_mut();
            if cpu.ppu_counter.borrow().h_ticks >= 1104 && !cpu.hdma_triggered_this_line {
                cpu.hdma_triggered_this_line = true;
                cpu.hdma_pending_this_line = true;
            }
        }
    }

    // TODO: Add idle cycles in various places where IO cycles are involved
    // See https://archive.org/details/vl65c816datasheetvlsi1988ocrbm/page/n9/mode/1up
    fn idle<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<()> + 'a {
        CPU::step(cpu, 6)
    }

    fn region_cycles(&self, addr: u24) -> u64 {
        // https://problemkaputt.de/fullsnes.htm#snesmemorymap
        // Succinct conditionals from Higan
        let addr = addr.raw();
        if addr & 0x408000 != 0 {
            if addr & 0x800000 != 0 {
                // 80-bf:8000-ffff; c0-ff:0000-ffff
                self.io_reg.waitstate_control.high_rom_cycles()
            } else {
                // 00-3f:8000-ffff; 40-7f:0000-ffff
                8
            }
        } else if addr.wrapping_add(0x6000) & 0x4000 != 0 {
            // 00-3f,80-bf:0000-1fff,6000-7fff
            8
        } else if addr.wrapping_sub(0x4000) & 0x7e00 != 0 {
            // 00-3f,80-bf:2000-3fff,4200-5fff
            6
        } else {
            // 00-3f,80-bf:4000-41ff
            12
        }
    }

    pub fn io_peak(&self, addr: u24) -> u8 {
        // TODO
        0
    }

    pub fn io_read(&mut self, addr: u24) -> u8 {
        match addr.lo16() {
            // TODO: Remove debugging value
            // This has bit6 set to simulate some weird open bus behavior for testing
            // 0x4210 => 0xC0,
            0x4210 => {
                static mut DEBUG_LATCH: bool = false;
                unsafe {
                    DEBUG_LATCH = !DEBUG_LATCH;
                }
                if unsafe { DEBUG_LATCH } {
                    0x80
                } else {
                    0x0
                }
            } // 0xC0,
            0x4211 => 0, // TODO: IRQ stuff
            // TODO: Remove debugging value
            0x4212 => {
                static mut DEBUG_LATCH: bool = false;
                unsafe {
                    DEBUG_LATCH = !DEBUG_LATCH;
                }
                if unsafe { DEBUG_LATCH } {
                    0x80
                } else {
                    0x0
                }
            } // 0xC0,
            0x4218..=0x421F => {
                let reg_off = addr.lo16() as usize - 0x4218;
                let controller_state = self.controller_states[reg_off / 4];
                (controller_state >> ((reg_off % 2) * 8)) as u8
            }
            0x4300..=0x437A => {
                let channel_index = (addr.0 as usize >> 4) & 0xF;
                let channel_regs = self.io_reg.dma_channels[channel_index];
                match (addr.0 as usize) & 0xF {
                    0x0 => channel_regs.setup.0,
                    0x1 => channel_regs.io_reg,
                    0x2 => channel_regs.addr.lo_byte(),
                    0x3 => channel_regs.addr.hi_byte(),
                    0x4 => channel_regs.addr.bank_byte(),
                    0x5 => channel_regs.indirect_addr_or_byte_count.lo_byte(),
                    0x6 => channel_regs.indirect_addr_or_byte_count.hi_byte(),
                    0x7 => channel_regs.indirect_addr_or_byte_count.bank_byte(),
                    0x8 => channel_regs.hdma_table_curr_addr.lo_byte(),
                    0x9 => channel_regs.hdma_table_curr_addr.hi_byte(),
                    0xA => channel_regs.hdma_line_counter.0,
                    0xB | 0xF => channel_regs.unused_byte,
                    _ => 0, // TODO: Open bus (maybe handle this in Bus)
                }
            }
            _ => {
                log::debug!("TODO: CPU IO read {addr}");
                0
            } // TODO: Remove this fallback
            _ => panic!("Invalid IO read of CPU at {addr:#06X}"),
        }
    }

    pub fn io_write(&mut self, addr: u24, data: u8) {
        match addr.lo16() {
            0x4200 => self.io_reg.interrupt_control.0 = data,
            0x420B => {
                self.dmas_enqueued = Some(data);
            }
            0x420C => {
                // log::debug!("TODO: Start HDMA transfer: {addr} {data:02X}");
                self.hdmas_enqueued = Some(data);
            }
            0x4300..=0x437A => {
                let channel_index = (addr.0 as usize >> 4) & 0xF;
                let channel_regs = &mut self.io_reg.dma_channels[channel_index];
                match (addr.0 as usize) & 0xF {
                    0x0 => channel_regs.setup.0 = data,
                    0x1 => channel_regs.io_reg = data,
                    0x2 => channel_regs.addr.set_lo_byte(data),
                    0x3 => channel_regs.addr.set_hi_byte(data),
                    0x4 => channel_regs.addr.set_bank_byte(data),
                    0x5 => channel_regs.indirect_addr_or_byte_count.set_lo_byte(data),
                    0x6 => channel_regs.indirect_addr_or_byte_count.set_hi_byte(data),
                    0x7 => channel_regs.indirect_addr_or_byte_count.set_bank_byte(data),
                    0x8 => channel_regs.hdma_table_curr_addr.set_lo_byte(data),
                    0x9 => channel_regs.hdma_table_curr_addr.set_hi_byte(data),
                    0xA => channel_regs.hdma_line_counter.0 = data,
                    0xB | 0xF => channel_regs.unused_byte = data,
                    _ => {}
                }
            }
            0x420D => self.io_reg.waitstate_control.0 = data,
            _ => log::debug!("TODO: CPU IO write {addr}: {data:02X}"), // TODO: Remove this fallback
            _ => panic!("Invalid IO write of CPU at {addr:#06X}"),
        }
    }

    fn run_dma<'a>(cpu: Rc<RefCell<CPU>>, channel_mask: u8) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            for channel_i in 0..=7 {
                if (channel_mask >> channel_i) & 1 != 1 {
                    continue;
                }
                // TODO: Ban wram-to-wram
                let channel_regs = cpu.borrow().io_reg.dma_channels[channel_i];
                let mut cpu_addr = channel_regs.addr.0;
                let n_bytes = channel_regs.indirect_addr_or_byte_count.dma_byte_count();
                let io_to_cpu = channel_regs.setup.transfer_direction();
                let cpu_addr_step = channel_regs.setup.cpu_addr_step();
                let io_reg_offsets = channel_regs.setup.gpdma_io_reg_offsets();
                let unit_size = io_reg_offsets.len();
                let io_reg_base = channel_regs.io_reg;
                for i in 0..n_bytes {
                    let io_reg = io_reg_base.wrapping_add(io_reg_offsets[i as usize % unit_size]);
                    let io_reg_addr = u24(0x2100 | io_reg as u32);
                    if io_to_cpu {
                        let data = yield_all!(CPU::read_u8(cpu.clone(), io_reg_addr));
                        yield_all!(CPU::write_u8(cpu.clone(), cpu_addr, data));
                    } else {
                        let data = yield_all!(CPU::read_u8(cpu.clone(), cpu_addr));
                        yield_all!(CPU::write_u8(cpu.clone(), io_reg_addr, data));
                    }
                    cpu_addr = cpu_addr.wrapping_add_signed(cpu_addr_step);
                }
            }
        }
    }

    fn setup_hdma<'a>(cpu: Rc<RefCell<CPU>>, channel_mask: u8) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            for channel_i in 0..=7 {
                if (channel_mask >> channel_i) & 1 != 1 {
                    continue;
                }
                let channel_regs = cpu.borrow().io_reg.dma_channels[channel_i];
                let table_base_addr = channel_regs.addr.0;
                let indirect = channel_regs.setup.hdma_addr_mode();
                if channel_regs.hdma_line_counter.line_count() == 0 {
                    cpu.borrow_mut().do_hdmas_this_line[channel_i] = true;
                    let table_off = cpu.borrow().io_reg.dma_channels[channel_i]
                        .hdma_table_curr_addr
                        .0 as u32;
                    let addr = u24(((table_base_addr.bank() as u32) << 16) | table_off);
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .hdma_table_curr_addr
                        .0 = channel_regs.hdma_table_curr_addr.0.wrapping_add(1);
                    let new_line_count = yield_all!(CPU::read_u8(cpu.clone(), addr));
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .hdma_line_counter
                        .set_line_count(new_line_count);
                    // Reached terminating 0x00 in HDMA table
                    if new_line_count == 0x00 {
                        cpu.borrow_mut().hdmas_complete_this_frame[channel_i] = true;
                    }
                }
                if indirect {
                    let table_off = cpu.borrow().io_reg.dma_channels[channel_i]
                        .hdma_table_curr_addr
                        .0 as u32;
                    let addr = u24(((table_base_addr.bank() as u32) << 16) | table_off);
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .hdma_table_curr_addr
                        .0 = channel_regs.hdma_table_curr_addr.0.wrapping_add(1);
                    let data = yield_all!(CPU::read_u8(cpu.clone(), addr));
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .indirect_addr_or_byte_count
                        .set_hi_byte(data);
                    // TODO: Data might be shifted into the lo16 of this register, so this low byte might be the old high byte
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .indirect_addr_or_byte_count
                        .set_lo_byte(0);
                    let table_off = cpu.borrow().io_reg.dma_channels[channel_i]
                        .hdma_table_curr_addr
                        .0 as u32;
                    let addr = u24(((table_base_addr.bank() as u32) << 16) | table_off);
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .hdma_table_curr_addr
                        .0 = channel_regs.hdma_table_curr_addr.0.wrapping_add(1);
                    let data = yield_all!(CPU::read_u8(cpu.clone(), addr));
                    cpu.borrow_mut().io_reg.dma_channels[channel_i]
                        .indirect_addr_or_byte_count
                        .set_hi_byte(data);
                }
            }
        }
    }

    fn run_hdma<'a>(cpu: Rc<RefCell<CPU>>, channel_mask: u8) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            for channel_i in 0..=7 {
                if (channel_mask >> channel_i) & 1 != 1
                    || cpu.borrow().hdmas_complete_this_frame[channel_i]
                {
                    continue;
                }
                let line_counter = cpu.borrow().io_reg.dma_channels[channel_i]
                    .hdma_line_counter
                    .line_count();
                cpu.borrow_mut().io_reg.dma_channels[channel_i]
                    .hdma_line_counter
                    .set_line_count(line_counter.wrapping_sub(1));
                if !cpu.borrow_mut().do_hdmas_this_line[channel_i] {
                    continue;
                }
                // TODO: Ban wram-to-wram
                let channel_regs = cpu.borrow().io_reg.dma_channels[channel_i];
                let table_base_addr = channel_regs.addr.0;
                let indirect = channel_regs.setup.hdma_addr_mode();
                let indirect_addr = channel_regs.indirect_addr_or_byte_count.0;
                let io_to_cpu = channel_regs.setup.transfer_direction();
                let io_reg_base = channel_regs.io_reg;
                let io_reg_offsets = channel_regs.setup.hdma_io_reg_offsets();
                for io_reg_offset in io_reg_offsets {
                    let io_reg = io_reg_base.wrapping_add(*io_reg_offset);
                    let io_reg_addr = u24(0x2100 | io_reg as u32);
                    let cpu_addr = if indirect {
                        cpu.borrow_mut().io_reg.dma_channels[channel_i]
                            .indirect_addr_or_byte_count
                            .0 = indirect_addr.wrapping_add_signed(1);
                        indirect_addr
                    } else {
                        let table_off = cpu.borrow().io_reg.dma_channels[channel_i]
                            .hdma_table_curr_addr
                            .0 as u32;
                        cpu.borrow_mut().io_reg.dma_channels[channel_i]
                            .hdma_table_curr_addr
                            .0 = (table_off as u16).wrapping_add(1);
                        u24(((table_base_addr.bank() as u32) << 16) | table_off)
                    };
                    if io_to_cpu {
                        let data = yield_all!(CPU::read_u8(cpu.clone(), io_reg_addr));
                        yield_all!(CPU::write_u8(cpu.clone(), cpu_addr, data));
                    } else {
                        let data = yield_all!(CPU::read_u8(cpu.clone(), cpu_addr));
                        yield_all!(CPU::write_u8(cpu.clone(), io_reg_addr, data));
                    }
                }
                let repeat = (cpu.borrow().io_reg.dma_channels[channel_i]
                    .hdma_line_counter
                    .line_count()
                    >> 7)
                    & 1
                    == 1;
                cpu.borrow_mut().do_hdmas_this_line[channel_i] = repeat;
            }
        }
    }

    fn read_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u8> + 'a {
        #[coroutine]
        move || {
            let region_cycles = cpu.borrow().region_cycles(addr);
            // yield_all!(CPU::step(cpu.clone(), region_cycles - 4));
            let n_clocks = region_cycles - 4;
            let (aligned_clocks, poll_offset) = {
                let mut cpu = cpu.borrow_mut();
                cpu.ticks_run += n_clocks;
                let ticks_mod_4 = cpu.ticks_mod_4;
                cpu.ticks_mod_4 = ticks_mod_4.wrapping_add(n_clocks as u8) % 4;
                let aligned_clocks = n_clocks.saturating_sub((4 - ticks_mod_4 as u64) % 2);
                // If the first of two PPU ticks for this poll cycle has already happened
                // (i.e. on clocks 1 and 2 of the poll cycle) then the next PPU tick should not trigger a poll.
                let poll_offset = (ticks_mod_4 == 1 || ticks_mod_4 == 2) as u64;
                (aligned_clocks, poll_offset)
            };
            // We tick the PPU counter by 2's for efficiency; all PPU timing events occur on even clocks.
            for ppu_tick in 0..((aligned_clocks + 1) / 2) {
                let scanline = yield_all!(PpuCounter::tick(cpu.borrow().ppu_counter.clone(), 2));
                if scanline {
                    yield_all!(CPU::on_scanline_start(cpu.clone()));
                }
                if ppu_tick % 2 == poll_offset {
                    let mut cpu = cpu.borrow_mut();
                    let irq_mode = cpu.io_reg.interrupt_control.h_v_irq();
                    let v_scan_count = cpu.io_reg.v_scan_count.timer_value();
                    let h_scan_count = cpu.io_reg.h_scan_count.timer_value();
                    let h_dot = cpu.ppu_counter.borrow().h_ticks / 4;
                    let scanline = cpu.ppu_counter.borrow().scanline;
                    cpu.irq_enqueued |= match irq_mode {
                        0 => false,
                        1 => h_dot == h_scan_count,
                        2 => scanline == v_scan_count && h_dot == 0,
                        3 | _ => scanline == v_scan_count && h_dot == h_scan_count,
                    };
                }
            }
            {
                let mut cpu = cpu.borrow_mut();
                if cpu.ppu_counter.borrow().h_ticks >= 1104 && !cpu.hdma_triggered_this_line {
                    cpu.hdma_triggered_this_line = true;
                    cpu.hdma_pending_this_line = true;
                }
            }
            let data = yield_all!(Bus::read_u8(cpu.borrow_mut().bus.clone(), addr));
            yield_all!(CPU::step(cpu.clone(), 4));
            data
        }
    }

    fn read_u16<'a>(cpu: Rc<RefCell<CPU>>, addr: u24) -> impl Yieldable<u16> + 'a {
        #[coroutine]
        move || {
            let lo = yield_all!(CPU::read_u8(cpu.clone(), addr)) as u16;
            // let hi = yield_all!(CPU::read_u8(cpu.clone(), addr + 1u32)) as u16;
            // TODO: DO NOT SUBMIT: Not sure...
            let hi = yield_all!(CPU::read_u8(cpu.clone(), addr.wrapping_add_lo16(1))) as u16;
            (hi << 8) | lo
        }
    }

    fn write_u8<'a>(cpu: Rc<RefCell<CPU>>, addr: u24, data: u8) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            let region_cycles = cpu.borrow().region_cycles(addr);
            yield_all!(Bus::write_u8(cpu.borrow_mut().bus.clone(), addr, data));
            yield_all!(CPU::step(cpu.clone(), region_cycles));
        }
    }

    fn read_pointer<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl Yieldable<u16> + 'a {
        #[coroutine]
        move || {
            let mut data = yield_all!(CPU::read_u8(cpu.clone(), pointer.addr)) as u16;
            if pointer.long {
                data |= (yield_all!(CPU::read_u8(cpu.clone(), pointer.addr + 1u32)) as u16) << 8;
                // TODO: DO NOT SUBMIT: Not sure...
                // data |= (yield_all!(CPU::read_u8(cpu.clone(), pointer.addr.wrapping_add_lo16(1)))
                //     as u16)
                //     << 8;
            }
            data
        }
    }

    fn write_pointer<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
        data: u16,
    ) -> impl Yieldable<()> + 'a {
        #[coroutine]
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
        #[coroutine]
        move || {
            yield_all!(CPU::read_u8(
                cpu.clone(),
                CPU::direct_addr(cpu.clone(), addr)
            ))
        }
    }

    // DO NOT SUBMIT: Stuff like this can OBVIOUSLY be modified to take &self
    fn bank_addr(cpu: Rc<RefCell<CPU>>, addr: u24) -> u24 {
        u24((cpu.borrow().reg.b as u32) << 16) | addr
    }

    fn stack_pull_u8<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u8> + 'a {
        // TODO: Does 0x100 ever get used, namely in emulation mode?
        #[coroutine]
        move || {
            let sp = cpu.borrow().reg.sp;
            cpu.borrow_mut().reg.sp = sp.wrapping_add(1);
            let val = yield_all!(CPU::read_u8(cpu.clone(), u24(cpu.borrow().reg.sp as u32)));
            val
        }
    }

    fn stack_pull_u16<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        #[coroutine]
        move || {
            let lo = yield_all!(CPU::stack_pull_u8(cpu.clone()));
            let hi = yield_all!(CPU::stack_pull_u8(cpu.clone()));
            ((hi as u16) << 8) | lo as u16
        }
    }

    // Calls either the _u8 or _u16 variant of stack_pull, depending on the X flag
    fn stack_pull_x<'a>(cpu: Rc<RefCell<CPU>>) -> impl Yieldable<u16> + 'a {
        #[coroutine]
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
        #[coroutine]
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
        #[coroutine]
        move || {
            yield_all!(CPU::write_u8(
                cpu.clone(),
                u24(cpu.borrow().reg.sp as u32),
                data
            ));
            let sp = cpu.borrow().reg.sp;
            cpu.borrow_mut().reg.sp = sp.wrapping_sub(1);
        }
    }

    fn stack_push_u16<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::stack_push_u8(cpu.clone(), (data >> 8) as u8));
            yield_all!(CPU::stack_push_u8(cpu.clone(), (data & 0xFF) as u8));
        }
    }

    // Calls either the _u8 or _u16 variant of stack_push, depending on the X flag
    fn stack_push_x<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            if cpu.borrow().reg.p.x_or_b {
                yield_all!(CPU::stack_push_u8(cpu, data as u8));
            } else {
                yield_all!(CPU::stack_push_u16(cpu, data));
            }
        }
    }

    // Calls either the _u8 or _u16 variant of stack_push, depending on the M flag
    fn stack_push_m<'a>(cpu: Rc<RefCell<CPU>>, data: u16) -> impl Yieldable<()> + 'a {
        #[coroutine]
        move || {
            if cpu.borrow().reg.p.m {
                yield_all!(CPU::stack_push_u8(cpu, data as u8));
            } else {
                yield_all!(CPU::stack_push_u16(cpu, data));
            }
        }
    }

    // Addressing modes:

    fn immediate<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let addr = cpu.borrow().reg.pc;
            cpu.borrow_mut().progress_pc(if long { 2 } else { 1 });
            Pointer { addr, long }
        }
    }

    // TODO: Reduce code duplication across these three
    fn direct<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let direct_addr = u24(fetch!(cpu) as u32);
            let addr = CPU::direct_addr(cpu.clone(), direct_addr);
            Pointer { addr, long }
        }
    }

    fn direct_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let direct_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.get_x() as u32);
            let addr = CPU::direct_addr(cpu.clone(), direct_addr);
            Pointer { addr, long }
        }
    }

    fn direct_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let direct_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.get_y() as u32);
            let addr = CPU::direct_addr(cpu.clone(), direct_addr);
            Pointer { addr, long }
        }
    }

    // TODO: Reduce code duplication across these three
    fn absolute<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                // TODO: NOTE: This is a critical bug fix
                CPU::bank_addr(cpu.clone(), u24(((fetch!(cpu) as u32) << 8) | addr_lo))
            };
            Pointer { addr, long }
        }
    }

    fn absolute_x<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                // TODO: NOTE: This is a critical bug fix
                CPU::bank_addr(
                    cpu.clone(),
                    u24((((fetch!(cpu) as u32) << 8) | addr_lo) + cpu.borrow().reg.get_x() as u32),
                )
            };
            Pointer { addr, long }
        }
    }

    fn absolute_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let addr = {
                let addr_lo = fetch!(cpu) as u32;
                // TODO: NOTE: This is a critical bug fix
                CPU::bank_addr(
                    cpu.clone(),
                    u24((((fetch!(cpu) as u32) << 8) | addr_lo) + cpu.borrow().reg.get_y() as u32),
                )
            };
            Pointer { addr, long }
        }
    }

    fn absolute_long<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
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
        #[coroutine]
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

    fn absolute_indirect<'a>(cpu: Rc<RefCell<CPU>>, _: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let indirect_addr = {
                let addr_lo = fetch!(cpu) as u32;
                u24(((fetch!(cpu) as u32) << 8) | addr_lo)
            };
            let addr = u24({
                let addr_lo = yield_all!(CPU::read_u8(cpu.clone(), indirect_addr)) as u32;
                ((yield_all!(CPU::read_u8(cpu.clone(), indirect_addr + 1u32)) as u32) << 8)
                    | addr_lo
            });
            Pointer { addr, long: true }
        }
    }

    fn absolute_indirect_long<'a>(
        cpu: Rc<RefCell<CPU>>,
        long: bool,
    ) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let indirect_addr = {
                let addr_lo = fetch!(cpu) as u32;
                u24(((fetch!(cpu) as u32) << 8) | addr_lo)
            };
            let addr = u24({
                let addr_lo = yield_all!(CPU::read_u8(cpu.clone(), indirect_addr)) as u32;
                let addr_mid = yield_all!(CPU::read_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                ((yield_all!(CPU::read_u8(cpu.clone(), indirect_addr + 2u32)) as u32) << 8)
                    | (addr_mid << 8)
                    | addr_lo
            });
            Pointer { addr, long }
        }
    }

    fn absolute_indirect_indexed<'a>(
        cpu: Rc<RefCell<CPU>>,
        _: bool,
    ) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let indirect_addr = {
                let pb = cpu.borrow().reg.pc.bank();
                let addr_lo = fetch!(cpu) as u32;
                let lo16 = (((fetch!(cpu) as u32) << 8) | addr_lo)
                    .wrapping_add(cpu.borrow().reg.get_x() as u32);
                u24(((pb as u32) << 16) | lo16)
            };
            // TODO: Use read_u16 to simplify some of these addressing modes
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
        #[coroutine]
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                // TODO: DO NOT SUBMIT: Maybe change this?
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                // u24((hi << 8) | lo)
                // TODO: NOTE: This is a critical bug fix
                CPU::bank_addr(cpu.clone(), u24((hi << 8) | lo))
            };
            Pointer { addr, long }
        }
    }

    fn indexed_indirect<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let offset = cpu.borrow().reg.get_x() as u32;
            let indirect_addr = u24(fetch!(cpu) as u32 + offset);
            let bank_addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(
                    cpu.clone(),
                    indirect_addr.wrapping_add_lo16(1)
                )) as u32;
                u24((hi << 8) | lo)
            };
            // TODO: NOTE: This is a critical bug fix
            let addr = CPU::bank_addr(cpu.clone(), bank_addr);
            Pointer { addr, long }
        }
    }

    fn indirect_indexed<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
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
        #[coroutine]
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                let bank =
                    yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 2u32)) as u32;
                u24((bank << 16) | (hi << 8) | lo)
            };
            Pointer { addr, long }
        }
    }

    fn indirect_long_y<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
        move || {
            let indirect_addr = u24(fetch!(cpu) as u32);
            // TODO: Check this logic; namely, the overflow of adding y
            let addr = {
                let lo = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr)) as u32;
                let hi = yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 1u32)) as u32;
                let bank =
                    yield_all!(CPU::read_direct_u8(cpu.clone(), indirect_addr + 2u32)) as u32;
                u24((bank << 16) | (hi << 8) | lo)
            } + cpu.borrow().reg.get_y() as u32;
            Pointer { addr, long }
        }
    }

    fn stack_relative<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl Yieldable<Pointer> + 'a {
        #[coroutine]
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
        #[coroutine]
        move || {
            let stack_addr = u24(fetch!(cpu) as u32 + cpu.borrow().reg.sp as u32);
            let addr_lo = yield_all!(CPU::read_u8(cpu.clone(), stack_addr)) as u32;
            // TODO: DO NOT SUBMIT: Not sure
            let addr_hi = yield_all!(CPU::read_u8(cpu.clone(), stack_addr + 1u32)) as u32;
            let bank_addr = ((addr_hi << 8) | addr_lo) + cpu.borrow().reg.get_y() as u32;
            let addr = CPU::bank_addr(cpu.clone(), u24(bank_addr));
            Pointer { addr, long }
        }
    }

    // Instructions:

    fn ora<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let val = cpu.borrow().reg.get_a() | data;
            cpu.borrow_mut().reg.set_a(val);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            cpu.borrow_mut().reg.p.n = (val >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = val == 0;
        }
    }

    fn eor<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let val = cpu.borrow().reg.get_a() ^ data;
            cpu.borrow_mut().reg.set_a(val);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            cpu.borrow_mut().reg.p.n = (val >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = val == 0;
        }
    }

    fn and<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let val = cpu.borrow().reg.get_a() & data;
            cpu.borrow_mut().reg.set_a(val);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            cpu.borrow_mut().reg.p.n = (val >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = val == 0;
        }
    }

    fn bit_immediate<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
    ) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let zero = (cpu.borrow().reg.get_a() & data) == 0;
            cpu.borrow_mut().reg.p.z = zero;
        }
    }

    fn bit<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let zero = (cpu.borrow().reg.get_a() & data) == 0;
            cpu.borrow_mut().reg.p.z = zero;
            let msb = n_bits!(cpu, m) - 1;
            cpu.borrow_mut().reg.p.n = (data >> msb) & 1 == 1;
            cpu.borrow_mut().reg.p.v = (data >> (msb - 1)) & 1 == 1;
        }
    }

    fn trb<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let a = cpu.borrow().reg.get_a();
            let zero = (cpu.borrow().reg.get_a() & data) == 0;
            cpu.borrow_mut().reg.p.z = zero;
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data & !a));
        }
    }

    fn tsb<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let a = cpu.borrow().reg.get_a();
            let zero = (cpu.borrow().reg.get_a() & data) == 0;
            cpu.borrow_mut().reg.p.z = zero;
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data | a));
        }
    }

    // TODO: Make a macro for these flag instructions?
    fn clc<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.c = false;
        }
    }

    fn cli<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.i = false;
        }
    }

    fn cld<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.d = false;
        }
    }

    fn clv<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.v = false;
        }
    }

    fn sec<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.c = true;
        }
    }

    fn sei<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.i = true;
        }
    }

    fn sed<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            cpu.borrow_mut().reg.p.d = true;
        }
    }

    fn rep<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let p = cpu.borrow().reg.get_p();
            cpu.borrow_mut().reg.set_p(p & !data as u8);
        }
    }

    fn sep<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let p = cpu.borrow().reg.get_p();
            cpu.borrow_mut().reg.set_p(p | data as u8);
        }
    }

    fn xba<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            yield_all!(CPU::idle(cpu.clone()));
            let a = cpu.borrow().reg.a;
            let (hi, lo) = (a >> 8, a & 0xFF);
            let result = (lo << 8) | hi;
            cpu.borrow_mut().reg.a = result;
            cpu.borrow_mut().reg.p.n = (hi >> 7) == 1;
            cpu.borrow_mut().reg.p.z = hi == 0;
        }
    }

    fn xce<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.swap_carry_emulation_flags();
        }
    }

    // TODO: Factor out load instructions (macro?)
    fn lda<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            cpu.borrow_mut().reg.set_a(data);
            let n_bits = if cpu.borrow().reg.p.m { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn ldx<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            cpu.borrow_mut().reg.set_x(data);
            // TODO: Need to take into account e flag; probably factor out (maybe to
            // StatusRegister)
            let n_bits = if cpu.borrow().reg.p.x_or_b { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn ldy<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            cpu.borrow_mut().reg.set_y(data);
            let n_bits = if cpu.borrow().reg.p.x_or_b { 8 } else { 16 };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn stz<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, 0));
        }
    }

    fn sta<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = cpu.borrow().reg.get_a();
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
        }
    }

    fn stx<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = cpu.borrow().reg.get_x();
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
        }
    }

    fn sty<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = cpu.borrow().reg.get_y();
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
        }
    }

    fn transfer_a_sp<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            let a = cpu.borrow().reg.a;
            if cpu.borrow().reg.p.e {
                cpu.borrow_mut().reg.sp = (1 << 8) | (a & 0xFF);
            } else {
                cpu.borrow_mut().reg.sp = a;
            }
        }
    }

    fn transfer_x_sp<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            let x = cpu.borrow().reg.x;
            if cpu.borrow().reg.p.e {
                cpu.borrow_mut().reg.sp &= 0xFF00;
                cpu.borrow_mut().reg.sp |= x & 0xFF;
            } else {
                cpu.borrow_mut().reg.sp = x;
            }
        }
    }

    fn block_transfer_op<'a>(
        cpu: Rc<RefCell<CPU>>,
        increment: bool,
    ) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let dst_bank = fetch!(cpu);
            let src_bank = fetch!(cpu);
            cpu.borrow_mut().reg.b = dst_bank;
            let src_addr = u24(((src_bank as u32) << 16) | cpu.borrow().reg.x as u32);
            let dst_addr = u24(((dst_bank as u32) << 16) | cpu.borrow().reg.y as u32);
            let data = yield_all!(CPU::read_u8(cpu.clone(), src_addr));
            yield_all!(CPU::write_u8(cpu.clone(), dst_addr, data));
            yield_all!(CPU::idle(cpu.clone()));
            if increment {
                let x = cpu.borrow().reg.get_x();
                cpu.borrow_mut().reg.set_x(x.wrapping_add(1));
                let y = cpu.borrow().reg.get_y();
                cpu.borrow_mut().reg.set_y(y.wrapping_add(1));
            } else {
                let x = cpu.borrow().reg.get_x();
                cpu.borrow_mut().reg.set_x(x.wrapping_sub(1));
                let y = cpu.borrow().reg.get_y();
                cpu.borrow_mut().reg.set_y(y.wrapping_sub(1));
            }
            yield_all!(CPU::idle(cpu.clone()));
            // The entire 16 bits of A is decremented, regardless of the M and E flags
            let a = cpu.borrow().reg.a;
            cpu.borrow_mut().reg.a = a.wrapping_sub(1);
            if cpu.borrow().reg.a != 0xFFFF {
                // If the transfer isn't complete, return the PC to the beginning of this instruction
                cpu.borrow_mut().progress_pc(-3);
            }
        }
    }

    fn mvp<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        CPU::block_transfer_op(cpu, false)
    }

    fn mvn<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        CPU::block_transfer_op(cpu, true)
    }

    // TODO: Factor out increment and decrement instructions
    fn ina<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
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
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn inx<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
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
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn iny<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
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
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn inc<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer)).wrapping_add(1);
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, data));
            // TODO: Simplify some of these functions with n_bits!()
            let n_bits = if cpu.borrow().reg.p.m || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };
            // TODO: Factor out these flag updates
            cpu.borrow_mut().reg.p.n = (data >> (n_bits - 1)) == 1;
            cpu.borrow_mut().reg.p.z = (data & ((1u32 << n_bits) - 1) as u16) == 0;
        }
    }

    fn dea<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_a().wrapping_sub(1);
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

    fn dex<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_x().wrapping_sub(1);
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

    fn dey<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_y().wrapping_sub(1);
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

    fn dec<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer)).wrapping_sub(1);
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
    ) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let n_bits: u32 = if cpu.borrow().reg.p.m || cpu.borrow().reg.p.e {
                8
            } else {
                16
            };

            // TODO: Really need to check the 8- and 16-bit flag logic
            let data = {
                let mem_val = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
                if subtract {
                    if n_bits == 8 {
                        !(mem_val as i8 as i16) as u16
                    } else {
                        !(mem_val as i16) as u16
                    }
                } else {
                    mem_val
                }
            };

            let carry = cpu.borrow().reg.p.c as u16;
            let result = if cpu.borrow().reg.p.d {
                let temp = bcd_to_bin(cpu.borrow().reg.get_a())
                    .wrapping_add(bcd_to_bin(data))
                    .wrapping_add(carry as u16);
                let bcd_max = if n_bits == 8 { 100 } else { 10000 };
                cpu.borrow_mut().reg.p.c = temp >= bcd_max;
                // TODO: This sets V incorrectly; V seems to depend on the weird implementation of BCD adjustment
                bin_to_bcd(temp % bcd_max)
            } else {
                let temp = cpu.borrow().reg.get_a() as i32 + data as i32 + carry as i32;
                cpu.borrow_mut().reg.p.c = temp > ((1 << n_bits) - 1);
                temp as u16
            };

            let overflow =
                ((cpu.borrow().reg.get_a() ^ result) & (data ^ result) & (1 << (n_bits - 1))) != 0;
            cpu.borrow_mut().reg.p.v = overflow;
            cpu.borrow_mut().reg.set_a(result);
            let new_a = cpu.borrow_mut().reg.get_a();
            cpu.borrow_mut().reg.p.n = (new_a >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = new_a == 0;
        }
    }

    fn adc<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        return CPU::arithmetic_op(cpu, pointer, false);
    }

    fn sbc<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        return CPU::arithmetic_op(cpu, pointer, true);
    }

    fn compare_op<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
        reg_val: u16,
        flag: bool,
    ) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            // TODO: Really need to check the 8- and 16-bit flag logic

            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let result = reg_val as i32 - data as i32;

            let n_bits = if flag || cpu.borrow().reg.p.e { 8 } else { 16 };
            cpu.borrow_mut().reg.p.c = result >= 0;
            cpu.borrow_mut().reg.p.n = (result >> (n_bits - 1)) & 1 == 1;
            cpu.borrow_mut().reg.p.z = result == 0;
        }
    }

    fn cmp<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        let reg_val = cpu.borrow().reg.get_a();
        let flag = cpu.borrow().reg.p.m;
        return CPU::compare_op(cpu, pointer, reg_val, flag);
    }

    fn cpx<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        let reg_val = cpu.borrow().reg.get_x();
        let flag = cpu.borrow().reg.p.x_or_b;
        return CPU::compare_op(cpu, pointer, reg_val, flag);
    }

    fn cpy<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        let reg_val = cpu.borrow().reg.get_y();
        let flag = cpu.borrow().reg.p.x_or_b;
        return CPU::compare_op(cpu, pointer, reg_val, flag);
    }

    fn jmp<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.pc &= 0xFF_0000u32;
            cpu.borrow_mut().reg.pc |= pointer.addr.lo16();
        }
    }

    fn jml<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            cpu.borrow_mut().reg.pc = pointer.addr;
        }
    }

    fn jsr<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let return_pc = cpu.borrow().reg.pc.lo16();
            yield_all!(CPU::stack_push_u16(cpu.clone(), return_pc));
            cpu.borrow_mut().reg.pc &= 0xFF_0000u32;
            cpu.borrow_mut().reg.pc |= pointer.addr.lo16();
        }
    }

    fn jsl<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let return_pb = cpu.borrow().reg.pc.bank();
            let return_pc = cpu.borrow().reg.pc.lo16();
            yield_all!(CPU::stack_push_u8(cpu.clone(), return_pb));
            yield_all!(CPU::stack_push_u16(cpu.clone(), return_pc));
            cpu.borrow_mut().reg.pc = pointer.addr;
        }
    }

    fn interrupt<'a>(cpu: Rc<RefCell<CPU>>, vector: u24) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            if !cpu.borrow().reg.p.e {
                let return_pb = cpu.borrow().reg.pc.bank();
                yield_all!(CPU::stack_push_u8(cpu.clone(), return_pb));
            }
            let return_pc = cpu.borrow().reg.pc.lo16();
            yield_all!(CPU::stack_push_u16(cpu.clone(), return_pc));
            let p = cpu.borrow().reg.p.get();
            // TODO: If E flag, do some special stuff with Break flag
            yield_all!(CPU::stack_push_u8(cpu.clone(), p));
            cpu.borrow_mut().reg.p.i = true;
            cpu.borrow_mut().reg.pc = u24(yield_all!(CPU::read_u16(cpu.clone(), vector)) as u32);
        }
    }

    fn brk<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        let vector = if cpu.borrow().reg.p.e {
            // The emulation mode break vector is the same as its IRQ vector
            BRK_VECTOR_E
        } else {
            BRK_VECTOR
        };
        CPU::interrupt(cpu, vector)
    }

    // Rotate and shift instructions

    fn rotate_through_carry(cpu: Rc<RefCell<CPU>>, data: u16, left: bool) -> u16 {
        let n_bits = n_bits!(cpu, m);
        let leftmost_mask = 1 << (n_bits - 1);
        let (check_mask, carry_mask) = if left {
            (leftmost_mask, 0x01)
        } else {
            (0x01, leftmost_mask)
        };
        let old_carry = cpu.borrow().reg.p.c;
        cpu.borrow_mut().reg.p.c = (data & check_mask) != 0;
        let result = {
            let temp = if left { data << 1 } else { data >> 1 };
            if old_carry {
                temp | carry_mask
            } else {
                temp & !carry_mask
            }
        };
        cpu.borrow_mut().reg.p.n = (result >> (n_bits - 1)) == 1;
        cpu.borrow_mut().reg.p.z = result & ((1u32 << n_bits) - 1) as u16 == 0;
        result
    }

    fn rotate_op<'a>(
        cpu: Rc<RefCell<CPU>>,
        pointer: Pointer,
        left: bool,
    ) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let result = CPU::rotate_through_carry(cpu.clone(), data, left);
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, result));
        }
    }

    fn rol<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        CPU::rotate_op(cpu, pointer, true)
    }

    fn ror<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        CPU::rotate_op(cpu, pointer, false)
    }

    fn rotate_acc_op<'a>(cpu: Rc<RefCell<CPU>>, left: bool) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_a();
            let result = CPU::rotate_through_carry(cpu.clone(), data, left);
            cpu.borrow_mut().reg.set_a(result);
        }
    }

    fn rol_a<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        CPU::rotate_acc_op(cpu, true)
    }

    fn ror_a<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        CPU::rotate_acc_op(cpu, false)
    }

    fn shift_with_carry(cpu: Rc<RefCell<CPU>>, data: u16, left: bool) -> u16 {
        let n_bits = n_bits!(cpu, m);
        let check_mask = if left { 1 << (n_bits - 1) } else { 0x01 };
        cpu.borrow_mut().reg.p.c = (data & check_mask) == check_mask;
        let result = if left { data << 1 } else { data >> 1 };
        cpu.borrow_mut().reg.p.n = (result >> (n_bits - 1)) == 1;
        cpu.borrow_mut().reg.p.z = result & ((1u32 << n_bits) - 1) as u16 == 0;
        result
    }

    fn asl<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let result = CPU::shift_with_carry(cpu.clone(), data, true);
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, result));
        }
    }

    fn asl_a<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_a();
            let result = CPU::shift_with_carry(cpu.clone(), data, true);
            cpu.borrow_mut().reg.set_a(result);
        }
    }

    fn lsr<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let data = yield_all!(CPU::read_pointer(cpu.clone(), pointer));
            let result = CPU::shift_with_carry(cpu.clone(), data, false);
            yield_all!(CPU::write_pointer(cpu.clone(), pointer, result));
        }
    }

    fn lsr_a<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
            let data = cpu.borrow().reg.get_a();
            let result = CPU::shift_with_carry(cpu.clone(), data, false);
            cpu.borrow_mut().reg.set_a(result);
        }
    }

    // Branch instructions

    fn bra<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            // let source_pc = cpu.borrow().reg.pc + 1u16;
            // let dest_pc = u24((source_pc.raw() as i32 + (fetch!(cpu) as i8 as i32)) as u32);
            // TODO: NOTE: This is a critical bug fix
            let source_pc = cpu.borrow().reg.pc;
            let offset = fetch!(cpu) as i8 as i16;
            let dest_pc = source_pc.wrapping_add_lo16(offset + 1);
            cpu.borrow_mut().reg.pc = dest_pc;
            yield_all!(CPU::idle(cpu.clone()));
            // TODO: Is this right? Maybe only for emulation mode...
            if (source_pc & 0x100u16).raw() != (dest_pc & 0x100u16).raw() {
                yield_all!(CPU::idle(cpu.clone()));
            }
        }
    }

    fn brl<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let source_pc = cpu.borrow().reg.pc + 2u16;
            let dest_pc = u24((source_pc.raw() as i32 + (fetch_u16!(cpu) as i16 as i32)) as u32);
            // TODO: NOTE: This is a critical bug fix
            // let source_pc = cpu.borrow().reg.pc;
            // let offset = fetch_u16!(cpu) as i16;
            // let dest_pc = source_pc.wrapping_add_lo16(offset + 2);
            cpu.borrow_mut().reg.pc = dest_pc;
            yield_all!(CPU::idle(cpu.clone()));
            // TODO: Maybe some bank cross cycles? Maybe only for emulation mode...
        }
    }

    // Return instructions

    fn return_op<'a>(cpu: Rc<RefCell<CPU>>, long: bool) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            cpu.borrow_mut().reg.pc &= 0xFF_0000u32;
            let addr = yield_all!(CPU::stack_pull_u16(cpu.clone()));
            cpu.borrow_mut().reg.pc |= addr;
            if long {
                cpu.borrow_mut().reg.pc &= 0x00_FFFFu32;
                let pb = yield_all!(CPU::stack_pull_u8(cpu.clone())) as u32;
                cpu.borrow_mut().reg.pc |= pb << 16;
            }
        }
    }

    fn rts<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        CPU::return_op(cpu, false)
    }

    fn rtl<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        CPU::return_op(cpu, true)
    }

    fn rti<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            let p = yield_all!(CPU::stack_pull_u8(cpu.clone()));
            cpu.borrow_mut().reg.set_p(p);
            cpu.borrow_mut().reg.pc = {
                let addr = yield_all!(CPU::stack_pull_u16(cpu.clone())) as u32;
                let pb = yield_all!(CPU::stack_pull_u8(cpu.clone())) as u32;
                u24((pb << 16) | addr)
            };
        }
    }

    fn pull_p<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            yield_all!(CPU::idle(cpu.clone()));
            let data = yield_all!(CPU::stack_pull_u8(cpu.clone()));
            cpu.borrow_mut().reg.set_p(data);
        }
    }

    fn push_pb<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::idle(cpu.clone()));
            let data = cpu.borrow_mut().reg.pc.bank();
            yield_all!(CPU::stack_push_u8(cpu.clone(), data));
        }
    }

    fn push_addr<'a>(cpu: Rc<RefCell<CPU>>, pointer: Pointer) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            yield_all!(CPU::stack_push_u16(cpu.clone(), pointer.addr.lo16()));
        }
    }

    fn push_relative_addr<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            // let source_pc = cpu.borrow().reg.pc + 2u16;
            // TODO: NOTE: This is a critical bug fix
            let source_pc = cpu.borrow().reg.pc + 3u16;
            let dest_pc = u24((source_pc.raw() as i32 + (fetch_u16!(cpu) as i16 as i32)) as u32);
            yield_all!(CPU::stack_push_u16(cpu.clone(), dest_pc.lo16()));
        }
    }

    fn stp<'a>(cpu: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            // cpu.borrow_mut().stopped = true;
            cpu.borrow_mut().progress_pc(-1);
        }
    }

    fn nop<'a>(_: Rc<RefCell<CPU>>) -> impl InstructionCoroutine + 'a {
        #[coroutine]
        move || {
            dummy_yield!();
        }
    }

    pull_instrs!();
    push_instrs!();
    transfer_instrs!();
    branch_instrs!();
}
