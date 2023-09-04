pub mod registers;

pub use registers::{IoRegisters, Registers, StatusRegister};

use crate::cpu::yield_ticks;
use crate::scheduler::*;

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use paste::paste;

pub const RESET_VECTOR: u16 = 0xFFFE;

#[rustfmt::skip]
const BOOT_ROM: [u8; 64] = [
	0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0,
	0xFC, 0x8F, 0xAA, 0xF4, 0x8F, 0xBB, 0xF5, 0x78,
	0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4,
	0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B, 0xE4, 0xF5,
	0xCB, 0xF4, 0xD7, 0x00, 0xFC, 0xD0, 0xF3, 0xAB,
	0x01, 0x10, 0xEF, 0x7E, 0xF4, 0x10, 0xEB, 0xBA,
	0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD,
	0x5D, 0xD0, 0xDB, 0x1F, 0x00, 0x00, 0xC0, 0xFF
];

macro_rules! transfer_instrs {
    ($from:ident => sp) => {
        paste! {
            fn [<transfer_ $from _sp>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    let data = smp.borrow().reg.$from;
                    smp.borrow_mut().reg.sp = data;
                }
            }
        }
    };
    ($from:ident => $to:ident) => {
        paste! {
            fn [<transfer_ $from _ $to>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    let data = smp.borrow().reg.$from;
                    smp.borrow_mut().reg.$to = data;
                    smp.borrow_mut().reg.psw.n = ((data >> 7) == 1);
                    smp.borrow_mut().reg.psw.z = (data == 0);
                }
            }
        }
    };
    () => {
        transfer_instrs!(x => a);
        transfer_instrs!(a => x);
        transfer_instrs!(y => a);
        transfer_instrs!(a => y);
        transfer_instrs!(sp => x);
        transfer_instrs!(x => sp);
    }
}

macro_rules! load_instrs {
    ($reg:ident) => {
        paste! {
            fn [<load_ $reg>]<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(SMP::read_u8(smp.clone(), addr));
                    smp.borrow_mut().reg.$reg = data;
                    smp.borrow_mut().reg.psw.n = ((data >> 7) == 1);
                    smp.borrow_mut().reg.psw.z = (data == 0);
                }
            }
        }
    };
    () => {
        load_instrs!(a);
        load_instrs!(x);
        load_instrs!(y);
    }
}

macro_rules! store_instrs {
    ($reg:ident) => {
        paste! {
            fn [<store_ $reg>]<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
                move || {
                    // TODO: Some store instructions issue reads
                    let data = smp.borrow_mut().reg.$reg;
                    yield_all!(SMP::write_u8(smp.clone(), addr, data));
                }
            }
        }
    };
    () => {
        store_instrs!(a);
        store_instrs!(x);
        store_instrs!(y);
    }
}

/// Implement *_mem_to_mem and *_acc variants for a given instruction using SMP::*_algorithm().
macro_rules! alu_instr {
    ($op:ident) => {
        paste! {
            fn [<$op _mem_to_mem>]<'a>(
                smp: Rc<RefCell<SMP>>,
                addrs: MemToMemAddresses,
            ) -> impl InstructionGenerator + 'a {
                move || {
                    let dest_data = yield_all!(SMP::read_u8(smp.clone(), addrs.dest_addr));
                    let src_data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr));
                    let output = SMP::[<$op _algorithm>](smp.clone(), src_data, dest_data);
                    yield_all!(SMP::write_u8(smp.clone(), addrs.dest_addr, output));
                }
            }

            fn [<$op _acc>]<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
                move || {
                    let src_data = yield_all!(SMP::read_u8(smp.clone(), addr));
                    let a = smp.borrow().reg.a;
                    let output = Self::[<$op _algorithm>](smp.clone(), src_data, a);
                    smp.borrow_mut().reg.a = output;
                }
            }
        }
    };
}

/// Implement *_mem, *_acc, *_x, and *_y variants for a given instruction using SMP::*_algorithm().
macro_rules! step_shift_instrs {
    ($op:ident, mem) => {
        paste! {
            fn [<$op _mem>]<'a>( smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(SMP::read_u8(smp.clone(), addr));
                    let output = SMP::[<$op _algorithm>](smp.clone(), data);
                    yield_all!(SMP::write_u8(smp.clone(), addr, output));
                }
            }
        }
    };
    ($op:ident, $reg:ident, $reg_fn_name:ident) => {
        paste! {
            fn [<$op _ $reg_fn_name>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    let data = smp.borrow().reg.$reg;
                    let output = Self::[<$op _algorithm>](smp.clone(), data);
                    smp.borrow_mut().reg.$reg = output;
                }
            }
        }
    };
    ($op:ident, common) => {
        step_shift_instrs!($op, mem);
        step_shift_instrs!($op, a, acc);
    };
    ($op:ident, inc_dec) => {
        step_shift_instrs!($op, common);
        step_shift_instrs!($op, x, x);
        step_shift_instrs!($op, y, y);
    };
    () => {
        step_shift_instrs!(asl, common);
        step_shift_instrs!(rol, common);
        step_shift_instrs!(lsr, common);
        step_shift_instrs!(ror, common);
        step_shift_instrs!(dec, inc_dec);
        step_shift_instrs!(inc, inc_dec);
    };
}
macro_rules! flag_instrs {
    ($flag:ident, set) => {
        paste! {
            fn [<set_flag_ $flag>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    smp.borrow_mut().reg.psw.$flag = true;
                }
            }
        }
    };
    ($flag:ident, clear) => {
        paste! {
            fn [<clear_flag_ $flag>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    dummy_yield!();
                    smp.borrow_mut().reg.psw.$flag = false;
                }
            }
        }
    };
    ($flag:ident) => {
        flag_instrs!($flag, set);
        flag_instrs!($flag, clear);
    };
    () => {
        flag_instrs!(p);
        flag_instrs!(c);
        flag_instrs!(i);
        flag_instrs!(v, clear);
    };
}

macro_rules! push_pop_instrs {
    ($reg:ident) => {
        paste! {
            fn [<push_ $reg>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    let data = smp.borrow().reg.$reg;
                    yield_all!(SMP::stack_push_u8(smp.clone(), data));
                }
            }

            fn [<pop_ $reg>]<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(SMP::stack_pop_u8(smp.clone()));
                    smp.borrow_mut().reg.$reg = data;
                }
            }
        }
    };
    () => {
        push_pop_instrs!(a);
        push_pop_instrs!(x);
        push_pop_instrs!(y);
    };
}

macro_rules! branch_instrs {
    ($flag:ident => $val:expr, $set_clear:ident) => {
        paste! {
            fn [<branch_ $flag _ $set_clear>]<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
                move || {
                    let src_pc = smp.borrow().reg.pc;
                    // TODO: Do we even read the offset if we don't branch? Almost certainly, as it's part of decoding
                    let offset = yield_all!(SMP::read_u8(smp.clone(), addr));
                    let dest_pc = (src_pc as i32 + (offset as i8 as i32)) as u16;
                    if smp.borrow_mut().reg.psw.$flag == $val {
                        smp.borrow_mut().reg.pc = dest_pc;
                        // TODO: Need to look into how many cycles branch can take
                        // smp.borrow_mut().step(1);
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

macro_rules! bit_branch_instrs {
    ($bit:expr => $val:expr, $set_clear:ident) => {
        paste! {
            fn [<branch_bit_ $bit _ $set_clear>]<'a>(smp: Rc<RefCell<SMP>>, addrs: MemToMemAddresses) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr));
                    let offset = yield_all!(SMP::read_u8(smp.clone(), addrs.dest_addr));
                    let src_pc = smp.borrow().reg.pc;
                    let dest_pc = (src_pc as i32 + (offset as i8 as i32)) as u16;
                    if (data >> $bit) & 1 == $val {
                        smp.borrow_mut().reg.pc = dest_pc;
                        // TODO: Need to look into how many cycles branch can take
                        // smp.borrow_mut().step(1);
                    }
                }
            }
        }
    };
    ($($bit:expr),+) => {
        $(
        bit_branch_instrs!($bit => 1, set);
        bit_branch_instrs!($bit => 0, clear);
        )+
    };
    () => {
        bit_branch_instrs!(0, 1, 2, 3, 4, 5, 6, 7);
    };
}

macro_rules! set_clear_bit_instrs {
    ($bit:expr => $val:expr, $set_clear:ident) => {
        paste! {
            fn [<$set_clear _bit_ $bit>]<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
                move || {
                    let data = yield_all!(SMP::read_u8(smp.clone(), addr));
                    let result = (data & !(1 << $bit)) | (($val as u8) << $bit);
                    yield_all!(SMP::write_u8(smp.clone(), addr, result));
                }
            }
        }
    };
    ($($bit:expr),+) => {
        $(
        set_clear_bit_instrs!($bit => true, set);
        set_clear_bit_instrs!($bit => false, clear);
        )+
    };
    () => {
        set_clear_bit_instrs!(0, 1, 2, 3, 4, 5, 6, 7);
    };
}

macro_rules! instr {
    ($smp_rc: ident, $instr_f:ident) => {
        yield_ticks!($smp_rc, SMP::$instr_f($smp_rc.clone()))
    };
    ($smp_rc: ident, $instr_f:ident, implied) => {
        instr!($smp_rc, $instr_f)
    };
    ($smp_rc: ident, $instr_f:ident, $addr_mode_f:ident) => {{
        let data = yield_ticks!($smp_rc, SMP::$addr_mode_f($smp_rc.clone()));
        yield_ticks!($smp_rc, SMP::$instr_f($smp_rc.clone(), data))
    }};
    ($smp_rc: ident, $instr_f:ident, $addr_mode_f:ident) => {
        instr!($smp_rc, $instr_f, $addr_mode_f)
    };
}

macro_rules! instrs {
    ($smp_rc:ident, $opcode_expr:expr, $(($instr_f:ident; $($opcode:expr => $addr_mode_f:tt),+))+) => {
        let opcode_val = $opcode_expr;
        match opcode_val {
            $(
                $($opcode => instr!($smp_rc, $instr_f, $addr_mode_f),)+
            )+
            _ => panic!("Invalid SMP opcode {:#04X} at {:#06X}", opcode_val, $smp_rc.borrow().reg.pc - 1),
        }
    };
}

macro_rules! fetch {
    ($smp_rc: ident) => {{
        let data = yield_all!(SMP::read_u8($smp_rc.clone(), $smp_rc.borrow().reg.pc));
        $smp_rc.borrow_mut().reg.pc += 1;
        data
    }};
}

/// Holds the source and destination addresses for memory-to-memory instructions (e.g. OR (X),(Y))
#[derive(Copy, Clone)]
struct MemToMemAddresses {
    pub src_addr: u16,
    pub dest_addr: u16,
}

/// The SMP, i.e. the SPC700 audio coprocessor
pub struct SMP {
    reg: Registers,
    io_reg: IoRegisters,
    ticks_run: u64,
    ram: Vec<u8>,
    debug_log: bool, // TODO: Remove this debugging tool
}

impl SMP {
    pub fn new() -> Self {
        Self {
            reg: Registers::new(),
            io_reg: IoRegisters::new(),
            ticks_run: 0,
            ram: vec![0; 0x10000],
            debug_log: false,
        }
    }

    pub fn registers(&self) -> &Registers {
        &self.reg
    }

    pub fn registers_mut(&mut self) -> &mut Registers {
        &mut self.reg
    }

    pub fn reset(smp: Rc<RefCell<SMP>>) {
        smp.borrow_mut().ram.fill(0);
        smp.borrow_mut().ticks_run = 0;
        smp.borrow_mut().io_reg = IoRegisters::new();
        smp.borrow_mut().io_reg.internal_ports.fill(0);
        smp.borrow_mut().io_reg.external_ports.fill(0);
        smp.borrow_mut().io_reg.control.0 = 0x80;
        smp.borrow_mut().io_reg.dsp_addr = 0xFF;
        // TODO: Apparently should be DSP[7Fh]
        smp.borrow_mut().io_reg.dsp_data = 0;
        smp.borrow_mut().io_reg.timer_dividers.fill(0xFF);
        smp.borrow_mut().io_reg.timers.fill(0);
        smp.borrow_mut().reg = Registers::new();
        // TODO: Simplify
        smp.borrow_mut().reg.pc = ignore_yields!(Self::read_u16(smp.clone(), RESET_VECTOR));
        smp.borrow_mut().reg.sp = 0xEF;
    }

    pub fn run<'a>(smp: Rc<RefCell<SMP>>) -> impl DeviceGenerator + 'a {
        move || loop {
            // if smp.borrow().reg.pc == 0x08C5 {
            //     smp.borrow_mut().debug_log = true;
            // }
            if smp.borrow().debug_log {
                print!("SMP {:#06X}", smp.borrow().reg.pc);
            }
            let opcode = yield_ticks!(smp, SMP::read_u8(smp.clone(), smp.borrow().reg.pc));
            smp.borrow_mut().reg.pc += 1;
            if smp.borrow().debug_log {
                let reg = &smp.borrow().reg;
                println!(
                    ": {opcode:#04X}    A:{:02X} X:{:02X} Y:{:02X} SP:{:02X} PSW:{:02X} in_ports:{:02X?} ext_ports:{:02X?}",
                    reg.a,
                    reg.x,
                    reg.y,
                    reg.sp,
                    reg.psw.get(),
                    smp.borrow().io_reg.internal_ports,
                    smp.borrow().io_reg.external_ports,
                );
            }

            instrs!(
                smp, opcode,
                (transfer_x_a; 0x7D=>implied)
                (transfer_a_x; 0x5D=>implied)
                (transfer_y_a; 0xDD=>implied)
                (transfer_a_y; 0xFD=>implied)
                (transfer_sp_x; 0x9D=>implied)
                (transfer_x_sp; 0xBD=>implied)
                (mov_mem_to_mem; 0x8F=>immediate_to_direct, 0xFA=>direct_to_direct)
                (load_a; 0xE8=>immediate, 0xE4=>direct, 0xF4=>direct_x,
                 0xE5=>absolute, 0xF5=>absolute_x, 0xF6=>absolute_y,
                 0xE6=>indirect, 0xBF=>indirect_increment,
                 0xF7=>indirect_indexed, 0xE7=>indexed_indirect)
                (load_x; 0xCD=>immediate, 0xF8=>direct, 0xF9=>direct_y, 0xE9=>absolute)
                (load_y; 0x8D=>immediate, 0xEB=>direct, 0xFB=>direct_x, 0xEC=>absolute)
                (movw_mem_to_ya; 0xBA=>direct)
                (store_a; 0xC4=>direct, 0xD4=>direct_x, 0xC5=>absolute, 0xD5=>absolute_x,
                 0xD6=>absolute_y, 0xAF=>indirect_increment, 0xC6=>indirect,
                 0xD7=>indirect_indexed, 0xC7=>indexed_indirect)
                (store_x; 0xD8=>direct, 0xD9=>direct_y, 0xC9=>absolute)
                (store_y; 0xCB=>direct, 0xDB=>direct_x, 0xCC=>absolute)
                (movw_ya_to_mem; 0xDA=>direct)
                (push_a; 0x2D=>implied)
                (push_x; 0x4D=>implied)
                (push_y; 0x6D=>implied)
                (push_psw; 0x0D=>implied)
                (pop_a; 0xAE=>implied)
                (pop_x; 0xCE=>implied)
                (pop_y; 0xEE=>implied)
                (pop_psw; 0x8E=>implied)
                (or_acc; 0x08=>immediate, 0x06=>indirect, 0x04=>direct,
                 0x14=>direct_x, 0x05=>absolute, 0x15=>absolute_x,
                 0x16=>absolute_y, 0x17=>indirect_indexed, 0x07=>indexed_indirect)
                (or_mem_to_mem; 0x09=>direct_to_direct,
                 0x18=>immediate_to_direct, 0x19=>indirect_to_indirect)
                (and_acc; 0x28=>immediate, 0x26=>indirect, 0x24=>direct,
                 0x34=>direct_x, 0x25=>absolute, 0x35=>absolute_x,
                 0x36=>absolute_y, 0x37=>indirect_indexed, 0x27=>indexed_indirect)
                (and_mem_to_mem; 0x29=>direct_to_direct,
                 0x38=>immediate_to_direct, 0x39=>indirect_to_indirect)
                (eor_acc; 0x48=>immediate, 0x46=>indirect, 0x44=>direct,
                 0x54=>direct_x, 0x45=>absolute, 0x55=>absolute_x,
                 0x56=>absolute_y, 0x57=>indirect_indexed, 0x47=>indexed_indirect)
                (eor_mem_to_mem; 0x49=>direct_to_direct,
                 0x58=>immediate_to_direct, 0x59=>indirect_to_indirect)
                (cmp_acc; 0x68=>immediate, 0x66=>indirect, 0x64=>direct,
                 0x74=>direct_x, 0x65=>absolute, 0x75=>absolute_x,
                 0x76=>absolute_y, 0x77=>indirect_indexed, 0x67=>indexed_indirect)
                (cmp_mem_to_mem; 0x69=>direct_to_direct,
                 0x78=>immediate_to_direct, 0x79=>indirect_to_indirect)
                (cmp_x; 0xC8=>immediate, 0x3E=>direct, 0x1E=>absolute)
                (cmp_y; 0xAD=>immediate, 0x7E=>direct, 0x5E=>absolute)
                (adc_acc; 0x88=>immediate, 0x86=>indirect, 0x84=>direct,
                 0x94=>direct_x, 0x85=>absolute, 0x95=>absolute_x,
                 0x96=>absolute_y, 0x97=>indirect_indexed, 0x87=>indexed_indirect)
                (adc_mem_to_mem; 0x89=>direct_to_direct,
                 0x98=>immediate_to_direct, 0x99=>indirect_to_indirect)
                (sbc_acc; 0xA8=>immediate, 0xA6=>indirect, 0xA4=>direct,
                 0xB4=>direct_x, 0xA5=>absolute, 0xB5=>absolute_x,
                 0xB6=>absolute_y, 0xB7=>indirect_indexed, 0xA7=>indexed_indirect)
                (sbc_mem_to_mem; 0xA9=>direct_to_direct,
                 0xB8=>immediate_to_direct, 0xB9=>indirect_to_indirect)
                (xcn; 0x9F=>implied)
                (tclr1; 0x4E=>absolute)
                (tset1; 0x0E=>absolute)
                (asl_acc; 0x1C=>implied)
                (asl_mem; 0x0B=>direct, 0x1B=>direct_x, 0x0C=>absolute)
                (rol_acc; 0x3C=>implied)
                (rol_mem; 0x2B=>direct, 0x3B=>direct_x, 0x2C=>absolute)
                (lsr_acc; 0x5C=>implied)
                (lsr_mem; 0x4B=>direct, 0x5B=>direct_x, 0x4C=>absolute)
                (ror_acc; 0x7C=>implied)
                (ror_mem; 0x6B=>direct, 0x7B=>direct_x, 0x6C=>absolute)
                (dec_acc; 0x9C=>implied)
                (dec_x; 0x1D=>implied)
                (dec_y; 0xDC=>implied)
                (dec_mem; 0x8B=>direct, 0x9B=>direct_x, 0x8C=>absolute)
                (inc_acc; 0xBC=>implied)
                (inc_x; 0x3D=>implied)
                (inc_y; 0xFC=>implied)
                (inc_mem; 0xAB=>direct, 0xBB=>direct_x, 0xAC=>absolute)
                (addw; 0x7A=>direct)
                (subw; 0x9A=>direct)
                (cmpw; 0x5A=>direct)
                (incw; 0x3A=>direct)
                (decw; 0x1A=>direct)
                (div; 0x9E=>implied)
                (mul; 0xCF=>implied)
                (clear_bit_0; 0x12=>direct)
                (clear_bit_1; 0x32=>direct)
                (clear_bit_2; 0x52=>direct)
                (clear_bit_3; 0x72=>direct)
                (clear_bit_4; 0x92=>direct)
                (clear_bit_5; 0xB2=>direct)
                (clear_bit_6; 0xD2=>direct)
                (clear_bit_7; 0xF2=>direct)
                (set_bit_0; 0x02=>direct)
                (set_bit_1; 0x22=>direct)
                (set_bit_2; 0x42=>direct)
                (set_bit_3; 0x62=>direct)
                (set_bit_4; 0x82=>direct)
                (set_bit_5; 0xA2=>direct)
                (set_bit_6; 0xC2=>direct)
                (set_bit_7; 0xE2=>direct)
                (clear_flag_c; 0x60=>implied)
                (set_flag_c; 0x80=>implied)
                (flip_flag_c; 0xED=>implied)
                (clear_flag_v; 0xE0=>implied)
                (branch_n_clear; 0x10=>immediate)
                (branch_n_set; 0x30=>immediate)
                (branch_v_clear; 0x50=>immediate)
                (branch_v_set; 0x70=>immediate)
                (branch_c_clear; 0x90=>immediate)
                (branch_c_set; 0xB0=>immediate)
                (branch_z_clear; 0xD0=>immediate)
                (branch_z_set; 0xF0=>immediate)
                (branch_bit_0_set; 0x03=>direct_and_relative)
                (branch_bit_1_set; 0x23=>direct_and_relative)
                (branch_bit_2_set; 0x43=>direct_and_relative)
                (branch_bit_3_set; 0x63=>direct_and_relative)
                (branch_bit_4_set; 0x83=>direct_and_relative)
                (branch_bit_5_set; 0xA3=>direct_and_relative)
                (branch_bit_6_set; 0xC3=>direct_and_relative)
                (branch_bit_7_set; 0xE3=>direct_and_relative)
                (branch_bit_0_clear; 0x13=>direct_and_relative)
                (branch_bit_1_clear; 0x33=>direct_and_relative)
                (branch_bit_2_clear; 0x53=>direct_and_relative)
                (branch_bit_3_clear; 0x73=>direct_and_relative)
                (branch_bit_4_clear; 0x93=>direct_and_relative)
                (branch_bit_5_clear; 0xB3=>direct_and_relative)
                (branch_bit_6_clear; 0xD3=>direct_and_relative)
                (branch_bit_7_clear; 0xF3=>direct_and_relative)
                (cbne; 0x2E=>direct_and_relative, 0xDE=>direct_x_and_relative)
                (dbnz_y; 0xFE=>immediate)
                (dbnz_mem; 0x6E=>direct_and_relative)
                (bra; 0x2F=>immediate)
                (jmp; 0x5F=>absolute, 0x1F=>absolute_indexed_indirect)
                (call; 0x3F=>absolute)
                (pcall; 0x4F=>direct)
                (ret; 0x6F=>implied)
                (ret_from_interrupt; 0x7F=>implied)
                (clear_flag_p; 0x20=>implied)
                (set_flag_p; 0x40=>implied)
                (set_flag_i; 0xA0=>implied)
                (clear_flag_i; 0xC0=>implied)
            );

            yield (YieldReason::FinishedInstruction(Device::SMP), 0);
        }
    }

    fn step(&mut self, n_clocks: u64) {
        self.ticks_run += n_clocks;
        // TODO: This is a super simple dummy implementation, not remotely correct
        self.io_reg.debug_timer_divider =
            self.io_reg.debug_timer_divider.wrapping_add(n_clocks as u8);
        if self.io_reg.debug_timer_divider >= 128 {
            self.io_reg.debug_timer_divider -= 128;
            for timer in self.io_reg.timers.iter_mut() {
                *timer = timer.wrapping_add(1);
            }
        }
    }

    fn peak_io_reg(&self, addr: u16) -> u8 {
        match addr {
            0x00F0 => todo!("IO reg read {addr:#06X}"),
            0x00F1 => self.io_reg.control.0,
            0x00F2 => self.io_reg.dsp_addr,
            0x00F3 => self.io_reg.dsp_data,
            0x00F4..=0x00F7 => self.io_reg.external_ports[addr as usize - 0x00F4],
            0x00F8..=0x00F9 => todo!("IO reg read {addr:#06X}"),
            0x00FA..=0x00FC => self.io_reg.timer_dividers[addr as usize - 0x00FA],
            0x00FD..=0x00FF => self.io_reg.timers[addr as usize - 0x00FD],
            _ => panic!("Address {:#02X} is not an SMP IO register", addr),
        }
    }

    fn read_io_reg<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: I think this should maybe yield to CPU for some (all?) of these?
            dummy_yield!();
            smp.borrow().peak_io_reg(addr)
        }
    }

    fn write_io_reg<'a>(smp: Rc<RefCell<SMP>>, addr: u16, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            // TODO: I think this should maybe yield to CPU for some (all?) of these?
            dummy_yield!();
            match addr {
                0x00F0 => todo!("IO reg write {addr:#06X}"),
                0x00F1 => smp.borrow_mut().io_reg.control.0 = data,
                0x00F2 => smp.borrow_mut().io_reg.dsp_addr = data,
                0x00F3 => smp.borrow_mut().io_reg.dsp_data = data,
                0x00F4..=0x00F7 => {
                    smp.borrow_mut().io_reg.internal_ports[addr as usize - 0x00F4] = data
                }
                0x00F8..=0x00F9 => todo!("IO reg write {addr:#06X}"),
                0x00FA..=0x00FC => {
                    smp.borrow_mut().io_reg.timer_dividers[addr as usize - 0x00FA] = data
                }
                0x00FD..=0x00FF => {}
                _ => panic!("Address {:#02X} is not an SMP IO register", addr),
            }
        }
    }

    pub fn peak_u8(&self, addr: u16) -> u8 {
        let data = match addr {
            0x0000..=0x00EF => self.ram[addr as usize],
            0x00F0..=0x00FF => self.peak_io_reg(addr),
            0x0100..=0xFFBF => self.ram[addr as usize],
            0xFFC0..=0xFFFF => {
                if self.io_reg.control.rom_at_high_addresses() {
                    BOOT_ROM[addr as usize - 0xFFC0]
                } else {
                    self.ram[addr as usize]
                }
            }
        };
        data
    }

    pub fn peak_u16(&self, addr: u16) -> u16 {
        let lo = self.peak_u8(addr);
        ((self.peak_u8(addr.wrapping_add(1)) as u16) << 8) | lo as u16
    }

    fn read_u8<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: Could this be more granular? Does every access need to sync?
            yield YieldReason::Sync(Device::CPU);
            let data = match addr {
                0x0000..=0x00EF => smp.borrow().ram[addr as usize],
                0x00F0..=0x00FF => yield_all!(SMP::read_io_reg(smp.clone(), addr)),
                0x0100..=0xFFBF => smp.borrow_mut().ram[addr as usize],
                0xFFC0..=0xFFFF => {
                    if smp.borrow().io_reg.control.rom_at_high_addresses() {
                        BOOT_ROM[addr as usize - 0xFFC0]
                    } else {
                        smp.borrow().ram[addr as usize]
                    }
                }
            };
            // TODO: Some clock cycles before the read, depending on region
            smp.borrow_mut().step(2);
            data
        }
    }

    fn read_u16<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl Yieldable<u16> + 'a {
        move || {
            let lo = yield_all!(SMP::read_u8(smp.clone(), addr)) as u16;
            let hi = yield_all!(SMP::read_u8(smp.clone(), addr + 1)) as u16;
            (hi << 8) | lo
        }
    }

    fn write_u8<'a>(smp: Rc<RefCell<SMP>>, addr: u16, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            // TODO: Could this be more granular? Does every access need to sync?
            yield YieldReason::Sync(Device::CPU);
            // All writes always go to ram, even if they also go to e.g. IO
            smp.borrow_mut().ram[addr as usize] = data;
            match addr {
                0x00F0..=0x00FF => yield_all!(SMP::write_io_reg(smp.clone(), addr, data)),
                _ => {}
            }
            // TODO: Some clock cycles before the write, depending on region
            smp.borrow_mut().step(2);
        }
    }

    fn write_u16<'a>(smp: Rc<RefCell<SMP>>, addr: u16, data: u16) -> impl Yieldable<()> + 'a {
        move || {
            yield_all!(SMP::write_u8(smp.clone(), addr, data as u8));
            yield_all!(SMP::write_u8(smp.clone(), addr + 1, (data >> 8) as u8));
        }
    }

    fn stack_push_u8<'a>(smp: Rc<RefCell<SMP>>, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            let stack_addr = 0x100 + smp.borrow().reg.sp as u16;
            yield_all!(SMP::write_u8(smp.clone(), stack_addr, data));
            let new_sp = smp.borrow().reg.sp.wrapping_sub(1);
            smp.borrow_mut().reg.sp = new_sp;
        }
    }

    fn stack_pop_u8<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u8> + 'a {
        move || {
            let new_sp = smp.borrow().reg.sp.wrapping_add(1);
            smp.borrow_mut().reg.sp = new_sp;
            let stack_addr = 0x100 + smp.borrow().reg.sp as u16;
            yield_all!(SMP::read_u8(smp.clone(), stack_addr))
        }
    }

    pub fn io_peak(&self, addr: u16) -> u8 {
        // TODO
        0
    }

    pub fn io_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2140..=0x2143 => self.io_reg.internal_ports[addr as usize - 0x2140],
            _ => panic!("Invalid IO read of SMP at {addr:#06X}"),
        }
    }

    pub fn io_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2140..=0x2143 => self.io_reg.external_ports[addr as usize - 0x2140] = data,
            _ => panic!("Invalid IO write of SMP at {addr:#06X}"),
        }
    }

    // Addressing modes:

    fn immediate<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let addr = smp.borrow().reg.pc;
            // TODO: Need to use wrapping adds for PC increments and most addressing mode adds
            smp.borrow_mut().reg.pc += 1;
            addr
        }
    }

    // TODO: Reduce code duplication across these three
    fn direct<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr = direct_page_base + fetch!(smp) as u16;
            direct_addr
        }
    }

    fn direct_x<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr =
                direct_page_base + (fetch!(smp).wrapping_add(smp.borrow().reg.x)) as u16;
            direct_addr
        }
    }

    fn direct_y<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr =
                direct_page_base + (fetch!(smp).wrapping_add(smp.borrow().reg.y)) as u16;
            direct_addr
        }
    }

    // TODO: Reduce code duplication across these three
    fn absolute<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let addr_lo = fetch!(smp) as u16;
            ((fetch!(smp) as u16) << 8) | addr_lo
        }
    }

    fn absolute_x<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let addr_lo = fetch!(smp) as u16;
            (((fetch!(smp) as u16) << 8) | addr_lo).wrapping_add(smp.borrow().reg.x as u16)
        }
    }

    fn absolute_y<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let addr_lo = fetch!(smp) as u16;
            (((fetch!(smp) as u16) << 8) | addr_lo).wrapping_add(smp.borrow().reg.y as u16)
        }
    }

    fn absolute_indexed_indirect<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let addr_lo = fetch!(smp) as u16;
            let absolute_addr = ((fetch!(smp) as u16) << 8) | addr_lo;
            let indirect_addr = absolute_addr.wrapping_add(smp.borrow().reg.x as u16);
            yield_all!(SMP::read_u16(smp.clone(), indirect_addr)) // Jump destination
        }
    }

    fn indirect<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr = direct_page_base + smp.borrow().reg.x as u16;
            direct_addr
        }
    }

    fn indirect_increment<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let x = smp.borrow().reg.x;
            let direct_addr = direct_page_base + x as u16;
            smp.borrow_mut().reg.x = x.wrapping_add(1);
            direct_addr
        }
    }

    fn indirect_indexed<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr = fetch!(smp);
            let indirect_addr = {
                let addr_lo = yield_all!(SMP::read_u8(
                    smp.clone(),
                    direct_page_base + direct_addr as u16
                )) as u16;
                let addr_hi = yield_all!(SMP::read_u8(
                    smp.clone(),
                    direct_page_base + direct_addr.wrapping_add(1) as u16
                )) as u16;
                (addr_hi << 8) | addr_lo
            };
            indirect_addr + smp.borrow().reg.y as u16
        }
    }

    fn indexed_indirect<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr =
                direct_page_base + (fetch!(smp).wrapping_add(smp.borrow().reg.x)) as u16;
            let indirect_addr = {
                let addr_lo = yield_all!(SMP::read_u8(
                    smp.clone(),
                    direct_page_base + direct_addr as u16
                )) as u16;
                let addr_hi = yield_all!(SMP::read_u8(
                    smp.clone(),
                    direct_page_base + direct_addr.wrapping_add(1) as u16
                )) as u16;
                (addr_hi << 8) | addr_lo
            };
            indirect_addr
        }
    }

    fn immediate_to_direct<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<MemToMemAddresses> + 'a {
        move || {
            let src_addr = yield_all!(Self::immediate(smp.clone()));
            let dest_addr = yield_all!(Self::direct(smp.clone()));
            MemToMemAddresses {
                src_addr,
                dest_addr,
            }
        }
    }

    fn direct_to_direct<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<MemToMemAddresses> + 'a {
        move || {
            let src_addr = yield_all!(Self::direct(smp.clone()));
            let dest_addr = yield_all!(Self::direct(smp.clone()));
            MemToMemAddresses {
                src_addr,
                dest_addr,
            }
        }
    }

    fn indirect_to_indirect<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<MemToMemAddresses> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let src_addr = direct_page_base + smp.borrow().reg.y as u16;
            let dest_addr = direct_page_base + smp.borrow().reg.x as u16;
            MemToMemAddresses {
                src_addr,
                dest_addr,
            }
        }
    }

    fn direct_and_relative<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<MemToMemAddresses> + 'a {
        move || {
            let src_addr = yield_all!(Self::direct(smp.clone()));
            let dest_addr = yield_all!(Self::immediate(smp.clone()));
            MemToMemAddresses {
                src_addr,
                dest_addr,
            }
        }
    }

    fn direct_x_and_relative<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<MemToMemAddresses> + 'a {
        move || {
            let src_addr = yield_all!(Self::direct_x(smp.clone()));
            let dest_addr = yield_all!(Self::immediate(smp.clone()));
            MemToMemAddresses {
                src_addr,
                dest_addr,
            }
        }
    }

    // Instructions:

    // Generate MOV operations
    transfer_instrs!();
    load_instrs!();
    store_instrs!();

    fn mov_mem_to_mem<'a>(
        smp: Rc<RefCell<SMP>>,
        addrs: MemToMemAddresses,
    ) -> impl InstructionGenerator + 'a {
        move || {
            // TODO: This *might* issue a read, but maybe only for immediate?
            let data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr));
            yield_all!(SMP::write_u8(smp.clone(), addrs.dest_addr, data));
        }
    }

    fn movw_mem_to_ya<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u16(smp.clone(), addr));
            smp.borrow_mut().reg.set_ya(data);
            // TODO: Should this be bit15 or bit7
            smp.borrow_mut().reg.psw.n = (data >> 15) == 1;
            smp.borrow_mut().reg.psw.z = data == 0;
        }
    }

    fn movw_ya_to_mem<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            yield_all!(SMP::read_u8(smp.clone(), addr)); // Dummy read
            yield_all!(SMP::write_u16(
                smp.clone(),
                addr,
                smp.borrow_mut().reg.get_ya()
            ));
        }
    }

    // OR, AND, EOR, CMP, ADC, and SBC support some memory->memory operations;
    // these macros generate both the mem->mem and the accumulator implemetations.
    alu_instr!(or);
    alu_instr!(and);
    alu_instr!(eor);
    alu_instr!(adc);
    alu_instr!(sbc);

    fn or_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, mut dest_data: u8) -> u8 {
        dest_data |= src_data;
        smp.borrow_mut().reg.psw.n = (dest_data >> 7) == 1;
        smp.borrow_mut().reg.psw.z = dest_data == 0;
        dest_data
    }

    fn and_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, mut dest_data: u8) -> u8 {
        dest_data &= src_data;
        smp.borrow_mut().reg.psw.n = (dest_data >> 7) == 1;
        smp.borrow_mut().reg.psw.z = dest_data == 0;
        dest_data
    }

    fn eor_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, mut dest_data: u8) -> u8 {
        dest_data ^= src_data;
        smp.borrow_mut().reg.psw.n = (dest_data >> 7) == 1;
        smp.borrow_mut().reg.psw.z = dest_data == 0;
        dest_data
    }

    fn cmp_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, dest_data: u8) {
        smp.borrow_mut().reg.psw.c = dest_data >= src_data;
        smp.borrow_mut().reg.psw.n = src_data > dest_data;
        smp.borrow_mut().reg.psw.z = src_data == dest_data;
    }

    fn adc_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, dest_data: u8) -> u8 {
        let sum = {
            let carry = smp.borrow().reg.psw.c as i16;
            let temp = dest_data as i16 + src_data as i16 + carry;
            smp.borrow_mut().reg.psw.c = temp > 0xFF;
            smp.borrow_mut().reg.psw.h = ((dest_data & 0xF) + (src_data & 0xF) + carry as u8) > 0xF;
            smp.borrow_mut().reg.psw.n = (temp >> 7) == 1;
            smp.borrow_mut().reg.psw.z = (temp & 0xFF) == 0;
            temp as u8
        };
        // We overflowed iff the MSBs of src and dest were the same, and both differ from that of sum
        smp.borrow_mut().reg.psw.v = ((src_data ^ sum) & (dest_data ^ sum) & 0x80) == 0x80;
        sum
    }

    fn sbc_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, dest_data: u8) -> u8 {
        // TODO: Need to double-check this logic, and that all flags are still correct
        let src_1s_complement = ((src_data as i8).wrapping_neg().wrapping_sub(1)) as u8;
        SMP::adc_algorithm(smp, src_1s_complement, dest_data)
    }

    // CMP doesn't modify memory/registers, so we don't use the macro for it
    fn cmp_mem_to_mem<'a>(
        smp: Rc<RefCell<SMP>>,
        addrs: MemToMemAddresses,
    ) -> impl InstructionGenerator + 'a {
        move || {
            let dest_data = yield_all!(SMP::read_u8(smp.clone(), addrs.dest_addr));
            let src_data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr));
            SMP::cmp_algorithm(smp.clone(), src_data, dest_data);
        }
    }

    fn cmp_acc<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let src_data = yield_all!(SMP::read_u8(smp.clone(), addr));
            let a = smp.borrow().reg.a;
            Self::cmp_algorithm(smp.clone(), src_data, a);
        }
    }

    fn cmp_x<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let src_data = yield_all!(SMP::read_u8(smp.clone(), addr));
            let x = smp.borrow().reg.x;
            Self::cmp_algorithm(smp.clone(), src_data, x);
        }
    }

    fn cmp_y<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let src_data = yield_all!(SMP::read_u8(smp.clone(), addr));
            let y = smp.borrow().reg.y;
            Self::cmp_algorithm(smp.clone(), src_data, y);
        }
    }

    // Special ALU Operations

    fn tclr1<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let a = smp.borrow().reg.a;
            let data = yield_all!(SMP::read_u8(smp.clone(), addr));
            smp.borrow_mut().reg.psw.n = data > a;
            smp.borrow_mut().reg.psw.z = data == a;
            yield_all!(SMP::write_u8(smp.clone(), addr, data & !a));
        }
    }

    fn tset1<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let a = smp.borrow().reg.a;
            let data = yield_all!(SMP::read_u8(smp.clone(), addr));
            smp.borrow_mut().reg.psw.n = data > a;
            smp.borrow_mut().reg.psw.z = data == a;
            yield_all!(SMP::write_u8(smp.clone(), addr, data | a));
        }
    }

    fn xcn<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let a = smp.borrow().reg.a;
            smp.borrow_mut().reg.a = (a >> 4) | (a << 4);
        }
    }

    // Generate increment/decrement and rotate/shift instructions for A, X, Y and mem.
    step_shift_instrs!();

    fn asl_algorithm(smp: Rc<RefCell<SMP>>, data: u8) -> u8 {
        smp.borrow_mut().reg.psw.c = (data >> 7) == 1;
        let result = data << 1;
        smp.borrow_mut().reg.psw.n = (result >> 7) == 1;
        smp.borrow_mut().reg.psw.z = result == 0;
        result
    }

    fn rol_algorithm(smp: Rc<RefCell<SMP>>, data: u8) -> u8 {
        let old_carry = smp.borrow().reg.psw.c;
        smp.borrow_mut().reg.psw.c = (data >> 7) == 1;
        let result = (data << 1) | (old_carry as u8);
        smp.borrow_mut().reg.psw.n = (result >> 7) == 1;
        smp.borrow_mut().reg.psw.z = result == 0;
        result
    }

    fn lsr_algorithm(smp: Rc<RefCell<SMP>>, data: u8) -> u8 {
        smp.borrow_mut().reg.psw.c = (data & 0x1) == 1;
        let result = data >> 1;
        smp.borrow_mut().reg.psw.n = (result >> 7) == 1;
        smp.borrow_mut().reg.psw.z = result == 0;
        result
    }

    fn ror_algorithm(smp: Rc<RefCell<SMP>>, data: u8) -> u8 {
        let old_carry = smp.borrow().reg.psw.c;
        smp.borrow_mut().reg.psw.c = (data & 0x1) == 1;
        let result = (data >> 1) | ((old_carry as u8) << 7);
        smp.borrow_mut().reg.psw.n = (result >> 7) == 1;
        smp.borrow_mut().reg.psw.z = result == 0;
        result
    }

    fn dec_algorithm(smp: Rc<RefCell<SMP>>, data: u8) -> u8 {
        let result = data.wrapping_sub(1);
        smp.borrow_mut().reg.psw.n = (result >> 7) == 1;
        smp.borrow_mut().reg.psw.z = result == 0;
        result
    }

    fn inc_algorithm(smp: Rc<RefCell<SMP>>, data: u8) -> u8 {
        let result = data.wrapping_add(1);
        smp.borrow_mut().reg.psw.n = (result >> 7) == 1;
        smp.borrow_mut().reg.psw.z = result == 0;
        result
    }

    // 16-bit ALU operations

    fn addw_algorithm(smp: Rc<RefCell<SMP>>, data: u16) -> u16 {
        let old_ya = smp.borrow().reg.get_ya();
        let sum = {
            let temp = old_ya as i32 + data as i32;
            smp.borrow_mut().reg.psw.c = temp > 0xFFFF;
            // For wide arithmetic, half-carry is carry from bit11 to bit12
            smp.borrow_mut().reg.psw.h = ((old_ya & 0xFFF) + (data & 0xFFF)) > 0xFFF;
            // TODO: Is N based on bit15 or bit7 for these 16-bit instructions?
            smp.borrow_mut().reg.psw.n = (temp >> 15) == 1;
            smp.borrow_mut().reg.psw.z = (temp & 0xFFFF) == 0;
            temp as u16
        };
        // We overflowed iff the MSBs of src and dest were the same, and both differ from that of sum
        smp.borrow_mut().reg.psw.v = ((old_ya ^ sum) & (data ^ sum) & 0x8000) == 0x8000;
        sum
    }

    fn addw<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u16(smp.clone(), addr));
            let result = SMP::addw_algorithm(smp.clone(), data);
            smp.borrow_mut().reg.set_ya(result);
        }
    }

    fn subw<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u16(smp.clone(), addr));
            let data_1s_complement = ((data as i16).wrapping_neg().wrapping_sub(1)) as u16;
            let result = SMP::addw_algorithm(smp.clone(), data_1s_complement);
            smp.borrow_mut().reg.set_ya(result);
        }
    }

    fn cmpw<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u16(smp.clone(), addr));
            let ya = smp.borrow().reg.get_ya();
            smp.borrow_mut().reg.psw.c = ya >= data;
            smp.borrow_mut().reg.psw.n = data > ya;
            smp.borrow_mut().reg.psw.z = ya == data;
        }
    }

    fn incw<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u16(smp.clone(), addr));
            let result = data.wrapping_add(1);
            smp.borrow_mut().reg.psw.n = (result >> 15) == 1;
            smp.borrow_mut().reg.psw.z = result == 0;
            yield_all!(SMP::write_u16(smp.clone(), addr, result));
        }
    }

    fn decw<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u16(smp.clone(), addr));
            let result = data.wrapping_sub(1);
            smp.borrow_mut().reg.psw.n = (result >> 15) == 1;
            smp.borrow_mut().reg.psw.z = result == 0;
            yield_all!(SMP::write_u16(smp.clone(), addr, result));
        }
    }

    fn div<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            // Algorithm from https://snesdev.mesen.ca/wiki/index.php?title=SPC700:
            let mut yva = smp.borrow().reg.get_ya() as u32;
            let x = (smp.borrow().reg.x as u32) << 9;
            for _ in 0..9 {
                yva = ((yva << 1) | (yva >> 16)) & 0x1FFFF;
                if yva >= x {
                    yva ^= 1;
                }
                if yva & 1 == 1 {
                    yva = yva.wrapping_sub(x) & 0x1FFFF;
                }
            }
            let new_y = ((yva >> 9) & 0xFF) as u8;
            let new_a = yva as u8;
            smp.borrow_mut().reg.y = new_y;
            smp.borrow_mut().reg.a = new_a;
            // "ZN are set based on A. V is set if YA/X>$FF (so the result won't fit in A).
            //  H is odd, it seems to get set based on X&$F<=Y&$F"
            smp.borrow_mut().reg.psw.v = (yva >> 8) & 1 == 1;
            smp.borrow_mut().reg.psw.n = (new_a >> 7) == 1;
            smp.borrow_mut().reg.psw.z = new_a == 0;
            let reg_x = smp.borrow().reg.x;
            smp.borrow_mut().reg.psw.h = (reg_x & 0xF) <= (new_y & 0xF);
        }
    }

    fn mul<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let product = (smp.borrow().reg.y as u16).wrapping_mul(smp.borrow().reg.a as u16);
            smp.borrow_mut().reg.set_ya(product);
            let new_y = smp.borrow().reg.y;
            smp.borrow_mut().reg.psw.n = (new_y >> 7) == 1;
            smp.borrow_mut().reg.psw.z = new_y == 0;
        }
    }

    // 1-bit instructions
    set_clear_bit_instrs!();

    // Generate branch instructions
    branch_instrs!();
    bit_branch_instrs!();

    fn bra<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let src_pc = smp.borrow().reg.pc;
            let offset = yield_all!(SMP::read_u8(smp.clone(), addr));
            let dest_pc = (src_pc as i32 + (offset as i8 as i32)) as u16;
            smp.borrow_mut().reg.pc = dest_pc;
            // TODO: Need to look into how many cycles branch can take
            // smp.borrow_mut().step(1);
        }
    }

    fn jmp<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            smp.borrow_mut().reg.pc = addr;
            // TODO: Need to look into how many cycles branch can take
            // smp.borrow_mut().step(1);
        }
    }

    fn push_pc<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<()> + 'a {
        move || {
            let src_pc = smp.borrow().reg.pc;
            yield_all!(SMP::stack_push_u8(smp.clone(), (src_pc >> 8) as u8));
            yield_all!(SMP::stack_push_u8(smp.clone(), src_pc as u8));
        }
    }

    fn pop_pc<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<()> + 'a {
        move || {
            let lo = yield_all!(SMP::stack_pop_u8(smp.clone())) as u16;
            let hi = yield_all!(SMP::stack_pop_u8(smp.clone())) as u16;
            smp.borrow_mut().reg.pc = (hi << 8) | lo;
        }
    }

    fn call<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            yield_all!(SMP::push_pc(smp.clone()));
            smp.borrow_mut().reg.pc = addr;
            // TODO: Need to look into how many cycles branch can take
            // smp.borrow_mut().step(1);
        }
    }

    fn pcall<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            yield_all!(SMP::push_pc(smp.clone()));
            smp.borrow_mut().reg.pc = 0xFF00 | (addr & 0xFF);
            // TODO: Need to look into how many cycles branch can take
            // smp.borrow_mut().step(1);
        }
    }

    fn ret<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            yield_all!(SMP::pop_pc(smp.clone()));
            // TODO: Need to look into how many cycles branch can take
            // smp.borrow_mut().step(1);
        }
    }

    fn ret_from_interrupt<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            yield_all!(SMP::pop_psw(smp.clone()));
            yield_all!(SMP::pop_pc(smp.clone()));
            // TODO: Need to look into how many cycles branch can take
            // smp.borrow_mut().step(1);
        }
    }

    fn dbnz_y<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let y = smp.borrow().reg.y;
            smp.borrow_mut().reg.y = y.wrapping_sub(1);
            let src_pc = smp.borrow().reg.pc;
            let offset = yield_all!(SMP::read_u8(smp.clone(), addr));
            let dest_pc = (src_pc as i32 + (offset as i8 as i32)) as u16;
            if smp.borrow().reg.y != 0 {
                smp.borrow_mut().reg.pc = dest_pc;
            }
        }
    }

    fn cbne<'a>(smp: Rc<RefCell<SMP>>, addrs: MemToMemAddresses) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr));
            let src_pc = smp.borrow().reg.pc;
            let offset = yield_all!(SMP::read_u8(smp.clone(), addrs.dest_addr));
            let dest_pc = (src_pc as i32 + (offset as i8 as i32)) as u16;
            if smp.borrow().reg.a != data {
                smp.borrow_mut().reg.pc = dest_pc;
            }
        }
    }

    fn dbnz_mem<'a>(
        smp: Rc<RefCell<SMP>>,
        addrs: MemToMemAddresses,
    ) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr)).wrapping_sub(1);
            yield_all!(SMP::write_u8(smp.clone(), addrs.src_addr, data));
            let src_pc = smp.borrow().reg.pc;
            let offset = yield_all!(SMP::read_u8(smp.clone(), addrs.dest_addr));
            let dest_pc = (src_pc as i32 + (offset as i8 as i32)) as u16;
            if data != 0 {
                smp.borrow_mut().reg.pc = dest_pc;
            }
        }
    }

    // Flag instructions
    flag_instrs!();

    fn flip_flag_c<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            dummy_yield!();
            let c = smp.borrow_mut().reg.psw.c;
            smp.borrow_mut().reg.psw.c = !c;
        }
    }

    // Push/pop instructions
    push_pop_instrs!();

    fn push_psw<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            let data = smp.borrow().reg.psw.get();
            yield_all!(SMP::stack_push_u8(smp.clone(), data));
        }
    }

    fn pop_psw<'a>(smp: Rc<RefCell<SMP>>) -> impl InstructionGenerator + 'a {
        move || {
            let data = yield_all!(SMP::stack_pop_u8(smp.clone()));
            smp.borrow_mut().reg.psw.set(data);
        }
    }
}
