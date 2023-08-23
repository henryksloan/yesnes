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
                    let output = Self::[<$op _algorithm>](smp.clone(), src_data, smp.borrow().reg.a);
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
            _ => panic!("Invalid SMP opcode {:#04X} at {:#06X}", opcode_val, $smp_rc.borrow().reg.pc),
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
}

impl SMP {
    pub fn new() -> Self {
        Self {
            reg: Registers::new(),
            io_reg: IoRegisters::new(),
            ticks_run: 0,
            ram: vec![0; 0x10000],
        }
    }

    pub fn registers(&self) -> &Registers {
        &self.reg
    }

    pub fn registers_mut(&mut self) -> &mut Registers {
        &mut self.reg
    }

    pub fn reset(smp: Rc<RefCell<SMP>>) {
        smp.borrow_mut().ticks_run = 0;
        smp.borrow_mut().reg = Registers::new();
        // TODO: Simplify
        smp.borrow_mut().reg.pc = {
            let lo = {
                let mut gen = Self::read_u8(smp.clone(), RESET_VECTOR);
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u16,
                        _ => {}
                    }
                }
            };
            let hi = {
                let mut gen = Self::read_u8(smp.clone(), RESET_VECTOR + 1);
                loop {
                    match Pin::new(&mut gen).resume(()) {
                        GeneratorState::Complete(out) => break out as u16,
                        _ => {}
                    }
                }
            };
            (hi << 8) | lo
        };
        smp.borrow_mut().reg.sp = 0xEF;
    }

    pub fn run<'a>(smp: Rc<RefCell<SMP>>) -> impl DeviceGenerator + 'a {
        move || loop {
            println!("SMP");
            let opcode = yield_ticks!(smp, SMP::read_u8(smp.clone(), smp.borrow().reg.pc));
            smp.borrow_mut().reg.pc += 1;

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
                (store_a; 0xC4=>direct, 0xD4=>direct_x, 0xC5=>absolute, 0xD5=>absolute_x,
                 0xD6=>absolute_y, 0xAF=>indirect_increment, 0xC6=>indirect,
                 0xD7=>indirect_indexed, 0xC7=>indexed_indirect)
                (store_x; 0xD8=>direct, 0xD9=>direct_y)
                (store_y; 0xCB=>direct, 0xCC=>direct_x)
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
            );
        }
    }

    fn step(&mut self, n_clocks: u64) {
        self.ticks_run += n_clocks;
    }

    fn read_io_reg<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: I think this should maybe yield to CPU for some (all?) of these?
            dummy_yield!();
            match addr {
                0x00F0..=0x00F3 => todo!(),
                0x00F4..=0x00F7 => smp.borrow().io_reg.ports[addr as usize - 0x00F4],
                0x00F8..=0x00FF => todo!(),
                _ => panic!("Address {:#02X} is not an SMP IO register", addr),
            }
        }
    }

    fn write_io_reg<'a>(smp: Rc<RefCell<SMP>>, addr: u16, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            // TODO: I think this should maybe yield to CPU for some (all?) of these?
            dummy_yield!();
            match addr {
                0x00F0..=0x00F3 => todo!(),
                0x00F4..=0x00F7 => smp.borrow_mut().io_reg.ports[addr as usize - 0x00F4] = data,
                0x00F8..=0x00FF => todo!(),
                _ => panic!("Address {:#02X} is not an SMP IO register", addr),
            }
        }
    }

    fn read_u8<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl Yieldable<u8> + 'a {
        move || {
            // TODO: Could this be more granular? Does every access need to sync?
            yield YieldReason::Sync(Device::CPU);
            let data = match addr {
                0x0000..=0x00EF => smp.borrow().ram[addr as usize],
                0x00F0..=0x00FF => yield_all!(SMP::read_io_reg(smp.clone(), addr)),
                0x0100..=0xFFBF => smp.borrow_mut().ram[addr as usize],
                // TODO: Use control register to determine RAM/ROM
                0xFFC0..=0xFFFF => BOOT_ROM[addr as usize - 0xFFC0],
            };
            // TODO: Some clock cycles before the read, depending on region
            smp.borrow_mut().step(1);
            data
        }
    }

    fn write_u8<'a>(smp: Rc<RefCell<SMP>>, addr: u16, data: u8) -> impl Yieldable<()> + 'a {
        move || {
            // TODO: Could this be more granular? Does every access need to sync?
            yield YieldReason::Sync(Device::CPU);
            match addr {
                0x0000..=0x00EF => smp.borrow_mut().ram[addr as usize] = data,
                0x00F0..=0x00FF => yield_all!(SMP::write_io_reg(smp.clone(), addr, data)),
                0x0100..=0xFFBF => smp.borrow_mut().ram[addr as usize] = data,
                // TODO: Use control register to determine RAM/ROM
                0xFFC0..=0xFFFF => smp.borrow_mut().ram[addr as usize] = data,
            }
            // TODO: Some clock cycles before the write, depending on region
            smp.borrow_mut().step(4);
        }
    }

    pub fn io_peak(&self, addr: u16) -> u8 {
        // TODO
        0
    }

    pub fn io_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2140..=0x2143 => self.io_reg.ports[addr as usize - 0x2140],
            _ => panic!("Invalid IO read of SMP at {addr:#06X}"),
        }
    }

    pub fn io_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2140..=0x2143 => self.io_reg.ports[addr as usize - 0x2140] = data,
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
            let direct_addr = direct_page_base + fetch!(smp) as u16 + smp.borrow().reg.x as u16;
            direct_addr
        }
    }

    fn direct_y<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let direct_addr = direct_page_base + fetch!(smp) as u16 + smp.borrow().reg.y as u16;
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
            smp.borrow_mut().reg.x += x.wrapping_add(1);
            direct_addr
        }
    }

    fn indirect_indexed<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let indirect_addr = direct_page_base + fetch!(smp) as u16;
            let direct_addr = yield_all!(SMP::read_u8(smp.clone(), indirect_addr)) as u16;
            direct_addr + smp.borrow().reg.y as u16
        }
    }

    fn indexed_indirect<'a>(smp: Rc<RefCell<SMP>>) -> impl Yieldable<u16> + 'a {
        move || {
            dummy_yield!();
            let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
            let indirect_addr = direct_page_base + fetch!(smp) as u16 + smp.borrow().reg.x as u16;
            let direct_addr = yield_all!(SMP::read_u8(smp.clone(), indirect_addr)) as u16;
            direct_addr
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
            let src_addr = yield_all!(Self::direct(smp.clone()));
            let dest_addr = {
                let direct_page_base = smp.borrow().reg.psw.direct_page_addr();
                direct_page_base + smp.borrow().reg.y as u16
            };
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
            let data = yield_all!(SMP::read_u8(smp.clone(), addrs.src_addr));
            yield_all!(SMP::write_u8(smp.clone(), addrs.dest_addr, data));
        }
    }

    // OR, AND, EOR, CMP, ADC, and SBC support some memory->memory operations;
    // these macros generate both the mem->mem and the accumulator implemetations.
    alu_instr!(or);
    alu_instr!(and);
    alu_instr!(eor);
    alu_instr!(cmp);
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

    fn cmp_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, dest_data: u8) -> u8 {
        let difference = {
            let temp = dest_data as i16 - src_data as i16;
            smp.borrow_mut().reg.psw.c = temp > 0xFF;
            smp.borrow_mut().reg.psw.n = (temp >> 7) == 1;
            smp.borrow_mut().reg.psw.z = temp == 0;
            temp as u8
        };
        difference
    }

    fn adc_algorithm(smp: Rc<RefCell<SMP>>, src_data: u8, dest_data: u8) -> u8 {
        let sum = {
            let carry = smp.borrow().reg.psw.c as i16;
            let temp = dest_data as i16 + src_data as i16 + carry;
            smp.borrow_mut().reg.psw.c = temp > 0xFF;
            smp.borrow_mut().reg.psw.h = ((dest_data & 0xF) + (src_data & 0xF) + carry as u8) > 0xF;
            smp.borrow_mut().reg.psw.n = (temp >> 7) == 1;
            smp.borrow_mut().reg.psw.z = temp == 0;
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

    fn cmp_x<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let src_data = yield_all!(SMP::read_u8(smp.clone(), addr));
            let output = Self::cmp_algorithm(smp.clone(), src_data, smp.borrow().reg.x);
            smp.borrow_mut().reg.x = output;
        }
    }

    fn cmp_y<'a>(smp: Rc<RefCell<SMP>>, addr: u16) -> impl InstructionGenerator + 'a {
        move || {
            let src_data = yield_all!(SMP::read_u8(smp.clone(), addr));
            let output = Self::cmp_algorithm(smp.clone(), src_data, smp.borrow().reg.y);
            smp.borrow_mut().reg.y = output;
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
}
