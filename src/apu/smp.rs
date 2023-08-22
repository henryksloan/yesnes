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
            _ => panic!("Invalid SMP opcode {:#02X}", opcode_val),
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
                (load_a; 0xE8=>immediate, 0xE4=>direct, 0xF4=>direct_x,
                 0xE5=>absolute, 0xF5=>absolute_x, 0xF6=>absolute_y,
                 0xE6=>indirect, 0xBF=>indirect_increment,
                 0xF7=>indirect_indexed, 0xE7=>indexed_indirect)
                (load_x; 0xF8=>direct, 0xF9=>direct_y, 0xE9=>absolute)
                (load_y; 0xEB=>direct, 0xFB=>direct_x, 0xEC=>absolute)
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

    transfer_instrs!();
    load_instrs!();
}
