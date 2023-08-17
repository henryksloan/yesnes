pub mod instruction_data;

pub use instruction_data::{AddressingMode, InstructionData, INSTRUCTION_DATA};

use log::info;
use std::cell::RefCell;
use std::rc::Rc;

use crate::bus::Bus;
use crate::cpu;
use crate::u24::u24;

pub struct RegisterState {
    pub m: bool,
    pub x: bool,
}

impl RegisterState {
    pub fn new() -> Self {
        Self { m: false, x: false }
    }
}

#[derive(Clone, Copy)]
pub struct DisassemblerInstruction {
    pub instruction_data: InstructionData,
    pub operand: u32,
}

#[derive(Clone, Copy)]
pub struct DisassemblerEntry {
    pub addr: u24,
    pub instruction: DisassemblerInstruction,
}

pub struct Disassembler {
    bus: Rc<RefCell<Bus>>,
    register_state: RegisterState,
    disassembly_cache: Vec<Option<DisassemblerInstruction>>,
    disassembly_result: Vec<DisassemblerEntry>,
}

impl Disassembler {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self {
            bus,
            register_state: RegisterState::new(),
            disassembly_cache: vec![None; 1 << 24],
            disassembly_result: Vec::new(),
        }
    }

    pub fn disassemble(&mut self) {
        let pc = u24(Bus::peak_u16(self.bus.clone(), cpu::RESET_VECTOR) as u32);
        // DO NOT SUBMIT: Also analyze other vectors
        self.analyze(pc);
        for (addr, instruction) in self.disassembly_cache.iter().enumerate() {
            if let Some(instruction) = instruction {
                self.disassembly_result.push(DisassemblerEntry {
                    addr: u24(addr as u32),
                    instruction: *instruction,
                })
            }
        }
    }

    // DO NOT SUBMIT: This needs to be recursive with a state parameter.
    // DO NOT SUBMIT: Should recurse jumps, calls, etc.
    // DO NOT SUBMIT: Instructions may modify X/M/E, or push P.
    fn analyze(&mut self, start_addr: u24) {
        let mut addr = start_addr;
        loop {
            if self.disassembly_cache[addr.raw()].is_some() {
                return;
            }
            let opcode = Bus::peak_u8(self.bus.clone(), addr);
            let instr = INSTRUCTION_DATA[opcode as usize];
            let operand_bytes = instr.mode.operand_bytes(&self.register_state);
            // info!("{:?} ({})", instr, operand_bytes);
            let mut operand: u32 = 0;
            for i in 0..operand_bytes {
                let operand_byte_offset = operand_bytes - i;
                operand <<= 8;
                operand |= Bus::peak_u8(self.bus.clone(), addr + operand_byte_offset as u32) as u32;
            }
            self.disassembly_cache[addr.raw()] = Some(DisassemblerInstruction {
                instruction_data: instr,
                operand,
            });
            addr += (instr.mode.operand_bytes(&self.register_state) + 1) as u32;
            // Check for overflow
            if addr <= start_addr {
                return;
            }
        }
    }

    pub fn get_line_index(&self, addr: u24) -> usize {
        let mut prev_addr = u24(0);
        for (line_i, entry) in self.disassembly_result.iter().enumerate() {
            if addr == entry.addr {
                return line_i;
            } else if prev_addr < addr && addr < entry.addr {
                return line_i - 1;
            }
            prev_addr = entry.addr;
        }
        0
    }

    pub fn get_line(&self, line_i: usize) -> DisassemblerEntry {
        self.disassembly_result[line_i]
    }

    pub fn get_num_lines(&self) -> usize {
        self.disassembly_result.len()
    }
}
