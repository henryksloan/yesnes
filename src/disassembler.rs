pub mod instruction_data;

pub use instruction_data::{AddressingMode, Instruction, InstructionData, INSTRUCTION_DATA};

use log::info;
use std::cell::RefCell;
use std::rc::Rc;

use crate::bus::Bus;
use crate::cpu;
use crate::u24::u24;

pub struct RegisterState {
    pub m: bool,
    pub x: bool,
    pub e: bool,
}

impl RegisterState {
    pub fn new() -> Self {
        Self {
            m: true,
            x: true,
            e: false,
        }
    }
}

#[derive(Clone, Copy)]
pub struct DisassemblerInstruction {
    pub instruction_data: InstructionData,
    pub operand: u32,
    new_m_flag: Option<bool>,
    new_x_flag: Option<bool>,
    // The XCE instruction can either set or clear the E flag. Differentiating
    // sets and clears would require tracking the C flag, which is impossible in
    // static analysis. Therefore, we assume XCEs just clear E, which is usually true.
    assume_clears_e: bool,
}

impl DisassemblerInstruction {
    pub fn new(instruction_data: InstructionData, operand: u32) -> Self {
        let (new_m_flag, new_x_flag) = match instruction_data.instruction {
            Instruction::REP => {
                let resets_x = (operand >> 4) & 1 == 1;
                let resets_m = (operand >> 5) & 1 == 1;
                (resets_m.then_some(false), resets_x.then_some(false))
            }
            Instruction::SEP => {
                let sets_x = (operand >> 4) & 1 == 1;
                let sets_m = (operand >> 5) & 1 == 1;
                (sets_m.then_some(true), sets_x.then_some(true))
            }
            _ => (None, None),
        };
        DisassemblerInstruction {
            instruction_data,
            operand,
            new_m_flag,
            new_x_flag,
            assume_clears_e: matches!(instruction_data.instruction, Instruction::XCE),
        }
    }

    pub fn update_register_state(&self, register_state: &mut RegisterState) {
        if let Some(new_m_flag) = self.new_m_flag {
            register_state.m = new_m_flag;
        }
        if let Some(new_x_flag) = self.new_x_flag {
            register_state.x = new_x_flag;
        }
        if self.assume_clears_e {
            register_state.e = false;
        }
    }
}

#[derive(Clone, Copy)]
pub struct DisassemblerEntry {
    pub addr: u24,
    pub instruction: DisassemblerInstruction,
}

pub struct Disassembler {
    bus: Rc<RefCell<Bus>>,
    disassembly_cache: Vec<Option<DisassemblerInstruction>>,
    disassembly_result: Vec<DisassemblerEntry>,
}

impl Disassembler {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self {
            bus,
            disassembly_cache: vec![None; 1 << 24],
            disassembly_result: Vec::new(),
        }
    }

    pub fn disassemble(&mut self) {
        let pc = u24(Bus::peak_u16(self.bus.clone(), cpu::RESET_VECTOR) as u32);
        // DO NOT SUBMIT: Also analyze other vectors
        self.analyze(pc, &mut RegisterState::new());
        for (addr, instruction) in self.disassembly_cache.iter().enumerate() {
            if let Some(instruction) = instruction {
                self.disassembly_result.push(DisassemblerEntry {
                    addr: u24(addr as u32),
                    instruction: *instruction,
                })
            }
        }
    }

    // DO NOT SUBMIT: Should recurse on jumps, calls, etc.
    // DO NOT SUBMIT: Instructions may push P.
    fn analyze(&mut self, start_addr: u24, register_state: &mut RegisterState) {
        let mut addr = start_addr;
        loop {
            if self.disassembly_cache[addr.raw()].is_some() {
                return;
            }
            let opcode = Bus::peak_u8(self.bus.clone(), addr);
            let instr = INSTRUCTION_DATA[opcode as usize];
            let operand_bytes = instr.mode.operand_bytes(&register_state);
            // info!("{:?} ({})", instr, operand_bytes);
            let mut operand: u32 = 0;
            for i in 0..operand_bytes {
                let operand_byte_offset = operand_bytes - i;
                operand <<= 8;
                operand |= Bus::peak_u8(self.bus.clone(), addr + operand_byte_offset as u32) as u32;
            }
            let disassembled = DisassemblerInstruction::new(instr, operand);
            disassembled.update_register_state(register_state);
            self.disassembly_cache[addr.raw()] = Some(disassembled);
            addr += (instr.mode.operand_bytes(register_state) + 1) as u32;
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
