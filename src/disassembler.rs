// Some of the design ideas for this disassembler come from bsnes-plus and Higan-S
pub mod instruction_data;

pub use instruction_data::{AddressingMode, Instruction, InstructionData, INSTRUCTION_DATA};

use std::cell::RefCell;
use std::rc::Rc;

use crate::bus::Bus;
use crate::cpu::{self, StatusRegister};
use crate::u24::u24;

#[derive(Clone, Copy)]
struct RegisterState {
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

    pub fn from_status_register(status_register: &StatusRegister) -> Self {
        Self {
            m: status_register.m,
            x: status_register.x_or_b,
            e: status_register.e,
        }
    }
}

#[derive(Clone)]
struct AnalysisState {
    pub reg: RegisterState,
    pub p_stack: Vec<RegisterState>,
}

impl AnalysisState {
    pub fn new() -> Self {
        Self {
            reg: RegisterState::new(),
            p_stack: Vec::new(),
        }
    }

    pub fn from_status_register(status_register: &StatusRegister) -> Self {
        Self {
            reg: RegisterState::from_status_register(status_register),
            p_stack: Vec::new(),
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
    pub(self) fn new(instruction_data: InstructionData, operand: u32) -> Self {
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

    pub(self) fn update_analysis_state(&self, analysis_state: &mut AnalysisState) {
        if let Some(new_m_flag) = self.new_m_flag {
            analysis_state.reg.m = new_m_flag;
        }
        if let Some(new_x_flag) = self.new_x_flag {
            analysis_state.reg.x = new_x_flag;
        }
        if self.assume_clears_e {
            analysis_state.reg.e = false;
        }
        if matches!(self.instruction_data.instruction, Instruction::PHP) {
            analysis_state.p_stack.push(analysis_state.reg);
        }
        if matches!(self.instruction_data.instruction, Instruction::PLP) {
            if let Some(popped) = analysis_state.p_stack.pop() {
                analysis_state.reg = popped;
            }
        }
    }

    pub fn is_conditional_branch(&self) -> bool {
        use Instruction::*;
        matches!(
            self.instruction_data.instruction,
            BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS
        )
    }

    pub fn is_unconditional_branch(&self) -> bool {
        use Instruction::*;
        matches!(self.instruction_data.instruction, BRA | BRL | JMP | JML)
    }

    pub fn is_call(&self) -> bool {
        use Instruction::*;
        matches!(self.instruction_data.instruction, JSR | JSL)
    }

    pub fn is_return(&self) -> bool {
        use Instruction::*;
        matches!(self.instruction_data.instruction, RTI | RTL | RTS)
    }

    pub fn is_indirect(&self) -> bool {
        self.instruction_data.mode.is_indirect()
    }

    pub fn halts(&self) -> bool {
        use Instruction::*;
        matches!(self.instruction_data.instruction, BRK | STP)
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
        // DO NOT SUBMIT: Also analyze other vectors. Can/do any vectors contain 65C816-mode code?
        self.analyze(pc, &mut AnalysisState::new());
        self.update_disassembly();
    }

    pub fn update_disassembly_at(&mut self, addr: u24, status_register: &StatusRegister) {
        if self.disassembly_cache[addr.raw()].is_none() {
            self.analyze(
                addr,
                &mut AnalysisState::from_status_register(status_register),
            );
            self.update_disassembly();
        }
    }

    pub fn update_disassembly(&mut self) {
        self.disassembly_result.clear();
        for (addr, instruction) in self.disassembly_cache.iter().enumerate() {
            if let Some(instruction) = instruction {
                self.disassembly_result.push(DisassemblerEntry {
                    addr: u24(addr as u32),
                    instruction: *instruction,
                })
            }
        }
    }

    fn analyze(&mut self, start_addr: u24, analysis_state: &mut AnalysisState) {
        let mut addr = start_addr;
        loop {
            if self.disassembly_cache[addr.raw()].is_some() {
                return;
            }
            let opcode = Bus::peak_u8(self.bus.clone(), addr);
            let instr = INSTRUCTION_DATA[opcode as usize];
            let operand_bytes = instr.mode.operand_bytes(&analysis_state);
            let mut operand: u32 = 0;
            for i in 0..operand_bytes {
                let operand_byte_offset = operand_bytes - i;
                operand <<= 8;
                operand |= Bus::peak_u8(self.bus.clone(), addr + operand_byte_offset as u32) as u32;
            }
            let disassembled = DisassemblerInstruction::new(instr, operand);
            disassembled.update_analysis_state(analysis_state);
            self.disassembly_cache[addr.raw()] = Some(disassembled);

            if disassembled.halts() {
                break;
            }

            if disassembled.is_conditional_branch() {
                // TODO: More general solution to addressing modes in analyze()
                let offset = match disassembled.instruction_data.mode {
                    AddressingMode::PcRelative => operand as i8 as i32 + 2,
                    AddressingMode::PcRelativeLong => operand as i16 as i32 + 3,
                    _ => panic!(
                        "Unexpected addressing mode for {:?}: {:?}",
                        disassembled.instruction_data.instruction,
                        disassembled.instruction_data.mode
                    ),
                };
                let dest_addr = u24((addr.raw() as i32 + offset) as u32);
                // Take the branch with a copy of the analysis state.
                // Branches don't "return", so afterwards we proceed as if the condition has failed.
                self.analyze(dest_addr, &mut analysis_state.clone());
            } else if disassembled.is_call() && !disassembled.is_indirect() {
                // Send the analysis state as a reference. Calls return, so
                // modifications to registers should be preserved.
                // TODO: More general solution to addressing modes in analyze()
                // TODO: This logic isn't totally sound. For example, if we
                // already analyzed the code, we won't correctly modify registers.
                self.analyze(u24(operand), analysis_state);
            }

            if disassembled.is_unconditional_branch() && !disassembled.is_indirect() {
                // No need to recurse for an unconditional branch, as there's only one execution path.
                // TODO: More general solution to addressing modes in analyze()
                addr = match disassembled.instruction_data.mode {
                    AddressingMode::PcRelative => {
                        u24((addr.raw() as i32 + operand as i8 as i32 + 2) as u32)
                    }
                    AddressingMode::PcRelativeLong => {
                        u24((addr.raw() as i32 + operand as i16 as i32 + 3) as u32)
                    }
                    AddressingMode::Absolute | AddressingMode::AbsoluteLong => u24(operand),
                    _ => panic!(
                        "Unexpected addressing mode for {:?}: {:?}",
                        disassembled.instruction_data.instruction,
                        disassembled.instruction_data.mode
                    ),
                };
            } else if disassembled.is_return()
                || (disassembled.is_indirect()
                    && (disassembled.is_call() || disassembled.is_unconditional_branch()))
            {
                // We either hit a return or an indirect call/branch. Either way, we can't
                // correctly disassemble past this point.
                break;
            } else {
                addr += (instr.mode.operand_bytes(analysis_state) + 1) as u32;
            }
        }
    }

    pub fn get_line_index(&self, addr: u24) -> usize {
        let mut prev_addr = u24(0);
        for (line_i, entry) in self.disassembly_result.iter().enumerate() {
            if addr == entry.addr {
                return line_i;
            } else if prev_addr < addr && addr < entry.addr {
                return line_i.saturating_sub(1);
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
