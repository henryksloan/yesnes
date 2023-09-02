// Some of the design ideas for this disassembler come from bsnes-plus and Higan-S
pub mod instruction_data;

pub use instruction_data::cpu_instructions::{
    AddressingMode, CpuInstructionData, Instruction, CPU_INSTRUCTION_DATA,
};

use std::cell::RefCell;
use std::rc::Rc;

use crate::bus::Bus;
use crate::cpu::{self, StatusRegister};
use crate::u24::u24;

#[derive(Clone, Copy)]
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

    pub fn from_status_register(status_register: &StatusRegister) -> Self {
        Self {
            m: status_register.m,
            x: status_register.x_or_b,
            e: status_register.e,
        }
    }
}

#[derive(Clone)]
pub struct CpuAnalysisState {
    pub reg: RegisterState,
    pub p_stack: Vec<RegisterState>,
}

impl Default for CpuAnalysisState {
    fn default() -> Self {
        Self {
            reg: RegisterState::new(),
            p_stack: Vec::new(),
        }
    }
}

impl From<&StatusRegister> for CpuAnalysisState {
    fn from(p: &StatusRegister) -> Self {
        Self {
            reg: RegisterState::from_status_register(p),
            p_stack: Vec::new(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct CpuDisassembledInstruction {
    pub instruction_data: CpuInstructionData,
    pub operand: u32,
    new_m_flag: Option<bool>,
    new_x_flag: Option<bool>,
    // The XCE instruction can either set or clear the E flag. Differentiating
    // sets and clears would require tracking the C flag, which is impossible in
    // static analysis. Therefore, we assume XCEs just clear E, which is usually true.
    assume_clears_e: bool,
}

impl CpuDisassembledInstruction {
    pub(self) fn new(instruction_data: CpuInstructionData, operand: u32) -> Self {
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
        CpuDisassembledInstruction {
            instruction_data,
            operand,
            new_m_flag,
            new_x_flag,
            assume_clears_e: matches!(instruction_data.instruction, Instruction::XCE),
        }
    }

    pub(self) fn update_analysis_state(&self, analysis_state: &mut CpuAnalysisState) {
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

pub enum AnalysisStep<Address> {
    Break,
    BranchAndContinue(Address),
    Call(Address),
    Branch(Address),
    Continue,
}

impl DisassembledInstruction<CpuAnalysisState, CpuInstructionData> for CpuDisassembledInstruction {
    fn mnemonic(&self) -> String {
        self.instruction_data.mnemonic()
    }

    fn from_instruction_data(data: CpuInstructionData, operand: u32) -> Self {
        Self::new(data, operand)
    }

    fn update_analysis_state(&self, analysis_state: &mut CpuAnalysisState) {
        self.update_analysis_state(analysis_state);
    }
}

pub trait InstructionData<State> {
    fn operand_bytes(&self, analysis_state: &State) -> usize;
}

pub trait DisassembledInstruction<State, Data: InstructionData<State>> {
    fn mnemonic(&self) -> String;
    fn from_instruction_data(data: Data, operand: u32) -> Self;
    fn update_analysis_state(&self, analysis_state: &mut State);
}

// TODO: Figure out a way to make this not public
pub trait DebugProcessor {
    type Address: Into<usize> + From<usize> + Copy;
    type AnalysisState: Default + Clone;
    type Decoded: InstructionData<Self::AnalysisState> + Copy;
    type Disassembled: DisassembledInstruction<Self::AnalysisState, Self::Decoded> + Clone + Copy;

    const ADDR_SPACE_SIZE: usize;
    const INSTRUCTION_DATA: [Self::Decoded; 256];

    fn entry_points(&self) -> Vec<Self::Address>;
    fn peak_u8(&self, addr: Self::Address) -> u8;
    fn analysis_step(
        addr: Self::Address,
        disassembled: &Self::Disassembled,
    ) -> AnalysisStep<Self::Address>;
}

pub struct DebugCpu {
    bus: Rc<RefCell<Bus>>,
}

impl DebugCpu {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self { bus }
    }
}

// TODO: Need to check if this is the right struct on which to implement this
unsafe impl Send for DebugCpu {}

impl DebugProcessor for DebugCpu {
    type Address = u24;
    type AnalysisState = CpuAnalysisState;
    type Decoded = CpuInstructionData;
    type Disassembled = CpuDisassembledInstruction;

    const ADDR_SPACE_SIZE: usize = 1 << 24;
    const INSTRUCTION_DATA: [Self::Decoded; 256] = CPU_INSTRUCTION_DATA;

    fn entry_points(&self) -> Vec<u24> {
        // TODO: Also analyze other vectors. The 0xFFF* vectors are emu mode, the 0xFFE* vectors are not.
        vec![u24(
            Bus::peak_u16(self.bus.clone(), cpu::RESET_VECTOR) as u32
        )]
    }

    fn peak_u8(&self, addr: u24) -> u8 {
        Bus::peak_u8(self.bus.clone(), addr)
    }

    fn analysis_step(
        addr: Self::Address,
        disassembled: &Self::Disassembled,
    ) -> AnalysisStep<Self::Address> {
        if disassembled.halts() {
            return AnalysisStep::Break;
        }

        let operand = disassembled.operand;

        // TODO: This logic can be simplified now that we have AnalysisStep
        if disassembled.is_conditional_branch() {
            // TODO: More general solution to addressing modes in analyze()
            let offset = match disassembled.instruction_data.mode {
                AddressingMode::PcRelative => operand as i8 as i32 + 2,
                AddressingMode::PcRelativeLong => operand as i16 as i32 + 3,
                _ => panic!(
                    "Unexpected addressing mode for {:?}: {:?}",
                    disassembled.instruction_data.instruction, disassembled.instruction_data.mode
                ),
            };
            let dest_addr = u24((addr.raw() as i32 + offset) as u32);
            // Take the branch with a copy of the analysis state.
            // Branches don't "return", so afterwards we proceed as if the condition has failed.
            return AnalysisStep::BranchAndContinue(dest_addr);
        } else if disassembled.is_call() && !disassembled.is_indirect() {
            // Send the analysis state as a reference. Calls return, so
            // modifications to registers should be preserved.
            // TODO: More general solution to addressing modes in analyze()
            // TODO: This logic isn't totally sound. For example, if we
            // already analyzed the code, we won't correctly modify registers.
            return AnalysisStep::Call(u24(operand));
        }

        if disassembled.is_unconditional_branch() && !disassembled.is_indirect() {
            // No need to recurse for an unconditional branch, as there's only one execution path.
            // TODO: More general solution to addressing modes in analyze()
            let dest_addr = match disassembled.instruction_data.mode {
                AddressingMode::PcRelative => {
                    u24((addr.raw() as i32 + operand as i8 as i32 + 2) as u32)
                }
                AddressingMode::PcRelativeLong => {
                    u24((addr.raw() as i32 + operand as i16 as i32 + 3) as u32)
                }
                AddressingMode::Absolute | AddressingMode::AbsoluteLong => u24(operand),
                _ => panic!(
                    "Unexpected addressing mode for {:?}: {:?}",
                    disassembled.instruction_data.instruction, disassembled.instruction_data.mode
                ),
            };
            return AnalysisStep::Branch(dest_addr);
        } else if disassembled.is_return()
            || (disassembled.is_indirect()
                && (disassembled.is_call() || disassembled.is_unconditional_branch()))
        {
            // We either hit a return or an indirect call/branch. Either way, we can't
            // correctly disassemble past this point.
            return AnalysisStep::Break;
        } else {
            return AnalysisStep::Continue;
        }
    }
}

pub struct Disassembler<D: DebugProcessor> {
    debug_processor: D,
    disassembly_cache: Vec<Option<D::Disassembled>>,
    disassembly_result: Vec<(usize, D::Disassembled)>,
}

impl<D: DebugProcessor> Disassembler<D> {
    pub fn new(debug_processor: D) -> Self {
        Self {
            debug_processor,
            disassembly_cache: vec![None; D::ADDR_SPACE_SIZE],
            disassembly_result: Vec::new(),
        }
    }

    pub fn disassemble(&mut self) {
        for entry_point in self.debug_processor.entry_points() {
            self.analyze(entry_point, &mut D::AnalysisState::default());
        }
        self.update_disassembly();
    }

    pub fn update_disassembly_at(&mut self, addr: D::Address, state: impl Into<D::AnalysisState>) {
        if self.disassembly_cache[addr.into()].is_none() {
            self.analyze(addr, &mut state.into());
            self.update_disassembly();
        }
    }

    pub fn update_disassembly(&mut self) {
        self.disassembly_result.clear();
        for (addr, instruction) in self.disassembly_cache.iter().enumerate() {
            if let Some(instruction) = instruction {
                self.disassembly_result.push((addr, *instruction));
            }
        }
    }

    fn analyze(&mut self, start_addr: D::Address, analysis_state: &mut D::AnalysisState) {
        let mut addr = start_addr;
        loop {
            if self.disassembly_cache[addr.into()].is_some() {
                return;
            }
            let opcode = self.debug_processor.peak_u8(addr);
            let decoded = D::INSTRUCTION_DATA[opcode as usize];
            let operand_bytes = decoded.operand_bytes(analysis_state);
            let mut operand: u32 = 0;
            for i in 0..operand_bytes {
                let operand_byte_offset = operand_bytes - i;
                operand <<= 8;
                operand |= self
                    .debug_processor
                    .peak_u8(D::Address::from(addr.into() + operand_byte_offset))
                    as u32;
            }
            let disassembled = D::Disassembled::from_instruction_data(decoded, operand);
            disassembled.update_analysis_state(analysis_state);
            self.disassembly_cache[addr.into()] = Some(disassembled);

            match D::analysis_step(addr, &disassembled) {
                AnalysisStep::Break => break,
                AnalysisStep::BranchAndContinue(dest_addr) => {
                    self.analyze(dest_addr, &mut analysis_state.clone());
                    // TODO: Factor out this address change
                    addr = D::Address::from(addr.into() + (usize::from(operand_bytes) + 1));
                }
                AnalysisStep::Call(dest_addr) => {
                    self.analyze(dest_addr, analysis_state);
                    addr = D::Address::from(addr.into() + (usize::from(operand_bytes) + 1));
                }
                AnalysisStep::Branch(dest_addr) => addr = dest_addr,
                AnalysisStep::Continue => {
                    addr = D::Address::from(addr.into() + (usize::from(operand_bytes) + 1));
                }
            }
        }
    }

    pub fn get_line_index(&self, addr: D::Address) -> usize {
        let addr_usize: usize = addr.into();
        let mut prev_addr: usize = 0;
        for (line_i, entry) in self.disassembly_result.iter().enumerate() {
            if addr_usize == entry.0 {
                return line_i;
            } else if prev_addr < addr_usize && addr_usize < entry.0 {
                return line_i.saturating_sub(1);
            }
            prev_addr = entry.0;
        }
        0
    }

    pub fn get_line(&self, line_i: usize) -> (usize, D::Disassembled) {
        self.disassembly_result[line_i]
    }

    pub fn get_num_lines(&self) -> usize {
        self.disassembly_result.len()
    }
}
