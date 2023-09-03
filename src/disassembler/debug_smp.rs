pub use super::instruction_data::smp_instructions::{
    SmpAddressingMode, SmpInstruction, SmpInstructionData, SMP_INSTRUCTION_DATA,
};

use super::{AnalysisStep, DebugProcessor, DisassembledInstruction};

use crate::apu::smp::{self, SMP};
use crate::snes::SNES;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct SmpAnalysisState;

impl Default for SmpAnalysisState {
    fn default() -> Self {
        Self
    }
}

impl From<()> for SmpAnalysisState {
    fn from(_: ()) -> Self {
        Self
    }
}

#[derive(Clone, Copy)]
pub struct SmpDisassembledInstruction {
    pub instruction_data: SmpInstructionData,
    pub operand: u32,
}

impl DisassembledInstruction<SmpAnalysisState, SmpInstructionData> for SmpDisassembledInstruction {
    fn mnemonic(&self) -> String {
        self.instruction_data.mnemonic()
    }

    fn from_instruction_data(instruction_data: SmpInstructionData, operand: u32) -> Self {
        Self {
            instruction_data,
            operand,
        }
    }

    fn update_analysis_state(&self, _analysis_state: &mut SmpAnalysisState) {}

    fn mode_str(&self) -> String {
        // TODO: Do per-mode formatting at some point
        format!("{:?}({:08X})", self.instruction_data.mode, self.operand)
    }
}

pub struct DebugSmp {
    smp: Rc<RefCell<SMP>>,
}

impl DebugSmp {
    pub fn new(smp: Rc<RefCell<SMP>>) -> Self {
        Self { smp }
    }
}

// TODO: Need to check if this is the right struct on which to implement this
// unsafe impl Send for DebugCpu {}

impl DebugProcessor for DebugSmp {
    type Address = u16;
    type AnalysisState = SmpAnalysisState;
    type Decoded = SmpInstructionData;
    type Disassembled = SmpDisassembledInstruction;
    type Registers = smp::Registers;

    const ADDR_SPACE_SIZE: usize = 1 << 24;
    const INSTRUCTION_DATA: [Self::Decoded; 256] = SMP_INSTRUCTION_DATA;

    fn entry_points(&self) -> Vec<u16> {
        // TODO: Also analyze other vectors
        vec![self.smp.borrow().peak_u16(smp::RESET_VECTOR)]
    }

    fn peak_u8(&self, addr: u16) -> u8 {
        self.smp.borrow().peak_u8(addr)
    }

    fn analysis_step(
        addr: Self::Address,
        disassembled: &Self::Disassembled,
    ) -> AnalysisStep<Self::Address> {
        // DO NOT SUBMIT
        AnalysisStep::Break
    }

    fn registers(snes: &SNES) -> Self::Registers {
        *snes.smp.borrow().registers()
    }

    fn set_registers(snes: &mut SNES, registers: &Self::Registers) {
        *snes.smp.borrow_mut().registers_mut() = *registers;
    }

    fn pc(registers: &Self::Registers) -> u16 {
        registers.pc
    }
}
