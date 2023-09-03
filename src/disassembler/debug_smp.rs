pub use super::instruction_data::smp_instructions::{
    SmpAddressingMode, SmpInstruction, SmpInstructionData, SMP_INSTRUCTION_DATA,
};

use super::{AnalysisStep, DebugProcessor, DisassembledInstruction};

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
}

pub struct DebugSmp;

impl DebugSmp {
    pub fn new() -> Self {
        Self
    }
}

// TODO: Need to check if this is the right struct on which to implement this
// unsafe impl Send for DebugCpu {}

impl DebugProcessor for DebugSmp {
    type Address = u16;
    type AnalysisState = SmpAnalysisState;
    type Decoded = SmpInstructionData;
    type Disassembled = SmpDisassembledInstruction;

    const ADDR_SPACE_SIZE: usize = 1 << 24;
    const INSTRUCTION_DATA: [Self::Decoded; 256] = SMP_INSTRUCTION_DATA;

    fn entry_points(&self) -> Vec<u16> {
        // TODO: Also analyze other vectors. The 0xFFF* vectors are emu mode, the 0xFFE* vectors are not.
        // vec![u24(
        //     Bus::peak_u16(self.bus.clone(), smp::RESET_VECTOR) as u32
        // )]
        todo!()
    }

    fn peak_u8(&self, addr: u16) -> u8 {
        todo!()
    }

    fn analysis_step(
        addr: Self::Address,
        disassembled: &Self::Disassembled,
    ) -> AnalysisStep<Self::Address> {
        todo!()
    }
}
