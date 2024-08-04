pub use super::instruction_data::smp_instructions::{
    SmpAddressingMode, SmpInstruction, SmpInstructionData, SMP_INSTRUCTION_DATA,
};

use super::{AnalysisStep, DebugProcessor, DisassembledInstruction};

use crate::apu::smp::{self, SMP};
use crate::scheduler::Device;
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

impl SmpDisassembledInstruction {
    pub fn is_conditional_branch(&self) -> bool {
        use SmpInstruction::*;
        matches!(
            self.instruction_data.instruction,
            BBC | BBS | BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS
        )
    }

    pub fn is_unconditional_branch(&self) -> bool {
        use SmpInstruction::*;
        matches!(self.instruction_data.instruction, BRA | JMP)
    }

    pub fn is_call(&self) -> bool {
        use SmpInstruction::*;
        matches!(self.instruction_data.instruction, CALL | TCALL | PCALL)
    }

    pub fn is_return(&self) -> bool {
        use SmpInstruction::*;
        matches!(self.instruction_data.instruction, RET | RETI)
    }

    pub fn is_indirect(&self) -> bool {
        self.instruction_data.mode.is_indirect()
    }

    pub fn halts(&self) -> bool {
        use SmpInstruction::*;
        matches!(self.instruction_data.instruction, SLEEP | STOP)
    }
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

unsafe impl Send for DebugSmp {}

impl DebugProcessor for DebugSmp {
    type Address = u16;
    type AnalysisState = SmpAnalysisState;
    type Decoded = SmpInstructionData;
    type Disassembled = SmpDisassembledInstruction;
    type Registers = smp::Registers;

    const ADDR_SPACE_SIZE: usize = 1 << 24;
    const INSTRUCTION_DATA: [Self::Decoded; 256] = SMP_INSTRUCTION_DATA;
    const DEVICE: Device = Device::SMP;

    fn entry_points(&self) -> Vec<u16> {
        // TODO: Also analyze other vectors
        vec![self.smp.borrow().peak_u16(smp::RESET_VECTOR)]
    }

    fn peak_u8(&self, addr: u16) -> u8 {
        self.smp.borrow().peak_u8(addr)
    }

    fn analysis_step(addr: u16, disassembled: &Self::Disassembled) -> AnalysisStep<u16> {
        if disassembled.halts() {
            return AnalysisStep::Break;
        }

        let operand = disassembled.operand;

        // TODO: This logic can be simplified now that we have AnalysisStep
        if disassembled.is_conditional_branch() {
            // TODO: More general solution to addressing modes in analyze()
            let offset = match disassembled.instruction_data.mode {
                SmpAddressingMode::PcRelative => operand as i8 as i32 + 2,
                SmpAddressingMode::DirectRelativeBit(_) => (operand >> 8) as i8 as i32 + 2,
                _ => panic!(
                    "Unexpected addressing mode for {:?}: {:?}",
                    disassembled.instruction_data.instruction, disassembled.instruction_data.mode
                ),
            };
            let dest_addr = (addr as i32 + offset) as u16;
            // Take the branch with a copy of the analysis state.
            // Branches don't "return", so afterwards we proceed as if the condition has failed.
            return AnalysisStep::BranchAndContinue(dest_addr);
        } else if disassembled.is_call() && !disassembled.is_indirect() {
            let dest_addr = match disassembled.instruction_data.mode {
                SmpAddressingMode::Absolute => operand as u16,
                SmpAddressingMode::TVector(n) => 0xFFDE - n as u16 * 2,
                SmpAddressingMode::PVector => 0xFF00 | (operand as u16),
                _ => panic!(
                    "Unexpected addressing mode for {:?}: {:?}",
                    disassembled.instruction_data.instruction, disassembled.instruction_data.mode
                ),
            };
            // Send the analysis state as a reference. Calls return, so
            // modifications to registers should be preserved.
            // TODO: More general solution to addressing modes in analyze()
            // TODO: This logic isn't totally sound. For example, if we
            // already analyzed the code, we won't correctly modify registers.
            return AnalysisStep::Call(dest_addr);
        }

        if disassembled.is_unconditional_branch() && !disassembled.is_indirect() {
            // No need to recurse for an unconditional branch, as there's only one execution path.
            // TODO: More general solution to addressing modes in analyze()
            let dest_addr = match disassembled.instruction_data.mode {
                SmpAddressingMode::PcRelative => (addr as i32 + operand as i8 as i32 + 2) as u16,
                SmpAddressingMode::Absolute => operand as u16,
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

    fn registers(snes: &SNES) -> Self::Registers {
        *snes.smp.borrow().registers()
    }

    fn set_registers(snes: &SNES, registers: &Self::Registers) {
        *snes.smp.borrow_mut().registers_mut() = *registers;
    }

    fn pc(registers: &Self::Registers) -> u16 {
        registers.pc
    }

    fn to_analysis_state(_registers: &Self::Registers) -> Self::AnalysisState {
        SmpAnalysisState
    }

    fn breakpoint_at(snes: &SNES, addr: u16) -> bool {
        snes.smp.borrow().breakpoint_addrs.contains(&addr)
    }

    fn toggle_breakpoint_at(snes: &SNES, addr: u16) {
        if !snes.smp.borrow_mut().breakpoint_addrs.remove(&addr) {
            snes.smp.borrow_mut().breakpoint_addrs.insert(addr);
        }
    }
}
