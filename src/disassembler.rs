// Some of the design ideas for this disassembler come from bsnes-plus and Higan-S
pub mod debug_cpu;
pub mod debug_smp;
pub mod instruction_data;

pub use debug_cpu::{CpuAnalysisState, CpuDisassembledInstruction, DebugCpu};

pub enum AnalysisStep<Address> {
    Break,
    BranchAndContinue(Address),
    Call(Address),
    Branch(Address),
    Continue,
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
    type Address: Into<usize> + TryFrom<usize> + Copy + Default;
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
                operand |= self.debug_processor.peak_u8(
                    D::Address::try_from(addr.into() + operand_byte_offset).unwrap_or_default(),
                ) as u32;
            }
            let disassembled = D::Disassembled::from_instruction_data(decoded, operand);
            disassembled.update_analysis_state(analysis_state);
            self.disassembly_cache[addr.into()] = Some(disassembled);

            match D::analysis_step(addr, &disassembled) {
                AnalysisStep::Break => break,
                AnalysisStep::BranchAndContinue(dest_addr) => {
                    self.analyze(dest_addr, &mut analysis_state.clone());
                    // TODO: Factor out this address change
                    addr = D::Address::try_from(addr.into() + (usize::from(operand_bytes) + 1))
                        .unwrap_or_default();
                }
                AnalysisStep::Call(dest_addr) => {
                    self.analyze(dest_addr, analysis_state);
                    addr = D::Address::try_from(addr.into() + (usize::from(operand_bytes) + 1))
                        .unwrap_or_default();
                }
                AnalysisStep::Branch(dest_addr) => addr = dest_addr,
                AnalysisStep::Continue => {
                    addr = D::Address::try_from(addr.into() + (usize::from(operand_bytes) + 1))
                        .unwrap_or_default();
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
