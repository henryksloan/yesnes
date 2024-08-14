pub mod registers;
pub mod signed_magnitude_8;

pub use registers::Registers;
pub use signed_magnitude_8::SignedMagnitude8;

/// The S-DSP, the digital signal processor of the APU
pub struct DSP {
    reg: Registers,
}

impl DSP {
    pub fn new() -> Self {
        Self {
            reg: Registers::default(),
        }
    }

    pub fn reset(&mut self) {
        self.reg = Registers::default();
        self.reg.flags.0 = 0xE0;
    }
}
