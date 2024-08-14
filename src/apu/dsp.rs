pub mod registers;
pub mod signed_magnitude_8;

pub use registers::Registers;
pub use signed_magnitude_8::SignedMagnitude8;

/// The S-DSP, the digital signal processor of the APU
pub struct DSP {}

impl DSP {
    pub fn new() -> Self {
        Self {}
    }
}
