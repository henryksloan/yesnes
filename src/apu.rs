//! The SNES APU, composed of the SPC700 and DSP audio coprocessors

pub mod dsp;
pub mod smp;

pub use dsp::DSP;
pub use smp::SMP;
