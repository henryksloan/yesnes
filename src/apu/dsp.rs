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
            reg: Registers::new(),
        }
    }

    pub fn reset(&mut self) {
        self.reg = Registers::new();
        self.reg.flags.0 = 0xE0;
    }

    pub fn read_reg(&self, reg_i: u8) -> u8 {
        // The top half of the DSP address space is a read-only mirror of the bottom half
        let read_reg_i = reg_i % 0x80;
        let (hi_nibble, lo_nibble) = (read_reg_i >> 4, read_reg_i & 0xF);
        match (hi_nibble, lo_nibble) {
            (channel_i @ 0x0..=0x7, channel_reg_i @ 0x0..=0x9) => {
                self.reg.channels[channel_i as usize].read_reg(channel_reg_i)
            }
            (global_reg_i, 0xC) => match global_reg_i {
                0x0 => self.reg.main_volume.left.0,
                0x1 => self.reg.main_volume.right.0,
                0x2 => self.reg.echo_volume.left.0,
                0x3 => self.reg.echo_volume.right.0,
                0x6 => self.reg.flags.0,
                0x7 => self.reg.raw_values[read_reg_i as usize], // DO NOT SUBMIT: ENDX
                _ => self.reg.raw_values[read_reg_i as usize],
            },
            (global_reg_i, 0xD) => match global_reg_i {
                0x0 => self.reg.echo_feedback.0,
                0x2 => self.reg.pitch_modulation_enable.0,
                0x3 => self.reg.noise_enable.0,
                0x4 => self.reg.echo_enable.0,
                0x5 => self.reg.brr_directory_hi8,
                0x6 => self.reg.echo_region_hi8,
                // The top nibble of echo_delay is read/writable but unused
                0x7 => self.reg.echo_delay.0,
                _ => self.reg.raw_values[read_reg_i as usize],
            },
            (echo_filter_coeff_i, 0xF) => {
                self.reg.echo_filter_coeff[echo_filter_coeff_i as usize].0
            }
            _ => self.reg.raw_values[read_reg_i as usize],
        }
    }

    pub fn write_reg(&mut self, reg_i: u8, data: u8) {
        // The top half of the DSP address space is a read-only mirror of the bottom half
        if reg_i >= 0x80 {
            return;
        }
        self.reg.raw_values[reg_i as usize] = data;
        let (hi_nibble, lo_nibble) = (reg_i >> 4, reg_i & 0xF);
        match (hi_nibble, lo_nibble) {
            (channel_i @ 0x0..=0x7, channel_reg_i @ 0x0..=0x9) => {
                self.reg.channels[channel_i as usize].write_reg(channel_reg_i, data);
            }
            (global_reg_i, 0xC) => match global_reg_i {
                0x0 => self.reg.main_volume.left.0 = data,
                0x1 => self.reg.main_volume.right.0 = data,
                0x2 => self.reg.echo_volume.left.0 = data,
                0x3 => self.reg.echo_volume.right.0 = data,
                0x6 => self.reg.flags.0 = data,
                0x7 => {} // DO NOT SUBMIT: ACK ENDX
                _ => {}
            },
            (global_reg_i, 0xD) => match global_reg_i {
                0x0 => self.reg.echo_feedback.0 = data,
                0x2 => self.reg.pitch_modulation_enable.0 = data,
                0x3 => self.reg.noise_enable.0 = data,
                0x4 => self.reg.echo_enable.0 = data,
                0x5 => self.reg.brr_directory_hi8 = data,
                0x6 => self.reg.echo_region_hi8 = data,
                // The top nibble of echo_delay is read/writable but unused
                0x7 => self.reg.echo_delay.0 = data,
                _ => {}
            },
            (echo_filter_coeff_i, 0xF) => {
                self.reg.echo_filter_coeff[echo_filter_coeff_i as usize].0 = data;
            }
            _ => {}
        }
    }
}
