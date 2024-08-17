pub mod channel;
pub mod registers;
pub mod signed_magnitude_8;

use channel::Channel;
use registers::Registers;

/// The S-DSP, the digital signal processor of the APU
pub struct DSP {
    reg: Registers,
    channels: [Channel; 8],
}

impl DSP {
    pub fn new() -> Self {
        Self {
            reg: Registers::new(),
            channels: [Channel::default(); 8],
        }
    }

    pub fn reset(&mut self) {
        self.reg = Registers::new();
        self.reg.flags.0 = 0xE0;
        for channel in &mut self.channels {
            *channel = Channel::default();
            channel.released = true;
        }
    }

    // DO NOT SUBMIT: Converge on a common "tick"/"step", "clock"/"tick" terminology
    pub fn tick(&mut self, apu_ram: &Box<[u8; 0x10000]>) {
        for channel_i in 0..8 {
            let new_pitch_counter = self.channels[channel_i].pitch_remainder
                + self.reg.channels[channel_i].pitch.pitch();
            self.channels[channel_i].brr_cursor += (new_pitch_counter / 0x1000) as usize;
            self.channels[channel_i].pitch_remainder = new_pitch_counter % 0x1000;
            if self.channels[channel_i].brr_cursor >= 16 {
                // The max pitch is 0x3FFF, so it's impossible to skip an entire sample buffer
                // DO NOT SUBMIT: Do we always start from the beginning of the next?
                self.channels[channel_i].brr_cursor %= 16;
                let brr_header = self.channels[channel_i].brr_header(apu_ram);
                if brr_header.end_flag() {
                    // DO NOT SUBMIT: fullsnes says that we "jump to Loop-address" regardless of loop_flag, but that *at least* seems irrelavent
                    self.reg.endx.set_channel(channel_i, true);
                    if brr_header.loop_flag() {
                        // DO NOT SUBMIT: Does this start from the beginning of the next?
                        self.channels[channel_i].brr_cursor = 0;
                        // Set the channel's new BRR address to the loop address from the directory table
                        let dir_addr = self.reg.brr_dir_base(channel_i) as usize;
                        self.channels[channel_i].brr_block_addr =
                            ((apu_ram[(dir_addr + 3) % 0x10000] as u16) << 8)
                                | (apu_ram[(dir_addr + 2) % 0x10000] as u16);
                    } else {
                        self.channels[channel_i].released = true;
                    }
                } else {
                    // Continue to the next 9-byte BRR block
                    self.channels[channel_i].brr_block_addr =
                        self.channels[channel_i].brr_block_addr.wrapping_add(9);
                }
            }
        }
    }

    pub fn get_output(&mut self, apu_ram: &Box<[u8; 0x10000]>) -> (i16, i16) {
        let (mut sum_left, mut sum_right) = (0i16, 0i16);
        for channel_i in 0..8 {
            let channel_out = self.get_channel_output(channel_i, apu_ram);
            // DO NOT SUBMIT: Make this arithmetic/signed magnitude better
            // DO NOT SUBMIT: fullsnes says "with 16bit overflow handling (after each addition)"... what?
            sum_left = sum_left.wrapping_add(
                ((channel_out as i32
                    * Into::<i8>::into(self.reg.channels[channel_i].volume.left) as i32)
                    >> 7) as i16,
            );
            sum_right = sum_right.wrapping_add(
                ((channel_out as i32
                    * Into::<i8>::into(self.reg.channels[channel_i].volume.right) as i32)
                    >> 7) as i16,
            );
        }
        sum_left =
            ((sum_left as i32 * Into::<i8>::into(self.reg.main_volume.left) as i32) >> 7) as i16;
        sum_right =
            ((sum_right as i32 * Into::<i8>::into(self.reg.main_volume.right) as i32) >> 7) as i16;
        // DO NOT SUBMIT: Mute and "final phase inversion"
        (sum_left, sum_right)
    }

    fn get_channel_output(&mut self, channel_i: usize, apu_ram: &Box<[u8; 0x10000]>) -> i16 {
        assert!(channel_i < 8);
        if self.channels[channel_i].released {
            return 0;
        }
        let brr_header = self.channels[channel_i].brr_header(apu_ram);
        // The block address points to the header, so add 1 for the first actual sample address
        let brr_base = self.channels[channel_i].brr_block_addr as usize + 1;
        let (brr_byte, brr_nibble) = (
            self.channels[channel_i].brr_cursor / 2,
            1 - (self.channels[channel_i].brr_cursor % 2),
        );
        let sample = {
            let nibble = (apu_ram[brr_base + brr_byte] >> (4 * brr_nibble)) & 0xF;
            let signed_nibble = ((nibble << 4) as i8) >> 4;
            // DO NOT SUBMIT: shift=13..15 might be a special case?
            ((signed_nibble as i16) << brr_header.shift()) >> 1
        };
        let scale_sample = |val: i16, numer: i32, denom: i32| ((val as i32 * numer) / denom) as i16;
        let prev_two = &mut self.channels[channel_i].prev_two_samples;
        let result = match brr_header.filter() {
            0 => sample,
            1 => sample.saturating_add(scale_sample(prev_two[0], 15, 16)),
            2 => sample
                .saturating_add(scale_sample(prev_two[0], 61, 32))
                .saturating_add(scale_sample(prev_two[1], 15, 16)),
            3 | _ => sample
                .saturating_add(scale_sample(prev_two[0], 115, 64))
                .saturating_add(scale_sample(prev_two[1], 13, 16)),
        };
        *prev_two = [sample, prev_two[0]];
        result
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
                0x7 => self.reg.endx.0,
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

    // DO NOT SUBMIT: Maybe just make apu_ram an Rc<RefCell>... :(
    pub fn write_reg(&mut self, reg_i: u8, data: u8, apu_ram: &Box<[u8; 0x10000]>) {
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
                0x4 => {
                    // KON: Start playing a note
                    // DO NOT SUBMIT: KON and KOF are clocked at 16000Hz... see fullsnes
                    for channel_i in 0..8 {
                        if (data >> channel_i) & 1 == 1 {
                            let dir_addr = self.reg.brr_dir_base(channel_i) as usize;
                            self.channels[channel_i].brr_block_addr =
                                ((apu_ram[(dir_addr + 1) % 0x10000] as u16) << 8)
                                    | (apu_ram[dir_addr % 0x10000] as u16);
                            self.channels[channel_i].brr_cursor = 0;
                            // self.channels[channel_i].pitch_remainder = 0;
                            self.channels[channel_i].released = false;
                            // DO NOT SUBMIT: Does anything flush prev_two_samples?
                        }
                    }
                }
                0x5 => {
                    // KOF: Put channel in released state
                    for channel_i in 0..8 {
                        if (data >> channel_i) & 1 == 1 {
                            // DO NOT SUBMIT: "released" is actually an ADSR state, so this should decay to zero at a constant rate (not instant)
                            self.channels[channel_i].released = true;
                        }
                    }
                }
                // DO NOT SUBMIT: If soft reset, key off all, zero envelope
                0x6 => self.reg.flags.0 = data,
                0x7 => self.reg.endx.0 = 0,
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
