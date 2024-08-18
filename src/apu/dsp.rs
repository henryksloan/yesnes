pub mod channel;
pub mod registers;

use channel::{AdsrState, Channel};
use registers::Registers;

/// The S-DSP, the digital signal processor of the APU
pub struct DSP {
    reg: Registers,
    channels: [Channel; 8],
    // A timer that is zero on reset and decrements each DSP tick (32000Hz), wrapping up to 0x77FF
    rate_counter: u16,
}

// rate_counter triggers ADSR/GAIN/noise events when (counter + offset) % period != 0
// Where counter and offset are selected from these tables based on rate registers.
const RATE_COUNTER_MAX: u16 = 0x77FF;
#[rustfmt::skip]
const RATE_COUNTER_PERIODS: [u16; 32] = [
    // V=0 represents an infinite period; this value ensures (counter + offset) % period != 0
    // (since the corresponding offset is 536)
    0xFFFF,
          2048, 1536,
    1280, 1024,  768,
     640,  512,  384,
     320,  256,  192,
     160,  128,   96,
      80,   64,   48,
      40,   32,   24,
      20,   16,   12,
      10,    8,    6,
       5,    4,    3,
       2,    1
];
// Each column of the period table has a delay offset w.r.t. rate_counter. This table is indexed by
// the appropriate rate register modulo 3.
const RATE_COUNTER_OFFSETS: [u16; 3] = [536, 0, 1024];

// ADSR/GAIN/noise operations are clocked by the rate_counter according to the period and
// offset tables. This function returns whether the operation should be applied given the counter value.
fn rate_operation_applies(rate_counter: u16, rate: u8) -> bool {
    (rate_counter + RATE_COUNTER_OFFSETS[rate as usize % 3]) % RATE_COUNTER_PERIODS[rate as usize]
        == 0
}

impl DSP {
    pub fn new() -> Self {
        Self {
            reg: Registers::new(),
            channels: [Channel::default(); 8],
            rate_counter: 0,
        }
    }

    pub fn reset(&mut self) {
        self.reg = Registers::new();
        self.reg.flags.0 = 0xE0;
        for channel in &mut self.channels {
            *channel = Channel::default();
            channel.adsr_state = AdsrState::Release;
        }
        self.rate_counter = 0;
    }

    // TODO: Converge on a common "tick"/"step", "clock"/"tick" terminology
    pub fn tick(&mut self, apu_ram: &[u8; 0x10000]) {
        for channel_i in 0..8 {
            let new_pitch_counter = self.channels[channel_i].pitch_remainder
                + self.reg.channels[channel_i].pitch.pitch();
            self.channels[channel_i].brr_cursor += (new_pitch_counter / 0x1000) as usize;
            self.channels[channel_i].pitch_remainder = new_pitch_counter % 0x1000;
            if self.channels[channel_i].brr_cursor >= 16 {
                // The max pitch is 0x3FFF, so it's impossible to skip an entire sample buffer
                // TODO: Do we always start from the beginning of the next?
                self.channels[channel_i].brr_cursor %= 16;
                let brr_header = self.channels[channel_i].brr_header(apu_ram);
                if brr_header.end_flag() {
                    // TODO: fullsnes says that we "jump to Loop-address" regardless of loop_flag, but that *at least* seems irrelavent
                    self.reg.endx.set_channel(channel_i, true);
                    if brr_header.loop_flag() {
                        // TODO: Should we always start from the beginning of the next?
                        self.channels[channel_i].brr_cursor = 0;
                        // Set the channel's new BRR address to the loop address from the directory table
                        let dir_addr = self.reg.brr_dir_base(channel_i) as usize;
                        self.channels[channel_i].brr_block_addr =
                            ((apu_ram[(dir_addr + 3) % 0x10000] as u16) << 8)
                                | (apu_ram[(dir_addr + 2) % 0x10000] as u16);
                    } else {
                        self.channels[channel_i].adsr_state = AdsrState::Release;
                        // The envelope is zeroes immediately upon BRR end
                        self.channels[channel_i].envelope_level = 0;
                    }
                } else {
                    // Continue to the next 9-byte BRR block
                    self.channels[channel_i].brr_block_addr =
                        self.channels[channel_i].brr_block_addr.wrapping_add(9);
                }
            }
            // Decompress and load new sample is a new one has been reached
            if new_pitch_counter >= 0x1000 {
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
                    // TODO: shift=13..15 is a special/illegal case?
                    ((signed_nibble as i16) << brr_header.shift()) >> 1
                };
                let scale_sample =
                    |val: i16, numer: i32, denom: i32| ((val as i32 * numer) / denom) as i16;
                let prev_two = &mut self.channels[channel_i].prev_two_samples;
                self.channels[channel_i].playing_sample = match brr_header.filter() {
                    0 => sample,
                    1 => sample.saturating_add(scale_sample(prev_two[0], 15, 16)),
                    2 => sample
                        .saturating_add(scale_sample(prev_two[0], 61, 32))
                        .saturating_add(scale_sample(prev_two[1], 15, 16)),
                    3 => sample
                        .saturating_add(scale_sample(prev_two[0], 115, 64))
                        .saturating_add(scale_sample(prev_two[1], 13, 16)),
                    _ => unreachable!(),
                };
                *prev_two = [sample, prev_two[0]];
            }
            // TODO: Gaussian interpolation
            // TODO: Pitch modulation
            // TODO: Echo
            // TODO: Noise
            self.tick_channel(channel_i);
        }
        self.rate_counter = self.rate_counter.wrapping_sub(1).min(RATE_COUNTER_MAX);
    }

    fn tick_channel(&mut self, channel_i: usize) {
        assert!(channel_i < 8);

        let channel = &mut self.channels[channel_i];
        let channel_regs = &mut self.reg.channels[channel_i];
        let envelope_level = &mut channel.envelope_level;
        // The Release state applies regardless of the ADSR and GAIN settings
        if channel.adsr_state == AdsrState::Release {
            // TODO: I think a KOF while playing such an end-block causes a fast release (-0x800 per sample)
            *envelope_level = envelope_level.saturating_sub(8);
        }
        if channel_regs.adsr_control.adsr_enable() {
            let (rate, step) = match channel.adsr_state {
                AdsrState::Attack => {
                    let attack_rate = channel_regs.adsr_control.attack_rate();
                    (
                        attack_rate * 2 + 1,
                        if attack_rate == 0xF { 1024 } else { 32 },
                    )
                }
                AdsrState::Decay => (
                    channel_regs.adsr_control.decay_rate() * 2 + 16,
                    -((((*envelope_level as i16).wrapping_sub(1)) >> 8) + 1),
                ),
                AdsrState::Sustain => (
                    channel_regs.adsr_control.sustain_rate(),
                    -((((*envelope_level as i16).wrapping_sub(1)) >> 8) + 1),
                ),
                // The Release-mode decay is done above, unconditionally
                AdsrState::Release => (0, 0),
            };
            if rate_operation_applies(self.rate_counter, rate) {
                *envelope_level = envelope_level.saturating_add_signed(step);
            }
        } else if channel_regs.gain_control.custom_gain() {
            let apply = rate_operation_applies(self.rate_counter, channel_regs.gain_control.rate());
            if apply {
                let step = match channel_regs.gain_control.mode() {
                    // Linear decrease
                    0 => -32,
                    // Exponential decrease
                    1 => -((((*envelope_level as i16).wrapping_sub(1)) >> 8) + 1),
                    // Linear increase
                    2 => 32,
                    // Bent increase
                    3 => {
                        if *envelope_level < 0x600 {
                            32
                        } else {
                            8
                        }
                    }
                    _ => unreachable!(),
                };
                *envelope_level = envelope_level.saturating_add_signed(step);
            }
        } else {
            *envelope_level = channel_regs.gain_control.fixed_volume() as u16 * 16;
        };

        *envelope_level = (*envelope_level).min(0x7FF);
        if self.reg.flags.soft_reset() {
            *envelope_level = 0;
            channel.adsr_state = AdsrState::Release;
        }
        channel_regs.envx = (*envelope_level >> 4) as u8;

        // If VxGAIN is in use, the ADSR state still changes, but the sustain_level boundary is read
        // from VxGAIN instead of VxADSR.
        let sustain_level = if channel_regs.adsr_control.adsr_enable() {
            channel_regs.adsr_control.sustain_level()
        } else {
            channel_regs.gain_control.garbage_boundary()
        };
        let sustain_boundary = (sustain_level + 1) * 0x100;
        channel.adsr_state = match channel.adsr_state {
            AdsrState::Attack if *envelope_level >= 0x7E0 => AdsrState::Decay,
            AdsrState::Decay if *envelope_level <= sustain_boundary => AdsrState::Sustain,
            _ => channel.adsr_state,
        };

        let mut result = channel.playing_sample;
        result = ((result as i32 * *envelope_level as i32) / 0x800) as i16;

        self.channels[channel_i].output = result;
        channel_regs.outx = (result >> 8) as u8;
    }

    pub fn get_output(&mut self) -> (i16, i16) {
        let (mut sum_left, mut sum_right) = (0i16, 0i16);
        for channel_i in 0..8 {
            let channel_out = self.channels[channel_i].output;
            sum_left = sum_left.saturating_add(
                ((channel_out as i32 * self.reg.channels[channel_i].volume.left as i32) >> 7)
                    as i16,
            );
            sum_right = sum_right.saturating_add(
                ((channel_out as i32 * self.reg.channels[channel_i].volume.right as i32) >> 7)
                    as i16,
            );
        }
        sum_left = ((sum_left as i32 * self.reg.main_volume.left as i32) >> 7) as i16;
        sum_right = ((sum_right as i32 * self.reg.main_volume.right as i32) >> 7) as i16;
        if self.reg.flags.mute_all() {
            return (0, 0);
        }
        // These XORs perform a phase inversion
        (
            (sum_left as u16 ^ 0xFFFF) as i16,
            (sum_right as u16 ^ 0xFFFF) as i16,
        )
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
                0x0 => self.reg.main_volume.left as u8,
                0x1 => self.reg.main_volume.right as u8,
                0x2 => self.reg.echo_volume.left as u8,
                0x3 => self.reg.echo_volume.right as u8,
                0x6 => self.reg.flags.0,
                0x7 => self.reg.endx.0,
                _ => self.reg.raw_values[read_reg_i as usize],
            },
            (global_reg_i, 0xD) => match global_reg_i {
                0x0 => self.reg.echo_feedback as u8,
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
                self.reg.echo_filter_coeff[echo_filter_coeff_i as usize] as u8
            }
            _ => self.reg.raw_values[read_reg_i as usize],
        }
    }

    pub fn write_reg(&mut self, reg_i: u8, data: u8, apu_ram: &[u8; 0x10000]) {
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
                0x0 => self.reg.main_volume.left = data as i8,
                0x1 => self.reg.main_volume.right = data as i8,
                0x2 => self.reg.echo_volume.left = data as i8,
                0x3 => self.reg.echo_volume.right = data as i8,
                0x4 => {
                    // KON: Start playing a note
                    // TODO: KON and KOF are clocked at 16000Hz... see fullsnes
                    // TODO: 5 empty samples upon KON
                    for channel_i in 0..8 {
                        if (data >> channel_i) & 1 == 1 {
                            let dir_addr = self.reg.brr_dir_base(channel_i) as usize;
                            self.channels[channel_i].brr_block_addr =
                                ((apu_ram[(dir_addr + 1) % 0x10000] as u16) << 8)
                                    | (apu_ram[dir_addr % 0x10000] as u16);
                            self.channels[channel_i].brr_cursor = 0;
                            // TODO: Does KON reset the pitch counter?
                            // self.channels[channel_i].pitch_remainder = 0;
                            self.channels[channel_i].adsr_state = AdsrState::Attack;
                            self.channels[channel_i].envelope_level = 0;
                            // TODO: Does anything flush prev_two_samples?
                        }
                    }
                }
                0x5 => {
                    // KOF: Put channel in released state
                    for channel_i in 0..8 {
                        if (data >> channel_i) & 1 == 1 {
                            self.channels[channel_i].adsr_state = AdsrState::Release;
                        }
                    }
                }
                0x6 => self.reg.flags.0 = data,
                0x7 => self.reg.endx.0 = 0,
                _ => {}
            },
            (global_reg_i, 0xD) => match global_reg_i {
                0x0 => self.reg.echo_feedback = data as i8,
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
                self.reg.echo_filter_coeff[echo_filter_coeff_i as usize] = data as i8;
            }
            _ => {}
        }
    }
}
