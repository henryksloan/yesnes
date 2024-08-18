pub mod channel;
pub mod registers;
pub mod signed_magnitude_8;

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
                        self.channels[channel_i].adsr_state = AdsrState::Release;
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
                    // DO NOT SUBMIT: shift=13..15 might be a special case?
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
                    3 | _ => sample
                        .saturating_add(scale_sample(prev_two[0], 115, 64))
                        .saturating_add(scale_sample(prev_two[1], 13, 16)),
                };
                *prev_two = [sample, prev_two[0]];
            }
            // TODO: Gaussian interpolation
            // DO NOT SUBMIT: Might have to separate this for e.g. pitch modulation
            self.tick_channel(channel_i);
        }
        self.rate_counter = self.rate_counter.wrapping_sub(1).min(RATE_COUNTER_MAX);
    }

    fn tick_channel(&mut self, channel_i: usize) {
        assert!(channel_i < 8);

        // DO NOT SUBMIT: Bind self.reg.channels[channel_i] etc. to reference variables
        let envelope_level = &mut self.channels[channel_i].envelope_level;
        // The Release state applies regardless of the ADSR and GAIN settings
        if self.channels[channel_i].adsr_state == AdsrState::Release {
            // DO NOT SUBMIT: This should be "Step=-800h when BRR-end". Same for soft-reset.
            *envelope_level = envelope_level.saturating_sub(8);
        }
        if self.reg.channels[channel_i].adsr_control.adsr_enable() {
            let (rate, step) = match self.channels[channel_i].adsr_state {
                AdsrState::Attack => {
                    let attack_rate = self.reg.channels[channel_i].adsr_control.attack_rate();
                    (
                        attack_rate * 2 + 1,
                        if attack_rate == 0xF { 1024 } else { 32 },
                    )
                }
                AdsrState::Decay => (
                    self.reg.channels[channel_i].adsr_control.decay_rate() * 2 + 16,
                    -((((*envelope_level as i16).wrapping_sub(1)) >> 8) + 1),
                ),
                AdsrState::Sustain => (
                    self.reg.channels[channel_i].adsr_control.sustain_rate(),
                    -((((*envelope_level as i16).wrapping_sub(1)) >> 8) + 1),
                ),
                // The Release-mode diminution is done above, unconditionally
                AdsrState::Release => (0, 0),
            };
            if rate_operation_applies(self.rate_counter, rate) {
                *envelope_level = envelope_level.saturating_add_signed(step);
            }
        } else {
            if self.reg.channels[channel_i].gain_control.custom_gain() {
                let apply = rate_operation_applies(
                    self.rate_counter,
                    self.reg.channels[channel_i].gain_control.rate(),
                );
                if apply {
                    let step = match self.reg.channels[channel_i].gain_control.mode() {
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
                        // DO NOT SUBMIT: This is useful, use it throughout
                        _ => unreachable!(),
                    };
                    *envelope_level = envelope_level.saturating_add_signed(step);
                }
            } else {
                *envelope_level =
                    self.reg.channels[channel_i].gain_control.fixed_volume() as u16 * 16;
            };
        };
        *envelope_level = (*envelope_level).min(0x7FF);
        self.reg.channels[channel_i].envx = (*envelope_level >> 4) as u8;

        // If VxGAIN is in use, the ADSR state still changes, but the sustain_level boundary is read
        // from VxGAIN instead of VxADSR.
        let sustain_level = if self.reg.channels[channel_i].adsr_control.adsr_enable() {
            self.reg.channels[channel_i].adsr_control.sustain_level()
        } else {
            self.reg.channels[channel_i].gain_control.garbage_boundary()
        };
        let sustain_boundary = (sustain_level + 1) * 0x100;
        self.channels[channel_i].adsr_state = match self.channels[channel_i].adsr_state {
            AdsrState::Attack if *envelope_level >= 0x7E0 => AdsrState::Decay,
            AdsrState::Decay if *envelope_level <= sustain_boundary => AdsrState::Sustain,
            _ => self.channels[channel_i].adsr_state,
        };

        let mut result = self.channels[channel_i].playing_sample;
        result = ((result as i32 * *envelope_level as i32) / 0x800) as i16;

        self.channels[channel_i].output = result;
        self.reg.channels[channel_i].outx = (result >> 8) as u8;
    }

    pub fn get_output(&mut self) -> (i16, i16) {
        let (mut sum_left, mut sum_right) = (0i16, 0i16);
        for channel_i in 0..8 {
            let channel_out = self.channels[channel_i].output;
            // DO NOT SUBMIT: Make this arithmetic/signed magnitude better
            // DO NOT SUBMIT: fullsnes says "with 16bit overflow handling (after each addition)"... what?
            // DO NOT SUBMIT: Is it signed magnitude or two's complement?
            sum_left = sum_left.saturating_add(
                ((channel_out as i32
                    // * Into::<i8>::into(self.reg.channels[channel_i].volume.left) as i32)
                    * self.reg.channels[channel_i].volume.left.0 as i8 as i32)
                    >> 7) as i16,
            );
            sum_right = sum_right.saturating_add(
                ((channel_out as i32
                    // * Into::<i8>::into(self.reg.channels[channel_i].volume.right) as i32)
                    * self.reg.channels[channel_i].volume.right.0 as i8 as i32)
                    >> 7) as i16,
            );
        }
        sum_left =
            // ((sum_left as i32 * Into::<i8>::into(self.reg.main_volume.left) as i32) >> 7) as i16;
            ((sum_left as i32 * self.reg.main_volume.left.0 as i8 as i32) >> 7) as i16;
        sum_right =
            // ((sum_right as i32 * Into::<i8>::into(self.reg.main_volume.right) as i32) >> 7) as i16;
            ((sum_right as i32 * self.reg.main_volume.right.0 as i8 as i32) >> 7) as i16;
        // DO NOT SUBMIT: Mute and "final phase inversion"
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
                            // DO NOT SUBMIT: What about pitch_remainder?
                            // self.channels[channel_i].pitch_remainder = 0;
                            self.channels[channel_i].adsr_state = AdsrState::Attack;
                            self.channels[channel_i].envelope_level = 0;
                            // DO NOT SUBMIT: Does anything flush prev_two_samples?
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
