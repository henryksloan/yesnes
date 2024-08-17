use super::signed_magnitude_8::{LeftRight, SignedMagnitude8};

use arrayvec::ArrayVec;
use bitfield::bitfield;

#[derive(Default, Clone)]
pub struct Registers {
    // Register offsets with no other purpose still function as read/writable RAM
    pub raw_values: ArrayVec<u8, 0x80>,
    // Registers X0h-X9h for the eight channels numbered 0-7
    pub channels: [ChannelRegisters; 8],
    // 0Ch, 1Ch (MVOL): Left and right main channel volume, signed
    pub main_volume: LeftRight,
    // 2Ch, 3Ch (EVOL): Left and right echo channel volume, signed
    pub echo_volume: LeftRight,
    // DO NOT SUBMIT: add/document 4Ch (KON) and 5Ch (KOF)
    // 6Ch (FLG): Main control flags
    pub flags: ControlFlags,
    // 7Ch (ENDX): Resettable bits set when a channel reaches the end of a BRR sample
    pub endx: PerChannel,
    // 0Dh (EFB): Echo feedback
    pub echo_feedback: SignedMagnitude8,
    // 2Dh (PMON): Pitch modulation enablement
    pub pitch_modulation_enable: PitchModulationEnable,
    // 3Dh (NON): Controls which channels are noise channels
    pub noise_enable: PerChannel,
    // 4Dh (EON): Controls which channels have echo
    pub echo_enable: PerChannel,
    // 5Dh (DIR): High byte of BRR sample directory address, indexed by SRCN for each channel
    pub brr_directory_hi8: u8,
    // 6Dh (ESA): High byte of the echo memory region
    pub echo_region_hi8: u8,
    // 7Dh (EDL): Echo delay time
    pub echo_delay: EchoDelay,
    // 0Fh-7Fh (C0-C7): Echo filter coefficients
    pub echo_filter_coeff: [SignedMagnitude8; 8],
}

impl Registers {
    pub fn new() -> Self {
        Self {
            raw_values: [0; 0x80].into(),
            ..Default::default()
        }
    }

    pub fn brr_dir_base(&self, channel_i: usize) -> u16 {
        (self.brr_directory_hi8 as u16 * 0x100)
            .wrapping_add(self.channels[channel_i].source_number as u16 * 4)
    }
}

#[derive(Default, Clone, Copy)]
pub struct ChannelRegisters {
    // X0h, X1h (VOL): Left and right channel volume, signed
    pub volume: LeftRight,
    // X2h, X3h (P): Sample pitch
    pub pitch: SamplePitch,
    // X4h (SRCN; sometimes SCRN, which might be a typo): Sample source entry in BRR directory (DIR)
    pub source_number: u8,
    // X5h, X6h (ADSR): Attack-Decay-Sustain-Release envelope control
    pub adsr_control: AdsrControl,
    // X7h (GAIN): Gain control
    pub gain_control: GainControl,
    // X8h (ENVX) The high 7 bits of the channel's 11-bit envelope
    pub envx: u8,
    // X9h (OUTX): The high byte of the channel's output
    pub outx: u8,
}

impl ChannelRegisters {
    // DO NOT SUBMIT: This seems to imply that this struct encapsulates an actual channel; rename and rework as such
    pub fn read_reg(&self, reg_i: u8) -> u8 {
        match reg_i {
            0x0 => self.volume.left.0,
            0x1 => self.volume.right.0,
            0x2 => self.pitch.lo_byte(),
            0x3 => self.pitch.hi_byte(),
            0x4 => self.source_number,
            0x5 => self.adsr_control.lo_byte(),
            0x6 => self.adsr_control.hi_byte(),
            0x7 => self.gain_control.0,
            0x8 => self.envx,
            0x9 => self.outx,
            _ => panic!("Invalid channel register index {reg_i}"),
        }
    }

    pub fn write_reg(&mut self, reg_i: u8, data: u8) {
        match reg_i {
            0x0 => self.volume.left.0 = data,
            0x1 => self.volume.right.0 = data,
            0x2 => self.pitch.set_lo_byte(data),
            0x3 => self.pitch.set_hi_byte(data),
            0x4 => self.source_number = data,
            0x5 => self.adsr_control.set_lo_byte(data),
            0x6 => self.adsr_control.set_hi_byte(data),
            0x7 => self.gain_control.0 = data,
            // These two registers are technically writable, but they are overwritten every tick
            // TODO: Should these writes reflect in the actual output? Maybe not?
            0x8 => self.envx = data,
            0x9 => self.outx = data,
            _ => panic!("Invalid channel register index {reg_i}"),
        }
    }
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct SamplePitch(u16);
  impl Debug;
  pub pitch, _: 13, 0;

  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct AdsrControl(u16);
  impl Debug;
  pub attack_rate, _: 3, 0;
  pub decay_rate, _: 6, 4;
  pub adsr_enable, _: 7;
  pub sustain_rate, _: 12, 8;
  pub sustain_level, _: 15, 13;

  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct GainControl(u8);
  impl Debug;
  // Used when bit7=0:
  pub fixed_volume, _: 6, 0;
  // Used when bit7=1:
  pub rate, _: 4, 0;
  pub mode, _: 6, 5;
  // Selects between 0=direct and 1=custom gain modes
  pub custom_gain, _: 7;
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct ControlFlags(u8);
  impl Debug;
  pub noise_frequency, _: 4, 0;
  pub echo_disable, _: 5;
  pub mute_all, _: 6;
  pub soft_reset, _: 7;
}

#[derive(Clone, Copy, Default, Debug)]
pub struct PerChannel(pub u8);

impl PerChannel {
    pub fn channel(&self, channel_i: usize) -> bool {
        assert!(channel_i < 8);
        (self.0 >> channel_i) & 1 == 1
    }

    pub fn set_channel(&mut self, channel_i: usize, val: bool) {
        assert!(channel_i < 8);
        let mask = (val as u8) << channel_i;
        self.0 = (self.0 & !mask) | mask;
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct PitchModulationEnable(pub u8);

impl PitchModulationEnable {
    pub fn channel_enabled(&self, channel_i: usize) -> bool {
        // Pitch modulation is controlled by the next lowest channel, so channel 0 can't use it
        assert!(channel_i > 1 && channel_i < 8);
        (self.0 >> channel_i) & 1 == 1
    }
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct EchoDelay(u8);
  impl Debug;
  pub echo_delay, _: 3, 0;
}
