use super::signed_magnitude_8::{LeftRight, SignedMagnitude8};

use bitfield::bitfield;

#[derive(Default, Clone, Copy)]
pub struct Registers {
    // Registers X0h-X9h for the eight channels numbered 0-7
    pub channels: [ChannelRegisters; 8],
    // 0Ch, 1Ch (MVOL): Left and right main channel volume, signed
    pub main_volume: LeftRight,
    // 2Ch, 3Ch (EVOL): Left and right echo channel volume, signed
    pub echo_volume: LeftRight,
    // DO NOT SUBMIT: add/document 4Ch (KON) and 5Ch (KOF)
    // 6Ch (FLG): Main control flags
    pub flags: ControlFlags,
    // DO NOT SUBMIT: add/document 7Ch (ENDX)
    // 0Dh (EFB): Echo feedback
    pub echo_feedback: SignedMagnitude8,
    // 2Dh (PMON): Pitch modulation enablement
    pub pitch_modulation_enable: PitchModulationEnable,
    // 3Dh (NON): Controls which channels are noise channels
    pub noise_enable: NoiseEnable,
    // 4Dh (EON): Controls which channels have echo
    pub echo_enable: EchoEnable,
    // 5Dh (DIR): High byte of BRR sample bank address, indexed by SRCN for each channel
    pub brr_directory_hi8: u8,
    // 6Dh (ESA): High byte of the echo memory region
    pub echo_region_hi8: u8,
    // 7Dh (EDL): Echo delay time
    pub echo_delay: EchoDelay,
    // 0Fh-7Fh (C0-C7): Echo filter coefficients
    pub echo_coeff: [SignedMagnitude8; 8],
}

pub struct ChannelRegisters {
    // X0h, X1h (VOL): Left and right channel volume, signed
    pub volume: LeftRight,
    // X2h, X3h (P): Sample pitch
    pub pitch: SamplePitch,
    // X4h (SRCN; sometimes SCRN, which might be a typo): Sample source entry
    pub source_number: u8,
    // X5h, X6h (ADSR): Attack-Decay-Sustain-Release envelope control
    pub adsr_control: AdsrControl,
    // X7h (GAIN): Gain control
    pub gain_control: GainControl,
    // X8h (ENVX) and X9h (OUTX) are read-only and dynamic
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
pub struct PitchModulationEnable(pub u8);

impl PitchModulationEnable {
    pub fn channel_enabled(&self, channel_i: usize) -> bool {
        // Pitch modulation is controlled by the next lowest channel, so channel 0 can't use it
        assert!(channel_i > 1 && channel_i < 8);
        (self.0 >> channel_i) & 1 == 1
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct NoiseEnable(pub u8);

impl NoiseEnable {
    pub fn channel_enabled(&self, channel_i: usize) -> bool {
        assert!(channel_i < 8);
        (self.0 >> channel_i) & 1 == 1
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct EchoEnable(pub u8);

impl EchoEnable {
    pub fn channel_enabled(&self, channel_i: usize) -> bool {
        assert!(channel_i < 8);
        (self.0 >> channel_i) & 1 == 1
    }
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct EchoDelay(u8);
  impl Debug;
  pub echo_delay, _: 3, 0;
}
