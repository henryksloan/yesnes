use super::signed_magnitude_8::SignedMagnitude8;

use bitfield::bitfield;

#[derive(Default, Clone, Copy)]
pub struct Registers {}

pub struct ChannelRegisters {
    // X0h, X1h (VOL): Left and right channel volume, signed
    volume_left: SignedMagnitude8,
    volume_right: SignedMagnitude8,
    // X2h, X3h (P): Sample pitch
    pitch: SamplePitch,
    // X4h (SRCN; sometimes SCRN, which might be a typo): Sample source entry
    source_number: u8,
    // X5h, X6h (ADSR): Attack-Decay-Sustain-Release envelope control
    adsr_control: AdsrControl,
    // X7h (GAIN): Gain control
    gain_control: GainControl,
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
