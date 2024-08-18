use bitfield::bitfield;

#[derive(Clone, Copy, Default)]
pub struct Channel {
    // The base (header) address of the 9-byte Bit Rate Reduction (BRR) block to be played
    pub brr_block_addr: u16,
    // The current nibble being played, 0..=15, where the high nibble of each byte comes first
    pub brr_cursor: usize,
    // DO NOT SUBMIT: Verify this
    // Each channel effectively has a counter that increases by P (pitch register) each tick (32000Hz),
    // incrementing the cursor each time it crosses a multiple of 0x1000. Therefore, "between ticks",
    // the counter is at some offset equal to the remainder of the previous division by 0x1000.
    pub pitch_remainder: u16,
    pub adsr_state: AdsrState,
    pub playing_sample: i16,
    // The current 11-bit envelope level
    pub envelope_level: u16,
    // The output of the channel, updated at 32000Hz. The high byte is exposed via VxOUTX.
    pub output: i16,
    // For BRR filtering; [sample i-1, sample i-2]
    pub prev_two_samples: [i16; 2],
}

// The state of the Attack-Decay-Sustain-Release envelope. The state changes depending on the
// envelope level, regardless of the ADSR and GAIN registers. If GAIN is in use, the
// Attack/Decay/Sustain modes are ignored, while the Release state (triggered by KOF)
// always reduces the envelope level at 8 units per tick.
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub enum AdsrState {
    Attack,
    Decay,
    Sustain,
    #[default]
    Release,
}

impl Channel {
    pub fn brr_header(&self, apu_ram: &Box<[u8; 0x10000]>) -> BrrHeader {
        BrrHeader(apu_ram[self.brr_block_addr as usize])
    }
}

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct BrrHeader(u8);
  impl Debug;
  pub end_flag, _: 0;
  pub loop_flag, _: 1;
  pub filter, _: 3, 2;
  pub shift, _: 7, 4;
}
