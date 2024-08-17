use bitfield::bitfield;

bitfield! {
  #[derive(Clone, Copy, Default)]
  pub struct SignedMagnitude8(u8);
  impl Debug;
  pub magnitude, _: 6, 0;
  pub sign, _: 7;
}

impl Into<i8> for SignedMagnitude8 {
    fn into(self) -> i8 {
        ((-2 * self.sign() as i8) + 1) * self.magnitude() as i8
    }
}

// A pair of signed magnitude integers for left and right channels,
// a common pattern in DSP registers (e.g. volume).
#[derive(Clone, Copy, Default)]
pub struct LeftRight {
    pub left: SignedMagnitude8,
    pub right: SignedMagnitude8,
}
