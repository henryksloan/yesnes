use bitfield::bitfield;

bitfield! {
  // An entry in the low OAM table, i.e. the first 512 bytes of OAM.
  #[derive(Clone, Copy, Default)]
  pub struct OamLoEntry(u32);
  impl Debug;
  u8;
  pub x_lo8, _: 7, 0;
  pub y, _: 15, 8;
  pub chr_n, _: 23, 16;
  pub u8, into ObjAttributes, attr, _: 31, 24;
}

bitfield! {
  // The attributes of an object in the OAM.
  #[derive(Clone, Copy, Default)]
  pub struct ObjAttributes(u8);
  impl Debug;
  pub nametable_select, _: 0;
  pub palette_n, _: 3, 1;
  pub priority, _: 5, 4;
  pub flip_x, _: 6;
  pub flip_y, _: 7;
}

impl Into<ObjAttributes> for u8 {
    fn into(self) -> ObjAttributes {
        ObjAttributes(self)
    }
}
