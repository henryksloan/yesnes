use bitfield::bitfield;

use crate::u24::u24;

#[derive(Default, Clone, Copy)]
pub struct Registers {
    pub a: u16, // Accumulator register
    pub x: u16, // Index registers
    pub y: u16,
    // Program counter
    // The most significant byte is the Program Counter Bank
    pub pc: u24,
    pub sp: u16, // Stack pointer
    pub p: StatusRegister,
    pub d: u16, // Direct page
    pub b: u8,  // Data bank
}

impl Registers {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_b(&self) -> u8 {
        self.b
    }

    pub fn set_b(&mut self, val: u8) {
        self.b = val;
    }

    pub fn get_d(&self) -> u16 {
        self.d
    }

    pub fn set_d(&mut self, val: u16) {
        self.d = val;
    }

    pub fn index_reg_16_bits(&self) -> bool {
        !self.p.e && !self.p.x_or_b
    }

    pub fn accumulator_16_bits(&self) -> bool {
        !self.p.e && !self.p.m
    }

    pub fn stack_pointer_16_bits(&self) -> bool {
        !self.p.e
    }

    // Gets the X register.
    // Whether it gets the whole thing or just the low bits
    // depends on the X flag.
    pub fn get_x(&self) -> u16 {
        if self.index_reg_16_bits() {
            self.x
        } else {
            self.x & 0xFF
        }
    }

    // Sets the X register.
    // Whether it sets the whole thing or just the low bits
    // depends on the X flag.
    pub fn set_x(&mut self, val: u16) {
        if self.index_reg_16_bits() {
            self.x = val;
        } else {
            self.x &= 0xFF00;
            self.x |= val & 0xFF;
        }
    }

    pub fn get_y(&self) -> u16 {
        if self.index_reg_16_bits() {
            self.y
        } else {
            self.y & 0xFF
        }
    }

    pub fn set_y(&mut self, val: u16) {
        if self.index_reg_16_bits() {
            self.y = val;
        } else {
            self.y &= 0xFF00;
            self.y |= val & 0xFF;
        }
    }

    // Gets the A register.
    // Whether it gets the whole thing or just the low bits
    // depends on the M flag.
    pub fn get_a(&self) -> u16 {
        if self.accumulator_16_bits() {
            self.a
        } else {
            self.a & 0xFF
        }
    }

    // Sets the A register.
    // Whether it sets the whole thing or just the low bits
    // depends on the M flag.
    pub fn set_a(&mut self, val: u16) {
        if self.accumulator_16_bits() {
            self.a = val;
        } else {
            self.a &= 0xFF00;
            self.a |= val & 0xFF;
        }
    }

    // Gets the SP register.
    // Whether it gets the whole thing or just the low bits
    // depends on the E flag.
    pub fn get_sp(&self) -> u16 {
        if self.stack_pointer_16_bits() {
            self.sp
        } else {
            self.sp & 0xFF
        }
    }

    // Sets the SP register.
    // Whether it sets the whole thing or just the low bits
    // depends on the E flag.
    pub fn set_sp(&mut self, val: u16) {
        if self.stack_pointer_16_bits() {
            self.sp = val;
        } else {
            self.sp &= 0xFF00;
            self.sp |= val & 0xFF;
        }
    }

    // Gets the P register.
    pub fn get_p(&self) -> u8 {
        self.p.get()
    }

    // Sets the P register.
    pub fn set_p(&mut self, val: u8) {
        self.p.set(val);
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct StatusRegister {
    pub n: bool, // Negative flag
    pub v: bool, // Overflow flag
    pub m: bool, // 8-bit accumulator and memory flag (native mode only)

    // X: 8-bit index flag (native mode only)
    // B: Break flag (emulation mode only)
    pub x_or_b: bool,

    pub d: bool, // Decimal flag
    pub i: bool, // IRQ disable
    pub z: bool, // Zero flag
    pub c: bool, // Carry flag

    pub e: bool, // 6502 emulation mode flag
}

impl StatusRegister {
    pub const fn new(data: u8) -> Self {
        Self {
            n: ((data >> 7) & 1) == 1,
            v: ((data >> 6) & 1) == 1,
            m: ((data >> 5) & 1) == 1,
            x_or_b: ((data >> 4) & 1) == 1,
            d: ((data >> 3) & 1) == 1,
            i: ((data >> 2) & 1) == 1,
            z: ((data >> 1) & 1) == 1,
            c: ((data >> 0) & 1) == 1,
            e: false,
        }
    }

    pub fn get(&self) -> u8 {
        ((self.n as u8) << 7)
            | ((self.v as u8) << 6)
            | ((self.m as u8) << 5)
            | ((self.x_or_b as u8) << 4)
            | ((self.d as u8) << 3)
            | ((self.i as u8) << 2)
            | ((self.z as u8) << 1)
            | ((self.c as u8) << 0)
    }

    pub fn set(&mut self, data: u8) {
        self.n = ((data >> 7) & 1) == 1;
        self.v = ((data >> 6) & 1) == 1;
        self.m = ((data >> 5) & 1) == 1;
        self.x_or_b = ((data >> 4) & 1) == 1;
        self.d = ((data >> 3) & 1) == 1;
        self.i = ((data >> 2) & 1) == 1;
        self.z = ((data >> 1) & 1) == 1;
        self.c = ((data >> 0) & 1) == 1;
    }
}

impl Ord for StatusRegister {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get().cmp(&other.get())
    }
}

impl PartialOrd for StatusRegister {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub interrupt_control: InterruptControl,
    pub waitstate_control: WaitstateControl,
    pub dma_channels: [DmaChannelRegisters; 8],
}

impl IoRegisters {
    pub fn new() -> Self {
        Default::default()
    }
}

bitfield! {
  /// 4200h - NMITIMEN - Interrupt Enable and Joypad Request (W)
  /// Configures NMI enablement, PPU H/V IRQs, and joypad reading
  #[derive(Clone, Copy, Default)]
  pub struct InterruptControl(u8);
  impl Debug;
  pub joypad_enable, _: 0;
  pub h_v_irq, _: 5, 4;
  pub vblank_nmi_enable, _: 7;
}

bitfield! {
  /// 420Dh - MEMSEL - Memory-2 Waitstate Control (W)
  /// Configures timers, IO, and high address access
  #[derive(Clone, Copy, Default)]
  pub struct WaitstateControl(u8);
  impl Debug;
  pub access_speed, _: 0;
}

impl WaitstateControl {
    pub fn high_rom_cycles(&self) -> u64 {
        if self.access_speed() {
            6
        } else {
            8
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct DmaChannelRegisters {
    pub setup: DmaSetup,
    // 43x1h - BBADx - DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus) (R/W)
    pub ppu_reg: u8,
    // Either holds HDMA Table address or current GP-DMA address
    pub addr: DmaAddr,
    // Either holds HDMA indirect address or remaining GP-DMA byte count
    pub indirect_addr_or_byte_count: IndirectAddrOrByteCount,
    pub hdma_table_curr_addr: HdmaTableCurrAddr,
    pub hda_line_counter: HdmaLineCounter,
    // 43xBh - UNUSEDx - Unused Byte (R/W)
    pub unused_byte: u8,
}

bitfield! {
  /// 43x0h - DMAPx - DMA/HDMA Parameters (R/W)
  /// Specifies the direction and address selection for a DMA channel
  #[derive(Clone, Copy, Default)]
  pub struct DmaSetup(u8);
  impl Debug;
  pub transfer_unit_select, _: 2, 0;
  pub gpdma_src_addr_step, _: 4, 3;
  pub hdma_addr_mode, _: 6;
  pub transfer_direction, _: 7;
}

impl DmaSetup {
    pub fn cpu_addr_step(&self) -> i32 {
        match self.gpdma_src_addr_step() {
            0b00 => 1,
            0b10 => -1,
            _ => 0,
        }
    }

    pub fn gpdma_ppu_reg_offsets(&self) -> &'static [u8] {
        match self.transfer_unit_select() {
            0b000 | 0b010 | 0b110 => &[0],
            0b001 | 0b101 => &[0, 1],
            0b011 | 0b111 => &[0, 0, 1, 1],
            0b100 | _ => &[0, 1, 2, 3],
        }
    }
}

bitfield! {
  /// 43x2h - A1TxL - HDMA Table Start Address (low) / DMA Current Addr (low) (R/W)
  /// 43x3h - A1TxH - HDMA Table Start Address (hi) / DMA Current Addr (hi) (R/W)
  /// 43x4h - A1Bx - HDMA Table Start Address (bank) / DMA Current Addr (bank) (R/W)
  /// Holds the three registers representing the HDMA Table address or current GP-DMA address
  #[derive(Clone, Copy, Default)]
  pub struct DmaAddr(u24);
  impl Debug;
  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
  pub u8, bank_byte, set_bank_byte: 23, 16;
}

bitfield! {
  /// 43x5h - DASxL - Indirect HDMA Address (low) / DMA Byte-Counter (low) (R/W)
  /// 43x6h - DASxH - Indirect HDMA Address (hi) / DMA Byte-Counter (hi) (R/W)
  /// 43x7h - DASBx - Indirect HDMA Address (bank) (R/W)
  /// Holds the three registers representing the HDMA Table address or current GP-DMA address
  #[derive(Clone, Copy, Default)]
  pub struct IndirectAddrOrByteCount(u24);
  impl Debug;
  pub u16, dma_byte_count, _: 15, 0;

  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
  pub u8, bank_byte, set_bank_byte: 23, 16;
}

bitfield! {
  /// 43x8h - A2AxL - HDMA Table Current Address (low) (R/W)
  /// 43x9h - A2AxH - HDMA Table Current Address (high) (R/W)
  /// For HDMA only, holds the current pointer into the HDMA Table
  #[derive(Clone, Copy, Default)]
  pub struct HdmaTableCurrAddr(u16);
  impl Debug;
  pub u8, lo_byte, set_lo_byte: 7, 0;
  pub u8, hi_byte, set_hi_byte: 15, 8;
}

bitfield! {
  /// 43xAh - NTRLx - HDMA Line-Counter (from current Table entry) (R/W)
  /// Holds the active HDMA table entry, with a decrementing line counter
  #[derive(Clone, Copy, Default)]
  pub struct HdmaLineCounter(u8);
  impl Debug;
  pub line_count, _: 6, 0;
  pub repeat, _: 7;
}
