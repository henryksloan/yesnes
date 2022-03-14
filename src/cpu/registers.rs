use crate::u24::u24;

use paste::paste;

#[derive(Default)]
pub struct Registers {
    pub a: u16, // Accumulator register
    pub x: u16, // Index registers
    pub y: u16,
    // Program counter
    // The most significant byte is the Program Counter Bank
    pub pc: u24,
    pub sp: u16, // Stack pointer
    pub p: StatusRegister,
    pub d: u16, // Zero page offset
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

    // Gets the X register.
    // Whether it gets the whole thing or just the low bits
    // depends on the X flag.
    pub fn get_x(&mut self) -> u16 {
        if self.p.x() {
            self.x & 0xFF
        } else {
            self.x
        }
    }

    // Sets the X register.
    // Whether it sets the whole thing or just the low bits
    // depends on the X flag.
    pub fn set_x(&mut self, val: u16) {
        if self.p.x() {
            self.x &= 0xFF00;
            self.x |= val & 0xFF;
        } else {
            self.x = val;
        }
    }

    pub fn get_y(&mut self) -> u16 {
        if self.p.x() {
            self.y & 0xFF
        } else {
            self.y
        }
    }

    pub fn set_y(&mut self, val: u16) {
        if self.p.x() {
            self.y &= 0xFF00;
            self.y |= val & 0xFF;
        } else {
            self.y = val;
        }
    }

    // Gets the A register.
    // Whether it gets the whole thing or just the low bits
    // depends on the M flag.
    pub fn get_a(&mut self) -> u16 {
        if self.p.m() {
            self.a & 0xFF
        } else {
            self.a
        }
    }

    // Sets the A register.
    // Whether it sets the whole thing or just the low bits
    // depends on the X flag.
    pub fn set_a(&mut self, val: u16) {
        if self.p.m() {
            self.a &= 0xFF00;
            self.a |= val & 0xFF;
        } else {
            self.a = val;
        }
    }
}

#[derive(Default)]
pub struct StatusRegister {
    raw: u8, // Flag values, excluding those that depend on E
    e: bool, // 6502 emulation mode flag
    m: bool, // 8-bit accumulator and memory flag (native mode only)
    x: bool, // 8-bit index flag (native mode only)
    b: bool, // Break flag (emulation mode only)
}

impl StatusRegister {
    pub fn get(&self) -> u8 {
        if self.e {
            // Bit 5 is always 1 in emulation mode
            self.raw | (1 << 5) | ((self.b as u8) << 4)
        } else {
            self.raw | ((self.m as u8) << 5) | ((self.x as u8) << 4)
        }
    }

    pub fn emulation_mode(&self) -> bool {
        self.e
    }

    pub fn set_emulation_mode(&mut self, new_mode: bool) {
        // This should set M and X high
        // The instruction that effects this should also affect some other registers
        self.e = new_mode;
    }

    pub fn x(&self) -> bool {
        self.x
    }

    pub fn m(&self) -> bool {
        self.m
    }
}
