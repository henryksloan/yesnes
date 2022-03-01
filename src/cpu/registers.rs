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
    pub db: u8, // Data bank
}

impl Registers {
    pub fn new() -> Self {
        Default::default()
    }
}

// A faux primative to store 24-bit addresses
#[derive(Copy, Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct u24(u32);

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
        self.e = new_mode;
    }
}
