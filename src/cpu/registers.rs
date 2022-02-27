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

    pub e: bool, // Emulation mode flag
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
pub struct StatusRegister(u8);
