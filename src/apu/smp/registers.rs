#[derive(Default, Clone, Copy)]
pub struct Registers {
    pub a: u8, // Accumulator register
    pub x: u8, // Index registers
    pub y: u8,
    // Program counter
    pub pc: u16,
    pub sp: u8, // Stack pointer
    pub psw: StatusRegister,
}

impl Registers {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_ya(&self) -> u16 {
        ((self.y as u16) << 8) | (self.a as u16)
    }

    pub fn set_ya(&mut self, val: u16) {
        self.y = (val >> 8) as u8;
        self.a = val as u8;
    }
}

#[derive(Default, Clone, Copy)]
pub struct StatusRegister {
    pub n: bool, // Negative flag
    pub v: bool, // Overflow flag
    pub p: bool, // Direct page location (0=00xx, 1=01xx)
    pub b: bool, // Break flag
    pub h: bool, // Half-carry
    pub i: bool, // IRQ enable (unused in APU)
    pub z: bool, // Zero flag
    pub c: bool, // Carry flag
}

impl StatusRegister {
    pub fn direct_page_addr(&self) -> u16 {
        if self.p {
            0x0100
        } else {
            0x0000
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct IoRegisters {
    pub ports: [u8; 4],
}

impl IoRegisters {
    pub fn new() -> Self {
        Default::default()
    }
}
