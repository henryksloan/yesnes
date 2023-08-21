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
