use crate::u24::u24;

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

    // TODO: These should all handle emulation flag

    // Gets the X register.
    // Whether it gets the whole thing or just the low bits
    // depends on the X flag.
    pub fn get_x(&self) -> u16 {
        if self.p.x_or_b {
            self.x & 0xFF
        } else {
            self.x
        }
    }

    // Sets the X register.
    // Whether it sets the whole thing or just the low bits
    // depends on the X flag.
    pub fn set_x(&mut self, val: u16) {
        if self.p.x_or_b {
            self.x &= 0xFF00;
            self.x |= val & 0xFF;
        } else {
            self.x = val;
        }
    }

    pub fn get_y(&self) -> u16 {
        if self.p.x_or_b {
            self.y & 0xFF
        } else {
            self.y
        }
    }

    pub fn set_y(&mut self, val: u16) {
        if self.p.x_or_b {
            self.y &= 0xFF00;
            self.y |= val & 0xFF;
        } else {
            self.y = val;
        }
    }

    // Gets the A register.
    // Whether it gets the whole thing or just the low bits
    // depends on the M flag.
    pub fn get_a(&self) -> u16 {
        if self.p.m {
            self.a & 0xFF
        } else {
            self.a
        }
    }

    // Sets the A register.
    // Whether it sets the whole thing or just the low bits
    // depends on the M flag.
    pub fn set_a(&mut self, val: u16) {
        if self.p.m {
            self.a &= 0xFF00;
            self.a |= val & 0xFF;
        } else {
            self.a = val;
        }
    }

    // Gets the SP register.
    // Whether it gets the whole thing or just the low bits
    // depends on the E flag.
    pub fn get_sp(&self) -> u16 {
        if self.p.e {
            self.sp & 0xFF
        } else {
            self.sp
        }
    }

    // Sets the SP register.
    // Whether it sets the whole thing or just the low bits
    // depends on the E flag.
    pub fn set_sp(&mut self, val: u16) {
        if self.p.e {
            self.sp &= 0xFF00;
            self.sp |= val & 0xFF;
        } else {
            self.sp = val;
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

#[derive(Default)]
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

    // TODO: The emulation flag is largely unimplemented
    pub e: bool, // 6502 emulation mode flag
}

impl StatusRegister {
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
