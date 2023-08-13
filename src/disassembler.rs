pub mod instruction_data;

pub use instruction_data::{AddressingMode, InstructionData, INSTRUCTION_DATA};

use std::cell::RefCell;
use std::rc::Rc;

use crate::bus::Bus;
use crate::cpu;
use crate::u24::u24;

pub struct Disassembler {
    bus: Rc<RefCell<Bus>>,
}

impl Disassembler {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self { bus }
    }

    pub fn disassemble(&mut self) {
        let pc = u24(Bus::peak_u16(self.bus.clone(), cpu::RESET_VECTOR) as u32);
    }
}
