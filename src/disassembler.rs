use std::cell::RefCell;
use std::rc::Rc;

use crate::bus::Bus;

pub struct Disassembler {
    bus: Rc<RefCell<Bus>>,
}

impl Disassembler {
    pub fn disassemble() {}
}
