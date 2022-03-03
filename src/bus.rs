use crate::ppu::PPU;
use crate::smp::SMP;

use std::cell::RefCell;
use std::rc::Rc;

pub struct Bus {
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,
}

impl Bus {
    pub fn new(ppu: Rc<RefCell<PPU>>, smp: Rc<RefCell<SMP>>) -> Self {
        Self { ppu, smp }
    }
}
