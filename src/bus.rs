use crate::ppu::PPU;
use crate::scheduler::{YieldReason, Yieldable};
use crate::smp::SMP;
use crate::u24::u24;

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

    // pub fn read_u8(&self, addr: u24) -> u8 {
    pub fn read_u8<'a>(&mut self, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            if false {
                yield YieldReason::SyncPPU(4);
            }
            6
        }
    }
}
