use crate::ppu::PPU;
use crate::scheduler::{YieldReason, Yieldable};
use crate::smp::SMP;
use crate::u24::u24;

use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

pub struct Bus {
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,
    cart_test: Vec<u8>,
}

impl Bus {
    pub fn new(ppu: Rc<RefCell<PPU>>, smp: Rc<RefCell<SMP>>) -> Self {
        Self {
            ppu,
            smp,
            cart_test: fs::read("/home/henry/roms/snes/Harvest Moon (USA).sfc").unwrap(),
        }
    }

    pub fn read_u8<'a>(&mut self, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            if false {
                yield YieldReason::SyncPPU;
            }
            println!("Read from {:?}", addr);
            6
        }
    }
}
