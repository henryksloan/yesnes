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

    pub fn read_u8<'a>(bus: Rc<RefCell<Bus>>, addr: u24) -> impl Yieldable<u8> + 'a {
        move || {
            if false {
                yield YieldReason::SyncPPU;
            }
            // TODO: Some generalized mapper logic
            match addr.hi8() {
                0x00..=0x3F | 0x80..=0xBF => {
                    if addr.hi8() == 0x00 && (0xFF00..=0xFFFF).contains(&addr.lo16()) {
                        bus.borrow().cart_test[(0x7F00 | (addr.lo16() & 0xFF)) as usize]
                    } else {
                        match addr.lo16() {
                            0x0000..=0x7FFF => 0, // TODO: System area
                            0x8000.. => bus.borrow().cart_test[(addr.lo16() - 0x8000) as usize],
                        }
                    }
                }
                _ => 0,
            }
        }
    }
}
