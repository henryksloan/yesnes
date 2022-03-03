use crate::{
    bus::Bus,
    cpu::CPU,
    ppu::PPU,
    scheduler::{RcGen, Scheduler},
    smp::SMP,
};

use std::cell::RefCell;
use std::rc::Rc;

pub struct SNES {
    cpu: Rc<RefCell<CPU>>,
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,
    cpu_thread: RcGen,
    ppu_thread: RcGen,
    smp_thread: RcGen,

    scheduler: Scheduler,
}

impl SNES {
    pub fn new() -> Self {
        let ppu = Rc::new(RefCell::new(PPU::new()));
        let smp = Rc::new(RefCell::new(SMP::new()));
        let bus = Bus::new(ppu.clone(), smp.clone());
        let cpu = Rc::new(RefCell::new(CPU::new(bus)));

        let cpu_thread = Rc::new(RefCell::new(cpu.borrow_mut().run()));
        let ppu_thread = Rc::new(RefCell::new(ppu.borrow_mut().run()));
        let smp_thread = Rc::new(RefCell::new(smp.borrow_mut().run()));
        let scheduler = Scheduler::new(cpu_thread.clone(), ppu_thread.clone(), smp_thread.clone());
        Self {
            cpu,
            ppu,
            smp,
            cpu_thread,
            ppu_thread,
            smp_thread,
            scheduler,
        }
    }
}
