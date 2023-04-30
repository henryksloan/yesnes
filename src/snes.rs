use crate::{bus::Bus, cpu::CPU, ppu::PPU, scheduler::Scheduler, smp::SMP};

use std::cell::RefCell;
use std::rc::Rc;

pub struct SNES {
    cpu: Rc<RefCell<CPU>>,
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,

    scheduler: Scheduler,
}

impl SNES {
    pub fn new() -> Self {
        let ppu = Rc::new(RefCell::new(PPU::new()));
        let smp = Rc::new(RefCell::new(SMP::new()));
        let bus = Rc::new(RefCell::new(Bus::new(ppu.clone(), smp.clone())));
        let cpu = Rc::new(RefCell::new(CPU::new(bus)));

        let cpu_thread = Box::from(CPU::run(cpu.clone()));
        let ppu_thread = Box::from(ppu.borrow_mut().run());
        let smp_thread = Box::from(smp.borrow_mut().run());
        let scheduler = Scheduler::new(cpu_thread, ppu_thread, smp_thread);

        cpu.borrow_mut().reset();

        Self {
            cpu,
            ppu,
            smp,
            scheduler,
        }
    }

    pub fn tick(&mut self) {
        self.scheduler.tick();
    }
}
