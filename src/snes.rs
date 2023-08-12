use crate::{bus::Bus, cpu::CPU, ppu::PPU, scheduler::Scheduler, smp::SMP};

use std::cell::RefCell;
use std::rc::Rc;

pub struct SNES {
    pub cpu: Rc<RefCell<CPU>>,
    pub bus: Rc<RefCell<Bus>>,
    ppu: Rc<RefCell<PPU>>,
    smp: Rc<RefCell<SMP>>,

    scheduler: Scheduler,
}

impl SNES {
    pub fn new() -> Self {
        let ppu = Rc::new(RefCell::new(PPU::new()));
        let smp = Rc::new(RefCell::new(SMP::new()));
        let bus = Rc::new(RefCell::new(Bus::new(ppu.clone(), smp.clone())));
        let cpu = Rc::new(RefCell::new(CPU::new(bus.clone())));

        let cpu_thread = Box::from(CPU::run(cpu.clone()));
        let ppu_thread = Box::from(ppu.borrow_mut().run());
        let smp_thread = Box::from(smp.borrow_mut().run());
        let scheduler = Scheduler::new(cpu_thread, ppu_thread, smp_thread);

        let mut snes = Self {
            cpu,
            ppu,
            smp,
            bus,
            scheduler,
        };
        snes.reset();

        snes
    }

    pub fn reset(&mut self) {
        self.cpu.borrow_mut().reset();
        self.bus.borrow_mut().reset();
    }

    pub fn run(&mut self) {
        self.scheduler.run();
    }

    pub fn run_instruction(&mut self) {
        self.scheduler.run_instruction();
    }
}
