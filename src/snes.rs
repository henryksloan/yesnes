use crate::scheduler::Device;
use crate::u24::u24;
use crate::{apu::SMP, bus::Bus, cpu::CPU, ppu::PPU, scheduler::Scheduler};

use std::cell::RefCell;
use std::rc::Rc;

pub struct SNES {
    pub cpu: Rc<RefCell<CPU>>,
    pub bus: Rc<RefCell<Bus>>,
    ppu: Rc<RefCell<PPU>>,
    pub smp: Rc<RefCell<SMP>>,

    scheduler: Scheduler,
}

unsafe impl Send for SNES {}

impl SNES {
    pub fn new() -> Self {
        let ppu = Rc::new(RefCell::new(PPU::new()));
        let smp = Rc::new(RefCell::new(SMP::new()));
        let bus = Rc::new(RefCell::new(Bus::new(ppu.clone(), smp.clone())));
        let cpu = Rc::new(RefCell::new(CPU::new(bus.clone())));
        bus.borrow_mut().connect_cpu(Rc::downgrade(&cpu));

        let scheduler = Self::create_scheduler(cpu.clone(), ppu.clone(), smp.clone());

        Self {
            cpu,
            ppu,
            smp,
            bus,
            scheduler,
        }
    }

    fn create_scheduler(
        cpu: Rc<RefCell<CPU>>,
        ppu: Rc<RefCell<PPU>>,
        smp: Rc<RefCell<SMP>>,
    ) -> Scheduler {
        let cpu_thread = Box::from(CPU::run(cpu.clone()));
        let ppu_thread = Box::from(PPU::run(ppu.clone()));
        let smp_thread = Box::from(SMP::run(smp.clone()));
        Scheduler::new(cpu_thread, ppu_thread, smp_thread)
    }

    pub fn load_cart(&mut self, cart_path: &str) {
        self.bus.borrow_mut().load_cart(cart_path);
    }

    pub fn reset(&mut self) {
        self.cpu.borrow_mut().reset();
        self.bus.borrow_mut().reset();
        self.ppu.borrow_mut().reset();
        SMP::reset(self.smp.clone());
        self.scheduler =
            Self::create_scheduler(self.cpu.clone(), self.ppu.clone(), self.smp.clone());
    }

    pub fn run(&mut self) {
        self.scheduler.run();
    }

    pub fn run_instruction(&mut self) {
        self.scheduler.run_instruction();
    }

    pub fn run_instruction_debug(&mut self, run_device: Device) -> (bool, bool) {
        self.scheduler.run_instruction_debug(run_device)
    }

    pub fn debug_get_frame(&self) -> [[[u8; 3]; 256]; 224] {
        self.ppu.borrow().debug_get_frame()
    }

    pub fn device_peak_u8(&self, device: Device, addr: u24) -> u8 {
        match device {
            Device::CPU => Bus::peak_u8(self.bus.clone(), addr),
            // TODO: Peaking SMP, etc.
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate test;

    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_1_frame(b: &mut Bencher) {
        let mut snes = SNES::new();
        snes.load_cart("/home/henry/roms/snes/Harvest Moon (USA).sfc");
        snes.reset();
        b.iter(move || while !snes.run_instruction_debug(Device::CPU).1 {});
    }

    // #[bench]
    // fn bench_600_frame(b: &mut Bencher) {
    //     let mut snes = SNES::new();
    //     snes.load_cart("/home/henry/roms/snes/Harvest Moon (USA).sfc");
    //     snes.reset();
    //     b.iter(move || {
    //         for _ in 0..600 {
    //             while !snes.run_instruction_debug(Device::CPU).1 {}
    //         }
    //     });
    // }
}
