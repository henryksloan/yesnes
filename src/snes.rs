use crate::disassembler::{DebugCpu, DebugSmp};
use crate::scheduler::{DebugPoint, Device};
use crate::{apu::SMP, bus::Bus, cpu::CPU, ppu::PPU, scheduler::Scheduler};

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

pub struct SNES {
    // TODO: This encapsulation is nice, but pub(crate) might make frontend API too clunky
    pub(crate) cpu: Rc<RefCell<CPU>>,
    pub(crate) bus: Rc<RefCell<Bus>>,
    pub(crate) ppu: Rc<RefCell<PPU>>,
    pub(crate) smp: Rc<RefCell<SMP>>,

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

    // TODO: Decouple the individual processors so the tests don't depend on a modified SNES object
    pub fn new_test() -> Self {
        let ppu = Rc::new(RefCell::new(PPU::new()));
        let smp = Rc::new(RefCell::new(SMP::new_test()));
        let bus = Rc::new(RefCell::new(Bus::new_test(ppu.clone(), smp.clone())));
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

    pub fn load_cart(&mut self, cart_path: &Path) {
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

    // TODO: This is kind of a nonsense function
    pub fn run_instruction(&mut self) {
        self.scheduler.run_instruction();
    }

    pub fn run_instruction_debug(
        &mut self,
        run_device: Device,
        stop_condition: Option<DebugPoint>,
    ) -> (bool, bool) {
        self.scheduler
            .run_instruction_debug(run_device, stop_condition)
    }

    pub fn take_frame(&mut self) -> Option<[[[u8; 3]; 256]; 224]> {
        self.cpu.borrow_mut().debug_frame.take()
    }

    pub fn debug_compute_tile(
        &self,
        tile_chr_base: usize,
        bits_per_pixel: usize,
    ) -> [[Option<[u8; 3]>; 8]; 8] {
        self.ppu
            .borrow()
            .debug_compute_tile(tile_chr_base, bits_per_pixel)
    }

    pub fn set_controller_state(&mut self, controller_i: usize, data: u16) {
        if controller_i < 4 {
            self.cpu.borrow_mut().controller_states[controller_i] = data;
        }
    }

    pub fn make_debug_cpu(&self) -> DebugCpu {
        DebugCpu::new(self.bus.clone())
    }

    pub fn make_debug_smp(&self) -> DebugSmp {
        DebugSmp::new(self.smp.clone())
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
        // TODO: Fill in some local testdata
        snes.load_cart("");
        snes.reset();
        b.iter(move || while !snes.run_instruction_debug(Device::CPU, None).1 {});
    }

    // #[bench]
    // fn bench_600_frame(b: &mut Bencher) {
    //     let mut snes = SNES::new();
    //     snes.load_cart("");
    //     snes.reset();
    //     b.iter(move || {
    //         for _ in 0..600 {
    //             while !snes.run_instruction_debug(Device::CPU).1 {}
    //         }
    //     });
    // }
}
