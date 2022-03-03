#![feature(generators, generator_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]

mod bus;
mod cpu;
mod ppu;
mod scheduler;
mod smp;
mod snes;

use crate::snes::SNES;

fn main() {
    /* let cpu = Rc::new(RefCell::new(CPU::new()));
    let ppu = Rc::new(RefCell::new(PPU::new()));
    let smp = Rc::new(RefCell::new(SMP::new()));
    let mut cpu_borrow = cpu.borrow_mut();
    let cpu_thread = Rc::new(RefCell::new(cpu_borrow.run()));
    let ppu_thread = Rc::new(RefCell::new(ppu.borrow_mut().run()));
    let smp_thread = Rc::new(RefCell::new(smp.borrow_mut().run()));
    let mut scheduler = Scheduler::new(cpu_thread, ppu_thread, smp_thread);
    scheduler.tick(); */
    let snes = SNES::new();
}
