#![feature(generators, generator_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]

mod bus;
mod cpu;
mod memory;
mod ppu;
mod scheduler;
mod smp;
mod snes;
mod u24;

use snes::SNES;

fn main() {
    let mut snes = SNES::new();
    for _ in 0..100 {
        snes.tick();
    }
}
