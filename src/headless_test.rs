#![feature(coroutines, coroutine_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]
#![feature(stmt_expr_attributes)]
#![feature(test)]

mod apu;
mod bus;
mod cartridge;
mod cpu;
mod disassembler;
mod frontend;
mod memory;
mod ppu;
mod scheduler;
mod snes;
mod u24;

use crate::scheduler::Device;
use std::time::Instant;

fn main() {
    let mut snes = snes::SNES::new();
    let cart_path = std::env::args().nth(1).expect("Expected a rom file");
    snes.load_cart(&cart_path);
    snes.reset();
    loop {
        // for _ in 0..5 {
        const FRAMES_PER_CHECK: u32 = 60;
        let now = Instant::now();
        for _ in 0..FRAMES_PER_CHECK {
            while !snes.run_instruction_debug(Device::CPU).1 {}
        }
        let elapsed = now.elapsed();
        println!(
            "Average over {FRAMES_PER_CHECK} frames: {:.2?}",
            elapsed / FRAMES_PER_CHECK
        );
    }
}
