#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![feature(generators, generator_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]

mod bus;
mod cpu;
mod disassembler;
mod frontend;
mod memory;
mod ppu;
mod scheduler;
mod smp;
mod snes;
mod u24;

fn main() -> Result<(), eframe::Error> {
    env_logger::init();
    frontend::run()
}
