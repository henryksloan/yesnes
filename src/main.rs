// TODO: Remove this once I figure out how to get terminal logging on Windows
// #![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![feature(coroutines, coroutine_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]
#![feature(test)]

extern crate test;

mod apu;
mod bus;
mod cpu;
mod disassembler;
mod frontend;
mod memory;
mod ppu;
mod scheduler;
mod snes;
mod u24;

fn main() -> Result<(), eframe::Error> {
    env_logger::init();
    frontend::run()
}
