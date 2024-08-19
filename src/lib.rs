// TODO: Remove this once I figure out how to get terminal logging on Windows
// #![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![feature(coroutines, coroutine_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]
#![feature(exhaustive_patterns)]
#![feature(test)]

extern crate test;

mod apu;
mod bus;
mod cartridge;
mod cpu;
pub mod disassembler;
pub mod frame_history;
pub mod numeric_types;
mod ppu;
mod scheduler;
pub mod snes;
pub mod u24;

pub use scheduler::Device;

pub mod debug_cpu {
    pub use crate::cpu::{Registers, StatusRegister};
}

pub mod debug_smp {
    pub use crate::apu::smp::{Registers, StatusRegister};
}

pub mod debug_point {
    pub use crate::scheduler::{Access, AccessType, DebugPoint};
}
