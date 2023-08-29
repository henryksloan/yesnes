pub mod counter;
mod registers;

pub use counter::PpuCounter;
use registers::IoRegisters;

use crate::scheduler::{Device, DeviceGenerator, YieldReason};

pub struct PPU {
    io_reg: IoRegisters,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            io_reg: IoRegisters::new(),
        }
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("PPU");
            yield (YieldReason::Sync(Device::CPU), 5);
        }
    }
}
