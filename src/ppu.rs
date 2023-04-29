use crate::scheduler::{DeviceGenerator, YieldReason};

pub struct PPU {}

impl PPU {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("PPU");
            yield (YieldReason::SyncCPU, 5);
        }
    }
}
