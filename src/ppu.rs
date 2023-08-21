use crate::scheduler::{Device, DeviceGenerator, YieldReason};

pub struct PPU {}

impl PPU {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("PPU");
            yield (YieldReason::Sync(Device::CPU), 5);
        }
    }
}
