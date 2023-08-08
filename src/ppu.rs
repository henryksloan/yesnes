use crate::scheduler::{device_thread::DeviceThread, DeviceGenerator, YieldReason};

pub struct PPU {}

impl PPU {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("PPU");
            yield (YieldReason::Sync(DeviceThread::CPU), 5);
        }
    }
}
