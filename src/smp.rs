use crate::scheduler::{device::Device, DeviceGenerator, YieldReason};

pub struct SMP {}

impl SMP {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("SMP");
            yield (YieldReason::Sync(Device::CPU), 5);
        }
    }
}
