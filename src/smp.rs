use crate::scheduler::{device_thread::DeviceThread, DeviceGenerator, YieldReason};

pub struct SMP {}

impl SMP {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            println!("SMP");
            yield (YieldReason::Sync(DeviceThread::CPU), 5);
        }
    }
}
