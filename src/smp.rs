use crate::scheduler::{DeviceGenerator, YieldReason};

pub struct SMP {}

impl SMP {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self) -> impl DeviceGenerator {
        move || loop {
            yield YieldReason::SyncCPU(5);
        }
    }
}
