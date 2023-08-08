use crate::scheduler::DeviceThread;

#[derive(Clone, Copy, Debug)]
pub enum YieldReason {
    Sync(DeviceThread),
    FinishedInstruction,
}

pub type YieldTicks = (YieldReason, u64);
