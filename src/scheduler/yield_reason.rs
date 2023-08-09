use crate::scheduler::Device;

#[derive(Clone, Copy, Debug)]
pub enum YieldReason {
    Sync(Device),
    FinishedInstruction,
}

pub type YieldTicks = (YieldReason, u64);
