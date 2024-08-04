use crate::scheduler::Device;
use crate::u24::u24;

#[derive(Clone, Copy, Debug)]
pub enum YieldReason {
    Sync(Device),
    FinishedInstruction(Device),
    Debug(DebugPoint),
    FrameReady,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AccessType {
    Read,
    Write,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Access {
    pub access_type: AccessType,
    pub addr: u24,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DebugPoint {
    CodeBreakpoint,
    UnimplementedAccess(Access),
    // TODO: Should these be per-device?
    Breakpoint,
    StartedInterrupt,
    FinishedInterrupt,
}

pub type YieldTicks = (YieldReason, u64);
