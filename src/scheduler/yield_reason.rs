use crate::scheduler::DeviceThread;

#[derive(Clone, Copy, Debug)]
pub enum YieldReason {
    SyncCPU,
    SyncPPU,
    SyncSMP,
}

// TODO: I think I could just replace YieldReason with DeviceThread
impl YieldReason {
    pub fn to_thread(&self) -> DeviceThread {
        match *self {
            Self::SyncCPU => DeviceThread::CPU,
            Self::SyncPPU => DeviceThread::PPU,
            Self::SyncSMP => DeviceThread::SMP,
        }
    }
}

pub type YieldTicks = (YieldReason, u64);
