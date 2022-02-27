use crate::scheduler::DeviceThread;

#[derive(Clone, Copy, Debug)]
pub enum YieldReason {
    SyncCPU(u32),
    SyncPPU(u32),
    SyncSMP(u32),
}

impl YieldReason {
    pub fn to_thread_ticks_tuple(&self) -> (DeviceThread, u32) {
        match *self {
            Self::SyncCPU(n_ticks) => (DeviceThread::CPU, n_ticks),
            Self::SyncPPU(n_ticks) => (DeviceThread::PPU, n_ticks),
            Self::SyncSMP(n_ticks) => (DeviceThread::SMP, n_ticks),
        }
    }
}
