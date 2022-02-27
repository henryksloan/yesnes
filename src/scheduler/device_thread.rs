#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeviceThread {
    CPU,
    PPU,
    SMP,
}
