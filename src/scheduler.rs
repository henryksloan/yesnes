pub mod yield_reason;

mod device_thread;
mod relative_clock;

pub use yield_reason::YieldReason;

use device_thread::DeviceThread;
use relative_clock::RelativeClock;

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

pub const CPU_FREQ: u32 = 21_477_272;
pub const PPU_FREQ: u32 = CPU_FREQ;
pub const SMP_FREQ: u32 = 24_576_000;

pub trait Yieldable<T> = Generator<Yield = YieldReason, Return = T>;
pub trait DeviceGenerator = Yieldable<!>;
pub trait InstructionGenerator = Yieldable<()>;
type BoxGen = Box<dyn Unpin + DeviceGenerator>;

macro_rules! yield_all {
    ($gen_expr:expr) => {{
        let mut gen = $gen_expr;
        loop {
            match Pin::new(&mut gen).resume(()) {
                GeneratorState::Yielded(yield_reason) => yield yield_reason,
                GeneratorState::Complete(out) => break out,
            }
        }
    }};
}

pub(crate) use yield_all;

// A silly hack:
// An expression only satisfies the Generator trait if it
// uses the `yield` keyword. But not all instructions yield,
// so this must be used to satisfy the trait
macro_rules! dummy_yield {
    () => {
        if false {
            yield YieldReason::SyncCPU(0);
        }
    };
}

pub(crate) use dummy_yield;

pub struct Scheduler {
    cpu: BoxGen,
    ppu: BoxGen,
    smp: BoxGen,
    curr: DeviceThread,
    cpu_ppu_clock: RelativeClock,
    cpu_smp_clock: RelativeClock,
}

impl Scheduler {
    pub fn new(cpu: BoxGen, ppu: BoxGen, smp: BoxGen) -> Self {
        Self {
            cpu,
            ppu,
            smp,
            curr: DeviceThread::CPU,
            cpu_ppu_clock: RelativeClock::new(
                DeviceThread::CPU,
                DeviceThread::PPU,
                CPU_FREQ,
                PPU_FREQ,
            ),
            cpu_smp_clock: RelativeClock::new(
                DeviceThread::CPU,
                DeviceThread::SMP,
                CPU_FREQ,
                SMP_FREQ,
            ),
        }
    }

    pub fn tick(&mut self) {
        let current_generator = match self.curr {
            DeviceThread::CPU => &mut self.cpu,
            DeviceThread::PPU => &mut self.ppu,
            DeviceThread::SMP => &mut self.smp,
        };
        let yielded = Pin::new(&mut *current_generator).resume(());
        match yielded {
            GeneratorState::Yielded(yield_reason) => self.sync_curr(yield_reason),
            _ => panic!("unexpected value from resume"),
        }
    }

    fn get_relative_clock(
        &mut self,
        processor_a: DeviceThread,
        processor_b: DeviceThread,
    ) -> &mut RelativeClock {
        match (processor_a, processor_b) {
            (DeviceThread::CPU, DeviceThread::PPU) | (DeviceThread::PPU, DeviceThread::CPU) => {
                &mut self.cpu_ppu_clock
            }
            (DeviceThread::CPU, DeviceThread::SMP) | (DeviceThread::SMP, DeviceThread::CPU) => {
                &mut self.cpu_smp_clock
            }
            _ => panic!(
                "no relative clock exists between {:?} and {:?}",
                processor_a, processor_b
            ),
        }
    }

    fn sync_curr(&mut self, yield_reason: YieldReason) {
        let curr_device = self.curr;
        let (other_device, n_ticks) = yield_reason.to_thread_ticks_tuple();
        let relative_clock: &mut RelativeClock = self.get_relative_clock(curr_device, other_device);
        relative_clock.tick(curr_device, n_ticks);
        if relative_clock.is_ahead(curr_device) {
            self.curr = other_device;
        }
    }
}
