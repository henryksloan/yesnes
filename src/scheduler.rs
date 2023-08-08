pub mod yield_reason;

pub mod device_thread;
mod relative_clock;

pub use yield_reason::{YieldReason, YieldTicks};

use device_thread::DeviceThread;
use relative_clock::RelativeClock;

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

pub const CPU_FREQ: u64 = 21_477_272;
pub const PPU_FREQ: u64 = CPU_FREQ;
pub const SMP_FREQ: u64 = 24_576_000;

pub trait Yieldable<T> = Generator<Yield = YieldReason, Return = T>;
pub trait DeviceGenerator = Generator<Yield = YieldTicks, Return = !>;
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
            use crate::scheduler::device_thread::DeviceThread;
            yield YieldReason::Sync(DeviceThread::CPU);
        }
    };
}

pub(crate) use dummy_yield;

pub struct Scheduler {
    cpu: BoxGen,
    ppu: BoxGen,
    smp: BoxGen,
    curr: DeviceThread,
    // TODO: Really, these relative clocks only make sense if they're all relative to the same
    // thing, like CPU. Should refactor to that effect, but should work for now?
    // Correction... I think these have to form a tree.
    // TODO: Represent these with an abstraction with the right structure (tree?)
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

    // DO NOT SUBMIT: These two functions (and more like them) can be abstracted nicely (arbitrary termination conditions)
    pub fn run(&mut self) {
        let current_generator = match self.curr {
            DeviceThread::CPU => &mut self.cpu,
            DeviceThread::PPU => &mut self.ppu,
            DeviceThread::SMP => &mut self.smp,
        };
        let yielded = Pin::new(&mut *current_generator).resume(());
        match yielded {
            GeneratorState::Yielded((yield_reason, n_ticks)) => {
                if let YieldReason::Sync(other_device) = yield_reason {
                    println!("Yielded {n_ticks} to {other_device:?}");
                    self.sync_curr(other_device, n_ticks)
                }
            }
            _ => panic!("unexpected value from resume"),
        }
    }

    pub fn run_instruction(&mut self) {
        let current_generator = match self.curr {
            DeviceThread::CPU => &mut self.cpu,
            DeviceThread::PPU => &mut self.ppu,
            DeviceThread::SMP => &mut self.smp,
        };
        let yielded = Pin::new(&mut *current_generator).resume(());
        match yielded {
            GeneratorState::Yielded((YieldReason::Sync(other_device), n_ticks)) => {
                println!("Yielded {n_ticks} to {other_device:?}");
                self.sync_curr(other_device, n_ticks)
            }
            GeneratorState::Yielded((YieldReason::FinishedInstruction, n_ticks)) => {
                println!("Finished an instruction");
            }
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

    fn get_relative_clocks(&mut self, processor: DeviceThread) -> Vec<&mut RelativeClock> {
        match processor {
            DeviceThread::CPU => vec![&mut self.cpu_ppu_clock, &mut self.cpu_smp_clock],
            DeviceThread::PPU => vec![&mut self.cpu_ppu_clock],
            DeviceThread::SMP => vec![&mut self.cpu_smp_clock],
        }
    }

    fn sync_curr(&mut self, other_device: DeviceThread, n_ticks: u64) {
        let curr_device = self.curr;
        for relative_clock in self.get_relative_clocks(curr_device) {
            relative_clock.tick(curr_device, n_ticks);
        }
        // DO NOT SUBMIT: AAAAA, this doesn't work. There's no guarantee that control won't return to A before B has fully synced.
        // Ah!, but the scheduler itself can handle that! Handling a `Sync(B)` means to keep resuming B until it's synced with A.
        let relative_clock: &mut RelativeClock = self.get_relative_clock(curr_device, other_device);
        if relative_clock.is_ahead(curr_device) {
            self.curr = other_device;
        }
    }
}
