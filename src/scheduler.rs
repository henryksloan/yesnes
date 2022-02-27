pub mod yield_reason;

mod device_thread;
mod relative_clock;

pub use yield_reason::YieldReason;

use device_thread::DeviceThread;
use relative_clock::RelativeClock;

use std::cell::RefCell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

pub const CPU_FREQ: u32 = 21_477_272;
pub const PPU_FREQ: u32 = CPU_FREQ;
pub const SMP_FREQ: u32 = 24_576_000;

pub trait DeviceGenerator = Generator<Yield = YieldReason, Return = !>;
pub trait InstructionGenerator = Generator<Yield = YieldReason, Return = ()>;
type RcGen<'a> = Rc<RefCell<dyn Unpin + Generator<Yield = YieldReason, Return = !> + 'a>>;

pub struct Scheduler<'a> {
    cpu: RcGen<'a>,
    ppu: RcGen<'a>,
    smp: RcGen<'a>,
    curr: DeviceThread,
    cpu_ppu_clock: RelativeClock,
    cpu_smp_clock: RelativeClock,
}

impl<'a> Scheduler<'a> {
    pub fn new(cpu: RcGen<'a>, ppu: RcGen<'a>, smp: RcGen<'a>) -> Self {
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
            DeviceThread::CPU => &self.cpu,
            DeviceThread::PPU => &self.ppu,
            DeviceThread::SMP => &self.smp,
        };
        let yielded = Pin::new(&mut *current_generator.borrow_mut()).resume(());
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
