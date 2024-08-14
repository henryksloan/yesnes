pub mod yield_reason;

pub mod device;
mod relative_clock;

pub use device::Device;
pub use relative_clock::RelativeClock;
pub use yield_reason::{Access, AccessType, DebugPoint, YieldReason, YieldTicks};

use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;

// Based on Higan's empirical numbers
// See also https://problemkaputt.de/fullsnes.htm#snestimings
pub const CPU_FREQ: u64 = 21_477_272;
pub const PPU_FREQ: u64 = CPU_FREQ;
// TODO: There may be a reason to clock the APU at double this, e.g.
// maybe to accurately clock timers. If so, waitstates should be halved.
pub const SMP_FREQ: u64 = 24_606_720 / 24;

// DO NOT SUBMIT: It's easy to forget to yield to these... how to warn on that?
// Filed https://github.com/rust-lang/rust/pull/129034 :)
pub trait Yieldable<T> = Coroutine<Yield = YieldReason, Return = T>;
pub trait DeviceCoroutine = Coroutine<Yield = YieldTicks, Return = !>;
pub trait InstructionCoroutine = Yieldable<()>;
type BoxGen = Box<dyn Unpin + DeviceCoroutine>;

struct DeviceThread {
    generator: BoxGen,
    waiting_for: Option<Device>,
}

impl DeviceThread {
    pub fn new(generator: BoxGen) -> Self {
        Self {
            generator,
            waiting_for: None,
        }
    }
}

macro_rules! yield_all {
    ($gen_expr:expr) => {{
        let mut gen = $gen_expr;
        loop {
            match Pin::new(&mut gen).resume(()) {
                CoroutineState::Yielded(yield_reason) => yield yield_reason,
                CoroutineState::Complete(out) => break out,
            }
        }
    }};
}

pub(crate) use yield_all;

macro_rules! ignore_yields {
    ($gen_expr:expr) => {{
        let mut gen = $gen_expr;
        loop {
            match Pin::new(&mut gen).resume(()) {
                CoroutineState::Complete(out) => break out,
                _ => {}
            }
        }
    }};
}

pub(crate) use ignore_yields;

pub struct Scheduler {
    cpu: DeviceThread,
    ppu: DeviceThread,
    smp: DeviceThread,
    curr: Device,
    // TODO: Really, these relative clocks only make sense if they're all relative to the same
    // thing, like CPU. Should refactor to that effect, but should work for now?
    // Correction... I think these have to form a tree.
    // TODO: Represent these with an abstraction with the right structure (tree?)
    // Each DeviceThread could have a list of pointers to relative clocks related to it, and these
    // could be built up from a Devices graph object
    cpu_ppu_clock: RelativeClock,
    cpu_smp_clock: RelativeClock,
}

impl Scheduler {
    pub fn new(cpu: BoxGen, ppu: BoxGen, smp: BoxGen) -> Self {
        Self {
            cpu: DeviceThread::new(cpu),
            ppu: DeviceThread::new(ppu),
            smp: DeviceThread::new(smp),
            curr: Device::CPU,
            cpu_ppu_clock: RelativeClock::new(Device::CPU, Device::PPU, CPU_FREQ, PPU_FREQ),
            cpu_smp_clock: RelativeClock::new(Device::CPU, Device::SMP, CPU_FREQ, SMP_FREQ),
        }
    }

    // TODO: These two functions (and more like them) can be abstracted nicely (arbitrary termination conditions)
    pub fn run(&mut self) {
        loop {
            let yielded = self.resume();
            match yielded {
                CoroutineState::Yielded((yield_reason, n_ticks)) => {
                    self.tick_curr_clocks(n_ticks);
                    if let YieldReason::Sync(other_device) = yield_reason {
                        self.curr_thread().waiting_for = Some(other_device);
                    }
                }
            }
        }
    }

    pub fn run_instruction(&mut self) {
        loop {
            let yielded = self.resume();
            match yielded {
                CoroutineState::Yielded((yield_reason, n_ticks)) => {
                    self.tick_curr_clocks(n_ticks);
                    match yield_reason {
                        YieldReason::Sync(other_device) => {
                            self.curr_thread().waiting_for = Some(other_device);
                        }
                        YieldReason::FinishedInstruction(_) => {
                            break;
                        }
                        YieldReason::Debug(debug_point) => {
                            if let DebugPoint::UnimplementedAccess(access) = debug_point {
                                log::debug!(
                                    "Unimplemented {:?} of {:#08}",
                                    access.access_type,
                                    access.addr
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    pub fn run_instruction_debug(
        &mut self,
        run_device: Device,
        stop_condition: Option<DebugPoint>,
    ) -> (bool, bool) {
        let mut hit_debug = false;
        let mut frame_ready = false;
        loop {
            let yielded = self.resume();
            match yielded {
                CoroutineState::Yielded((yield_reason, n_ticks)) => {
                    self.tick_curr_clocks(n_ticks);
                    match yield_reason {
                        YieldReason::Sync(other_device) => {
                            self.curr_thread().waiting_for = Some(other_device);
                        }
                        YieldReason::FinishedInstruction(device) if device == run_device => {
                            return (hit_debug, frame_ready);
                        }
                        YieldReason::FrameReady => {
                            frame_ready = true;
                        }
                        // TODO: Should this special case also apply to CodeBreakpoint?
                        YieldReason::Debug(DebugPoint::Breakpoint) => {
                            return (true, frame_ready);
                        }
                        YieldReason::Debug(debug_point) => {
                            if stop_condition
                                .is_some_and(|stop_condition| stop_condition == debug_point)
                                || debug_point == DebugPoint::CodeBreakpoint
                            {
                                hit_debug = true;
                            }
                            if let DebugPoint::UnimplementedAccess(access) = debug_point {
                                log::debug!(
                                    "Unimplemented {:?} of {:#08}",
                                    access.access_type,
                                    access.addr
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn curr_thread(&mut self) -> &mut DeviceThread {
        match self.curr {
            Device::CPU => &mut self.cpu,
            Device::PPU => &mut self.ppu,
            Device::SMP => &mut self.smp,
        }
    }

    fn resume(&mut self) -> CoroutineState<YieldTicks, !> {
        if let Some(waiting_for) = self.curr_thread().waiting_for {
            // If this thread is waiting for another, check if the other thread has caught up.
            // If it has, remove the `waiting_for` relationship. If not, yield to the other thread.
            self.sync_curr(waiting_for);
        }
        let new_curr_thread = self.curr_thread();
        Pin::new(&mut *new_curr_thread.generator).resume(())
    }

    fn get_relative_clock(
        &mut self,
        processor_a: Device,
        processor_b: Device,
    ) -> &mut RelativeClock {
        match (processor_a, processor_b) {
            (Device::CPU, Device::PPU) | (Device::PPU, Device::CPU) => &mut self.cpu_ppu_clock,
            (Device::CPU, Device::SMP) | (Device::SMP, Device::CPU) => &mut self.cpu_smp_clock,
            _ => panic!(
                "no relative clock exists between {:?} and {:?}",
                processor_a, processor_b
            ),
        }
    }

    fn tick_curr_clocks(&mut self, n_ticks: u64) {
        let curr_device = self.curr;
        let relative_clocks: &mut [&mut RelativeClock] = match curr_device {
            Device::CPU => &mut [&mut self.cpu_ppu_clock, &mut self.cpu_smp_clock],
            Device::PPU => &mut [&mut self.cpu_ppu_clock],
            Device::SMP => &mut [&mut self.cpu_smp_clock],
        };
        for relative_clock in relative_clocks {
            relative_clock.tick(curr_device, n_ticks);
        }
    }

    fn sync_curr(&mut self, other_device: Device) {
        let curr_device = self.curr;
        let curr_is_ahead = self
            .get_relative_clock(curr_device, other_device)
            .is_ahead(curr_device);
        let curr_thread = self.curr_thread();
        if curr_is_ahead {
            curr_thread.waiting_for = Some(other_device);
            self.curr = other_device;
        } else {
            curr_thread.waiting_for = None;
        }
    }
}
