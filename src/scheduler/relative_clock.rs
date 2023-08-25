use crate::scheduler::Device;

/// Tracks the relative time of two processors.
/// When counter >= 0, processor A is ahead;
/// when counter < 0, processor B is ahead.
/// Note that, when counter == 0, they are perfectly synchronized;
/// processor A is arbitrarily chosen to tick next.
/// https://near.sh/articles/design/schedulers
pub struct RelativeClock {
    counter: i64,
    processor_a: Device,
    processor_b: Device,
    scalar_a: u64,
    scalar_b: u64,
}

// TODO: Subtract the smaller value sometimes to avoid overflow
impl RelativeClock {
    pub fn new(
        processor_a: Device,
        processor_b: Device,
        frequency_a: u64,
        frequency_b: u64,
    ) -> Self {
        let factor = gcd::binary_u64(frequency_a, frequency_b);
        Self {
            counter: 0,
            processor_a,
            processor_b,
            scalar_a: frequency_a / factor,
            scalar_b: frequency_b / factor,
        }
    }

    pub fn tick(&mut self, processor: Device, n_ticks: u64) {
        if processor == self.processor_a {
            self.tick_a(n_ticks);
        } else if processor == self.processor_b {
            self.tick_b(n_ticks);
        } else {
            panic!(
                "processor {:?} is not tracked by this relative clock",
                processor
            );
        }
    }

    pub fn get_delta(&self, processor: Device) -> i64 {
        if processor == self.processor_a {
            self.counter
        } else if processor == self.processor_b {
            -self.counter
        } else {
            panic!(
                "processor {:?} is not tracked by this relative clock",
                processor
            );
        }
    }

    pub fn is_ahead(&self, processor: Device) -> bool {
        if processor == self.processor_a {
            self.a_ahead()
        } else if processor == self.processor_b {
            self.b_ahead()
        } else {
            panic!(
                "processor {:?} is not tracked by this relative clock",
                processor
            );
        }
    }

    /// Increments the counter by the number of processor A ticks.
    /// Note that each processor tick moves the counter in proportion
    /// to the *other* processor's frequency.
    fn tick_a(&mut self, n_ticks: u64) {
        self.counter += (n_ticks * self.scalar_b) as i64;
    }

    /// Like tick_a, but for processor B
    fn tick_b(&mut self, n_ticks: u64) {
        self.counter -= (n_ticks * self.scalar_a) as i64;
    }

    fn a_ahead(&self) -> bool {
        self.counter >= 0
    }

    fn b_ahead(&self) -> bool {
        self.counter < 0
    }
}
