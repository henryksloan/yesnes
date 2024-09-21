use crate::scheduler::*;

use std::cell::RefCell;
use std::rc::Rc;

/// Tracks the timing of the video scan... TODO: Complete this, including noting the frequency
pub struct PpuCounter {
    pub h_ticks: u16,
    pub scanline: u16,
}

impl PpuCounter {
    pub fn new() -> Self {
        Self {
            h_ticks: 0,
            scanline: 0,
        }
    }

    /// Ticks the counter by a number of ticks.
    /// Returns true if a new scanline has started
    // TODO: When this needs to get interlace, it needs to 1) sync the current thread to the CPU (?)
    // and 2) somehow get the interlace information (from the PPU?).
    pub fn tick<'a>(counter: Rc<RefCell<PpuCounter>>, n_clocks: u16) -> impl Yieldable<bool> + 'a {
        #[coroutine]
        move || {
            // TODO: Yield to CPU when we decide to latch the interlace
            let mut new_scanline = false;
            let counter = &mut *counter.borrow_mut();
            counter.h_ticks += n_clocks;
            if counter.h_ticks >= 1364 {
                new_scanline = true;
                counter.h_ticks -= 1364;
                counter.scanline += 1;
                // TODO: Overscan mode
                if counter.scanline >= 261 {
                    counter.scanline -= 261;
                }
            }
            new_scanline
        }
    }
}
