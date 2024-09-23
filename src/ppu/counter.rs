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
    pub fn tick(&mut self, n_clocks: u16) -> bool {
        // TODO: Yield to CPU when we decide to latch the interlace
        let mut new_scanline = false;
        self.h_ticks += n_clocks;
        if self.h_ticks >= 1364 {
            new_scanline = true;
            self.h_ticks -= 1364;
            self.scanline += 1;
            // TODO: Overscan mode
            if self.scanline >= 261 {
                self.scanline -= 261;
            }
        }
        new_scanline
    }
}
