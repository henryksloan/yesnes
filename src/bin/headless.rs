use std::time::Instant;
use yesnes::Device;

fn main() {
    let mut snes = yesnes::snes::SNES::new();
    let cart_path = std::env::args().nth(1).expect("Expected a rom file");
    snes.load_cart(&cart_path);
    snes.reset();
    loop {
        // for _ in 0..5 {
        const FRAMES_PER_CHECK: u32 = 60;
        let now = Instant::now();
        for _ in 0..FRAMES_PER_CHECK {
            while !snes.run_instruction_debug(Device::CPU).1 {}
        }
        let elapsed = now.elapsed();
        println!(
            "Average over {FRAMES_PER_CHECK} frames: {:.2?}",
            elapsed / FRAMES_PER_CHECK
        );
    }
}
