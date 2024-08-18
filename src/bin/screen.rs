extern crate yesnes;

use yesnes::frame_history::FrameHistory;
use yesnes::snes::SNES;
use yesnes::Device;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::path::PathBuf;
use std::time::Instant;

// TODO: Add sound (ideally merge all this stuff with the debugger binary)
struct YesnesApp {
    snes: SNES,
    frame_history: FrameHistory,
    image: ColorImage,
    texture: Option<TextureHandle>,
    snes_frame_history: FrameHistory,
    previous_snes_frame_instant: Option<Instant>,
    lock_fps: bool,
}

impl Default for YesnesApp {
    fn default() -> Self {
        let mut snes = SNES::new();
        let cart_path = PathBuf::from(std::env::args().nth(1).expect("Expected a rom file"));
        snes.load_cart(&cart_path);
        snes.reset();
        let image = ColorImage::new([256, 224], Color32::BLACK);
        Self {
            snes,
            frame_history: FrameHistory::new(),
            image,
            texture: None,
            snes_frame_history: FrameHistory::new(),
            previous_snes_frame_instant: None,
            lock_fps: true,
        }
    }
}

impl eframe::App for YesnesApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        self.frame_history
            .on_new_frame(ctx.input(|i| i.time), frame.info().cpu_usage);

        let controller_state = ctx.input(|input_state| {
            const KEYS: &[egui::Key] = &[
                egui::Key::C,          // B
                egui::Key::X,          // Y
                egui::Key::Enter,      // Select
                egui::Key::Space,      // Start
                egui::Key::ArrowUp,    // Up
                egui::Key::ArrowDown,  // Down
                egui::Key::ArrowLeft,  // Left
                egui::Key::ArrowRight, // Right
                egui::Key::V,          // A
                egui::Key::D,          // X
                egui::Key::A,          // L
                egui::Key::S,          // R
            ];
            let mut controller_state = 0;
            for (i, key) in KEYS.iter().enumerate() {
                controller_state |= (input_state.key_down(*key) as u16) << (15 - i);
            }
            controller_state
        });
        self.snes.set_controller_state(0, controller_state);
        while !self.snes.run_instruction_debug(Device::CPU, None).1 {}

        let frame = self.snes.take_frame();
        if let Some(frame) = frame {
            let render_delta = self
                .previous_snes_frame_instant
                .map(|previous| (Instant::now() - previous).as_secs_f32());
            if let Some(delta) = render_delta {
                if self.lock_fps && delta < 0.016 {
                    std::thread::sleep(std::time::Duration::from_secs_f32(0.016 - delta));
                }
            }
            let after = Instant::now();
            let display_delta = self
                .previous_snes_frame_instant
                .map(|previous| (after - previous).as_secs_f32());
            self.snes_frame_history
                .on_new_frame(ctx.input(|i| i.time), display_delta);
            self.previous_snes_frame_instant = Some(Instant::now());
            for y in 0..224 {
                for x in 0..256 {
                    let color = frame[y][x];
                    self.image[(x, y)] = Color32::from_rgb(color[0], color[1], color[2]);
                }
            }
        }

        match &mut self.texture {
            Some(t) => t.set(self.image.clone(), egui::TextureOptions::NEAREST),
            None => {
                self.texture = Some(ctx.load_texture(
                    "snes-screen",
                    self.image.clone(),
                    egui::TextureOptions::NEAREST,
                ))
            }
        };

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label(format!(
                    "Mean frame time: {:.2}ms, SNES Mean frame time: {:.2}ms",
                    1e3 * self.frame_history.mean_frame_time(),
                    1e3 * self.snes_frame_history.mean_frame_time()
                ));
                ui.checkbox(&mut self.lock_fps, "Lock FPS");
            });
            ui.add(
                egui::Image::new(self.texture.as_ref().unwrap())
                    .maintain_aspect_ratio(true)
                    .shrink_to_fit(),
            );
        });

        // TODO: Enforce some sort of frame rate
        // Effectively puts egui in "continuous mode", where we repaint as quickly as possible (up to refresh rate)
        ctx.request_repaint();
    }
}

fn main() -> Result<(), eframe::Error> {
    env_logger::init();
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([880.0, 800.0]),
        ..Default::default()
    };
    eframe::run_native(
        "yesnes",
        options,
        Box::new(|_cc| Ok(Box::<YesnesApp>::default())),
    )
}
