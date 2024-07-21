#![feature(coroutines, coroutine_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]
#![feature(test)]

mod apu;
mod bus;
mod cartridge;
mod cpu;
mod disassembler;
mod frontend;
mod memory;
mod ppu;
mod scheduler;
mod snes;
mod u24;

use std::time::Instant;

use crate::frontend::frame_history::FrameHistory;
use crate::scheduler::Device;
use crate::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

struct YesnesApp {
    snes: SNES,
    frame_history: FrameHistory,
    image: ColorImage,
    texture: Option<TextureHandle>,
    snes_frame_history: FrameHistory,
    previous_snes_frame_instant: Option<Instant>,
}

impl Default for YesnesApp {
    fn default() -> Self {
        let mut snes = SNES::new();
        let cart_path = std::env::args().nth(1).expect("Expected a rom file");
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
        self.snes.cpu.borrow_mut().controller_states[0] = controller_state;
        while !self.snes.run_instruction_debug(Device::CPU).1 {}

        let frame = self.snes.cpu.borrow_mut().debug_frame.take();
        if let Some(frame) = frame {
            let now = Instant::now();
            let delta = self
                .previous_snes_frame_instant
                .map(|previous| (now - previous).as_secs_f32());
            self.previous_snes_frame_instant = Some(now);
            self.snes_frame_history
                .on_new_frame(ctx.input(|i| i.time), delta);
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
            ui.label(format!(
                "Mean frame time: {:.2}ms",
                1e3 * self.frame_history.mean_frame_time()
            ));
            ui.label(format!(
                "SNES Mean frame time: {:.2}ms",
                1e3 * self.snes_frame_history.mean_frame_time()
            ));
            let rect_size = ui.available_rect_before_wrap().size();
            ui.add(
                egui::Image::new(self.texture.as_ref().unwrap())
                    .maintain_aspect_ratio(true)
                    .shrink_to_fit(), // .fit_to_exact_size([rect_size.x, rect_size.x * (224. / 255.)].into()),
            );
            // egui::Image::new(self.texture.as_ref().unwrap())
            //     .paint_at(ui, ui.available_rect_before_wrap());
            // ui.image(
            //     self.texture.as_ref().unwrap(),
            //     // [rect_size.x, rect_size.x * (224. / 255.)],
            // );
        });

        // Effectively puts egui in "continuous mode", where we repaint as quickly as possible (up to refresh rate)
        ctx.request_repaint();
    }
}

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1260.0, 1000.0]),
        ..Default::default()
    };
    eframe::run_native(
        "yesnes",
        options,
        Box::new(|_cc| Box::<YesnesApp>::default()),
    )
}
