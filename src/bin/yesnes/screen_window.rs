use super::app_window::AppWindow;
use std::time::Instant;

use yesnes::frame_history::FrameHistory;
use yesnes::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

pub struct ScreenWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    image: ColorImage,
    texture: Option<TextureHandle>,
    frame_ready: Arc<AtomicBool>,
    frame_history: FrameHistory,
    previous_frame_instant: Option<Instant>,
    lock_fps: bool,
}

impl ScreenWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>, frame_ready: Arc<AtomicBool>) -> Self {
        let id = egui::Id::new(&title);
        let image = ColorImage::new([256, 224], Color32::BLACK);
        Self {
            id,
            title,
            snes,
            image,
            texture: None,
            frame_ready,
            frame_history: FrameHistory::new(),
            previous_frame_instant: None,
            lock_fps: true,
        }
    }
}

impl AppWindow for ScreenWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        while !paused && !self.frame_ready.load(Ordering::Relaxed) {
            std::hint::spin_loop();
        }
        {
            // TODO: Much of this need not happen under the lock
            let mut frame = None;
            if let Ok(mut snes) = self.snes.lock() {
                self.frame_ready.store(false, Ordering::Relaxed);
                frame = snes.take_frame();
                if focused && !paused {
                    // TODO: Analyze input latency
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
                    snes.set_controller_state(0, controller_state);
                }
            }
            if let Some(frame) = frame {
                let render_delta = self
                    .previous_frame_instant
                    .map(|previous| (Instant::now() - previous).as_secs_f32());
                if let Some(delta) = render_delta {
                    if self.lock_fps && delta < 0.016 {
                        std::thread::sleep(std::time::Duration::from_secs_f32(0.016 - delta));
                    }
                }
                let display_delta = self
                    .previous_frame_instant
                    .map(|previous| (Instant::now() - previous).as_secs_f32());
                self.frame_history
                    .on_new_frame(ctx.input(|i| i.time), display_delta);
                self.previous_frame_instant = Some(Instant::now());
                for y in 0..224 {
                    for x in 0..256 {
                        let color = frame[y][x];
                        self.image[(x, y)] = Color32::from_rgb(color[0], color[1], color[2]);
                    }
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

        egui::Window::new(&self.title)
            .default_width(512.0)
            .default_height(512.0)
            .resizable(true)
            .scroll(egui::Vec2b::FALSE)
            .show(ctx, |ui| {
                if !focused {
                    ui.disable();
                }
                ui.horizontal(|ui| {
                    ui.label(format!(
                        "Mean frame time: {:.2}ms",
                        1e3 * self.frame_history.mean_frame_time()
                    ));
                    ui.checkbox(&mut self.lock_fps, "Lock FPS");
                });
                // TODO: This is a janky resizing implementation to maintain aspect ratio
                let rect = ui.available_rect_before_wrap();
                let new_size = {
                    let available_size = ui.available_size();
                    egui::Vec2::new(available_size.max_elem(), available_size.max_elem())
                };
                ui.allocate_space(new_size);
                ui.ctx().request_repaint();
                egui::Image::new(self.texture.as_ref().unwrap())
                    .maintain_aspect_ratio(true)
                    .paint_at(ui, rect);
            });
    }
}
