use super::app_window::AppWindow;
use super::frame_history::FrameHistory;
use std::time::Instant;

use crate::snes::SNES;

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

    title_bar: bool,
    closable: bool,
    collapsible: bool,
    resizable: bool,
    constrain: bool,
    scroll2: egui::Vec2b,
    disabled_time: f64,

    anchored: bool,
    anchor: egui::Align2,
    anchor_offset: egui::Vec2,
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

            title_bar: true,
            closable: true,
            collapsible: true,
            resizable: true,
            constrain: true,
            scroll2: egui::Vec2b::TRUE,
            disabled_time: f64::NEG_INFINITY,
            anchored: false,
            anchor: egui::Align2::RIGHT_TOP,
            anchor_offset: egui::Vec2::ZERO,
        }
    }
}

impl AppWindow for ScreenWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        if let Ok(true) =
            self.frame_ready
                .compare_exchange(true, false, Ordering::Relaxed, Ordering::Relaxed)
        {
            // TODO: Much of this need not happen under the lock
            let mut frame = None;
            if let Ok(snes) = self.snes.lock() {
                frame = snes.cpu.borrow_mut().debug_frame.take();
                if focused && !paused {
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
                    snes.cpu.borrow_mut().controller_states[0] = controller_state;
                }
            }
            if let Some(frame) = frame {
                let now = Instant::now();
                let delta = self
                    .previous_frame_instant
                    .map(|previous| (now - previous).as_secs_f32());
                self.previous_frame_instant = Some(now);
                self.frame_history
                    .on_new_frame(ctx.input(|i| i.time), delta);
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
            .default_width(256.0)
            .default_height(256.0)
            .resizable(true)
            .scroll2(egui::Vec2b::FALSE)
            .show(ctx, |ui| {
                if !focused {
                    ui.set_enabled(false);
                }
                ui.vertical(|ui| {
                    ui.label(format!(
                        "Mean frame time: {:.2}ms",
                        1e3 * self.frame_history.mean_frame_time()
                    ));
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
