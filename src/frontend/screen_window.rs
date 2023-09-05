use super::app_window::AppWindow;
use super::frame_history::FrameHistory;
use std::time::Instant;

use crate::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::sync::{Arc, Mutex};

pub struct ScreenWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    image: ColorImage,
    texture: Option<TextureHandle>,
    frame_history: FrameHistory,
    previous_frame_instant: Option<Instant>,
}

impl ScreenWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>) -> Self {
        let id = egui::Id::new(&title);
        let image = ColorImage::new([256, 224], Color32::BLACK);
        Self {
            id,
            title,
            snes,
            image,
            texture: None,
            frame_history: FrameHistory::new(),
            previous_frame_instant: None,
        }
    }
}

impl AppWindow for ScreenWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        if let Ok(snes) = self.snes.lock() {
            if let Some(frame) = snes.cpu.borrow_mut().debug_frame.take() {
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
            // .default_height(640.0)
            .show(ctx, |ui| {
                // if !focused {
                //     ui.set_enabled(false);
                // }
                // egui::TopBottomPanel::top(self.id.with("menu_bar"))
                //     .show_inside(ui, |ui| self.menu_bar(ui));

                // let egui_image = egui::Image::new(self.texture.as_ref().unwrap(), [256., 224.]);
                // let rect = ctx.available_rect();
                // egui_image.paint_at(ui, rect);
                ui.label(format!(
                    "Mean frame time: {:.2}ms",
                    1e3 * self.frame_history.mean_frame_time()
                ));
                ui.image(self.texture.as_ref().unwrap(), [256., 224.]);
            });
    }
}
