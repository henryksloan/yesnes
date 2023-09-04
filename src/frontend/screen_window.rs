use super::app_window::AppWindow;

use crate::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::sync::{Arc, Mutex};

pub struct ScreenWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    image: ColorImage,
    texture: Option<TextureHandle>,
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
        }
    }
}

impl AppWindow for ScreenWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        if let Ok(snes) = self.snes.lock() {
            if snes.cpu.borrow().debug_frame_ready {
                snes.cpu.borrow_mut().debug_frame_ready = false;
                let frame = snes.debug_get_frame();
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

                ui.image(self.texture.as_ref().unwrap(), [256., 224.]);
            });
    }
}
