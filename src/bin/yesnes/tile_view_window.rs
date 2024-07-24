use super::app_window::AppWindow;

use yesnes::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::sync::{Arc, Mutex};

pub struct TileViewWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    image: ColorImage,
    texture: Option<TextureHandle>,
    // TODO: A more dynamic mechanism for refreshing,
    // e.g. events (reset, new frame, step, etc.) rather than pausing and unpausing
    // and eventually fine-grained cache invalidation
    vram_stale: bool,
}

impl TileViewWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>) -> Self {
        let id = egui::Id::new(&title);
        let image = ColorImage::new([8, 8], Color32::BLACK);
        Self {
            id,
            title,
            snes,
            image,
            texture: None,
            vram_stale: true,
        }
    }

    fn refresh_vram_if_stale(&mut self, paused: bool) {
        self.vram_stale |= !paused;
        if self.vram_stale && paused {
            self.vram_stale = false;
            self.refresh_vram();
        }
    }

    fn refresh_vram(&mut self) {
        // TODO: Unwrapping this lock exposes us to poisoning... consider making such checks safe
        let snes = self.snes.lock().unwrap();
        // let tile = snes.debug_compute_tile(0xc060, 4);
        // let tile = snes.debug_compute_tile(0x6020, 4);
        let tile = snes.debug_compute_tile(0x4240, 4);
        for row in 0..8 {
            for col in 0..8 {
                let pixel = tile[row][col];
                self.image[(col, row)] = if let Some(color) = pixel {
                    Color32::from_rgb(color[0], color[1], color[2])
                } else {
                    Color32::BLACK
                };
            }
        }
    }
}

impl AppWindow for TileViewWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        self.refresh_vram_if_stale(paused);
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
                // egui::Image::new(self.texture.as_ref().unwrap())
                // ui.image(self.texture.as_ref().unwrap());
                ui.add(
                    egui::Image::new(self.texture.as_ref().unwrap())
                        .fit_to_exact_size(egui::vec2(64., 64.)),
                );
            });
    }
}
