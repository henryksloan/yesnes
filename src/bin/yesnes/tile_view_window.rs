use super::app_window::AppWindow;

use yesnes::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::sync::{Arc, Mutex};

pub struct TileViewWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    images: [ColorImage; 256],
    textures: [Option<TextureHandle>; 256],
    // TODO: A more dynamic mechanism for refreshing,
    // e.g. events (reset, new frame, step, etc.) rather than pausing and unpausing
    // and eventually fine-grained cache invalidation
    vram_stale: bool,
}

impl TileViewWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>) -> Self {
        let id = egui::Id::new(&title);
        Self {
            id,
            title,
            snes,
            images: core::array::from_fn(|_| ColorImage::new([8, 8], Color32::BLACK)),
            textures: core::array::from_fn(|_| None),
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
        for i in 0..256 {
            // let tile = snes.debug_compute_tile(0x6000 + 0x10 * i, 4);
            let tile = snes.debug_compute_tile(0x3000 + 0x10 * i, 4);
            // let tile = snes.debug_compute_tile(0x2000 + 0x10 * i, 4);
            for row in 0..8 {
                for col in 0..8 {
                    let pixel = tile[row][col];
                    self.images[i][(col, row)] = if let Some(color) = pixel {
                        Color32::from_rgb(color[0], color[1], color[2])
                    } else {
                        Color32::BLACK
                    };
                }
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
        for i in 0..256 {
            match &mut self.textures[i] {
                Some(t) => t.set(self.images[i].clone(), egui::TextureOptions::NEAREST),
                None => {
                    self.textures[i] = Some(ctx.load_texture(
                        format!("tile{}", i),
                        self.images[i].clone(),
                        egui::TextureOptions::NEAREST,
                    ))
                }
            };
        }

        egui::Window::new(&self.title)
            .default_width(256.0)
            .default_height(256.0)
            .resizable(true)
            .scroll2(egui::Vec2b::FALSE)
            .show(ctx, |ui| {
                if !focused {
                    ui.set_enabled(false);
                }
                if ui.button("⟳").clicked() {
                    self.refresh_vram();
                }
                ui.vertical(|ui| {
                    ui.style_mut().spacing.item_spacing = egui::vec2(0.0, 0.0);
                    for i in 0..16 {
                        ui.horizontal(|ui| {
                            for j in 0..16 {
                                ui.add(
                                    egui::Image::new(self.textures[i * 16 + j].as_ref().unwrap())
                                        .fit_to_exact_size(egui::vec2(32., 32.)),
                                    // .fit_to_exact_size(egui::vec2(16., 16.)),
                                );
                            }
                        });
                    }
                });
            });
    }
}
