use super::app_window::AppWindow;

use yesnes::snes::SNES;

use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::sync::{Arc, Mutex};

pub struct TileViewWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    images: Vec<ColorImage>,
    textures: Vec<Option<TextureHandle>>,
    // TODO: A more dynamic mechanism for refreshing,
    // e.g. events (reset, new frame, step, etc.) rather than pausing and unpausing
    // and eventually fine-grained cache invalidation
    vram_stale: bool,
    refreshed: bool,
    prev_scroll: f32,
    top_scroll_row: usize,
}

impl TileViewWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>) -> Self {
        let id = egui::Id::new(&title);
        Self {
            id,
            title,
            snes,
            images: vec![ColorImage::new([8, 8], Color32::BLACK); 2048],
            textures: vec![None; 2048],
            vram_stale: true,
            refreshed: false,
            prev_scroll: 0.,
            top_scroll_row: 0,
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
        // TODO: Also, locking regardless of pause is dangerous/wrong, can cause deadlocks
        self.refreshed = true;
        let snes = self.snes.lock().unwrap();
        for i in 0..2048 {
            let tile = snes.debug_compute_tile(0x10 * i, 4);
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
        if self.refreshed {
            self.refreshed = false;
            for i in 0..2048 {
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
        }

        egui::Window::new(&self.title)
            .default_width(320.0)
            .default_height(340.0)
            .resizable(true)
            .scroll(egui::Vec2b::FALSE)
            .show(ctx, |ui| {
                if !focused {
                    ui.disable();
                }
                let mut new_base_addr = None;
                ui.horizontal(|ui| {
                    if ui.button("⟳").clicked() {
                        self.refresh_vram();
                    }
                    let old_base_addr = 0x200 * self.top_scroll_row;
                    let mut base_addr = old_base_addr;
                    ui.add(
                        egui::DragValue::new(&mut base_addr)
                            .prefix("Base word=0x")
                            .speed(0)
                            .range(0..=0xFE00)
                            .hexadecimal(4, false, true)
                            .custom_parser(|s| {
                                u32::from_str_radix(s, 16)
                                    .map(|n| (n as usize & 0xFFFF) as f64)
                                    .ok()
                            })
                            .update_while_editing(false),
                    );
                    if base_addr != old_base_addr {
                        new_base_addr = Some(base_addr);
                    }
                });
                egui::ScrollArea::vertical().show(ui, |ui| {
                    let tile_width = ui.available_width() / 16.;
                    let margin = ui.visuals().clip_rect_margin;
                    let current_scroll =
                        (ui.clip_rect().top() - ui.min_rect().top() + margin) / tile_width;
                    self.prev_scroll = current_scroll;

                    ui.vertical(|ui| {
                        ui.style_mut().spacing.item_spacing = egui::vec2(0.0, 0.0);
                        for i in 0..128 {
                            let horiz = ui.horizontal(|ui| {
                                for j in 0..16 {
                                    ui.add(
                                        egui::Image::new(
                                            self.textures[i * 16 + j].as_ref().unwrap(),
                                        )
                                        .fit_to_exact_size(egui::vec2(tile_width, tile_width)),
                                    );
                                }
                            });
                            if let Some(new_base_addr) = new_base_addr {
                                if new_base_addr / 0x200 == i {
                                    println!("scrolling to {i}");
                                    self.top_scroll_row = i;
                                    horiz.response.scroll_to_me(Some(egui::Align::TOP));
                                }
                            } else if current_scroll.round() as usize == i {
                                self.top_scroll_row = i;
                            }
                        }
                    });
                });
            });
    }
}
