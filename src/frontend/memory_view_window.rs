use super::app_window::AppWindow;

use crate::bus::Bus;
use crate::snes::SNES;
use crate::u24::u24;

use egui_extras::{Column, TableBuilder};

use std::sync::{Arc, Mutex};

pub struct MemoryViewWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    // TODO: Consider a smarter scheme than occasionally copying the whole memory
    memory_mirror: Vec<u8>,
    memory_stale: bool,
}

impl MemoryViewWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>) -> Self {
        Self {
            id: egui::Id::new(&title),
            title,
            snes,
            memory_mirror: vec![0; 0x100_0000],
            memory_stale: true,
        }
    }

    fn refresh_stale_memory(&mut self, paused: bool) {
        // TODO: A more dynamic mechanism for refreshing,
        // e.g. events (reset, new frame, etc.) rather than pausing and unpausing
        if self.memory_stale && paused {
            self.memory_stale = false;
            let bus = &self.snes.lock().unwrap().bus;
            for i in 0..0x100_0000 {
                self.memory_mirror[i] = Bus::peak_u8(bus.clone(), u24(i as u32));
            }
        }
    }

    fn memory_viewer_table(&mut self, ui: &mut egui::Ui, paused: bool) {
        self.refresh_stale_memory(paused);
        let text_height = egui::TextStyle::Body.resolve(ui.style()).size;
        let mut table = TableBuilder::new(ui)
            .striped(true)
            .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
            .column(Column::exact(45.0))
            .columns(Column::exact(14.0), 0x10)
            .min_scrolled_height(0.0);
        table
            .header(20.0, |mut header| {
                header.col(|_ui| {});
                for low_nybble in 0x00..=0x0F {
                    // Left label column
                    header.col(|ui| {
                        ui.strong(format!("{low_nybble:02X}"));
                    });
                }
            })
            .body(|body| {
                body.rows(text_height, 0x10_0000, |row_index, mut row| {
                    let row_addr = row_index * 0x10;
                    row.col(|ui| {
                        ui.strong(format!("{:06X}", row_addr));
                    });
                    for low_nybble in 0x0..=0xF {
                        row.col(|ui| {
                            let data = self.memory_mirror[row_addr | low_nybble];
                            ui.label(format!("{:02X}", data));
                        });
                    }
                });
            });
    }
}

impl AppWindow for MemoryViewWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        egui::Window::new(&self.title)
            .default_width(480.0)
            .default_height(640.0)
            .show(ctx, |ui| {
                if !focused {
                    ui.set_enabled(false);
                }
                self.memory_viewer_table(ui, paused);
            });
    }
}
