mod shortcuts;

use shortcuts::*;

use super::app_window::{AppWindow, ShortcutWindow};
use super::line_input_window::*;

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
    scroll_to_row: Option<(usize, egui::Align)>,
    go_to_address_window: LineInputWindow,
}

impl MemoryViewWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>) -> Self {
        let id = egui::Id::new(&title);
        Self {
            id,
            title,
            snes,
            memory_mirror: vec![0; 0x100_0000],
            memory_stale: true,
            scroll_to_row: None,
            go_to_address_window: LineInputWindow::new("Go to address".to_string(), Some(id)),
        }
    }

    fn refresh_stale_memory(&mut self, paused: bool) {
        // TODO: A more dynamic mechanism for refreshing,
        // e.g. events (reset, new frame, etc.) rather than pausing and unpausing
        self.memory_stale |= !paused;
        if self.memory_stale && paused {
            self.memory_stale = false;
            let bus = &self.snes.lock().unwrap().bus;
            // TODO: This is very inefficient
            // DO NOT SUBMIT: Commenting this to track down deadlocks
            // for i in 0..0x100_0000 {
            //     self.memory_mirror[i] = Bus::peak_u8(bus.clone(), u24(i as u32));
            // }
        }
    }

    fn menu_bar(&mut self, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            use MemoryViewShortcut::*;
            ui.menu_button("Search", |ui| {
                ui.menu_button("Go to", |ui| {
                    self.menu_button_with_shortcut(ui, GoToAddress, "Address...");
                });
            });
        });
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
        if let Some((row_nr, align)) = self.scroll_to_row.take() {
            table = table.scroll_to_row(row_nr, Some(align));
        }
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

    fn show_windows(&mut self, ctx: &egui::Context) {
        self.go_to_address_window.show(ctx, |text| {
            let lower_input = text.to_lowercase();
            let trimmed_input = lower_input.trim().trim_start_matches("0x");
            let parsed_addr = u32::from_str_radix(trimmed_input, 16);
            if let Ok(addr) = parsed_addr {
                let addr_line = addr >> 4;
                self.scroll_to_row = Some((addr_line as usize, egui::Align::TOP));
            }
        });
    }
}

impl AppWindow for MemoryViewWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        self.show_windows(ctx);

        egui::Window::new(&self.title)
            .default_height(640.0)
            .show(ctx, |ui| {
                if !focused {
                    ui.set_enabled(false);
                }
                egui::TopBottomPanel::top(self.id.with("menu_bar"))
                    .show_inside(ui, |ui| self.menu_bar(ui));
                self.memory_viewer_table(ui, paused);
            });
    }
}

impl ShortcutWindow for MemoryViewWindow {
    type Shortcut = MemoryViewShortcut;

    const WINDOW_SHORTCUTS: &'static [Self::Shortcut] = MEMORY_VIEW_SHORTCUTS;

    fn handle_shortcut(&mut self, shortcut: &Self::Shortcut) {
        match shortcut {
            Self::Shortcut::GoToAddress => {
                self.go_to_address_window.open();
            }
        }
    }

    fn shortcut_enabled(&mut self, _shortcut: &Self::Shortcut) -> bool {
        true
    }
}
