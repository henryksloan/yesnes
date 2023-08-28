use super::app_window::AppWindow;
use super::debugger_window::DebuggerWindow;
use super::emu_thread::run_emu_thread;

use crate::bus::Bus;
use crate::disassembler::Disassembler;
use crate::snes::SNES;
use crate::u24::u24;

use crossbeam::channel;
use eframe::egui;
use egui_extras::{Column, TableBuilder};

use std::sync::{Arc, Mutex};

struct YesnesApp {
    snes: Arc<Mutex<SNES>>,
    emu_paused: Arc<Mutex<bool>>,
    // CPU memory viewer fields
    // TODO: Consider a smarter scheme than occasionally copying the whole memory
    memory_mirror: Vec<u8>,
    memory_stale: bool,
    active_window_id: Option<egui::Id>,
    cpu_debugger_window: DebuggerWindow,
}

impl Default for YesnesApp {
    fn default() -> Self {
        let snes = Arc::new(Mutex::new(SNES::new()));
        let disassembler = Arc::new(Mutex::new(Disassembler::new(
            snes.lock().unwrap().bus.clone(),
        )));
        disassembler.lock().unwrap().disassemble();
        let (sender, receiver) = channel::bounded(1024);
        let emu_paused = Arc::new(Mutex::new(true));
        run_emu_thread(
            snes.clone(),
            disassembler.clone(),
            receiver,
            emu_paused.clone(),
        );
        let disassembler_window = DebuggerWindow::new(
            "CPU Debugger".to_string(),
            snes.clone(),
            disassembler,
            emu_paused.clone(),
            sender,
        );
        Self {
            snes: snes.clone(),
            emu_paused: emu_paused.clone(),
            memory_mirror: vec![0; 0x100_0000],
            memory_stale: true,
            // TODO: Once I factor out the different windows to structs, get the window ID from there
            active_window_id: None,
            cpu_debugger_window: disassembler_window,
        }
    }
}

impl YesnesApp {
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

impl eframe::App for YesnesApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // TODO: When does `update` get called? We might not immediately get an `update` when paused changes.
        let paused = *self.emu_paused.lock().unwrap();
        // TODO: It might be reasonable to handle mutex poisoning here (e.g. other thread panics)
        egui::CentralPanel::default().show(ctx, |_ui| {});

        // TODO: Initially focus the CPU debugger
        self.cpu_debugger_window
            .show(ctx, paused, self.active_window_id);

        egui::Window::new("CPU Memory Viewer")
            .default_width(480.0)
            .default_height(640.0)
            .show(ctx, |ui| {
                if Some(egui::Id::new("CPU Memory Viewer")) != self.active_window_id {
                    ui.set_enabled(false);
                }
                self.memory_viewer_table(ui, paused);
            });

        ctx.memory_mut(|memory| {
            // Set the active window ID to the topmost (last-in-order) Area in the Middle order class
            self.active_window_id = memory
                .layer_ids()
                .into_iter()
                .filter(|layer_id| layer_id.order == egui::Order::Middle)
                .map(|layer_id| layer_id.id)
                .last();
        });
    }
}

pub fn run() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        initial_window_size: Some(egui::vec2(1260.0, 1000.0)),
        ..Default::default()
    };
    eframe::run_native(
        "yesnes",
        options,
        Box::new(|_cc| Box::<YesnesApp>::default()),
    )
}
