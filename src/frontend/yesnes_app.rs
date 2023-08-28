use super::app_window::{AppWindow, ShortcutWindow};
use super::debugger_window::DebuggerWindow;
use super::emu_thread::run_emu_thread;
use super::memory_view_window::MemoryViewWindow;

use crate::disassembler::Disassembler;
use crate::snes::SNES;

use crossbeam::channel;
use eframe::egui;

use std::sync::{Arc, Mutex};

struct YesnesApp {
    emu_paused: Arc<Mutex<bool>>,
    active_window_id: Option<egui::Id>,
    cpu_debugger_window: DebuggerWindow,
    memory_view_window: MemoryViewWindow,
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
        let cpu_debugger_window = DebuggerWindow::new(
            "CPU Debugger".to_string(),
            snes.clone(),
            disassembler,
            emu_paused.clone(),
            sender,
        );
        let memory_view_window =
            MemoryViewWindow::new("CPU Memory Viewer".to_string(), snes.clone());
        Self {
            emu_paused: emu_paused.clone(),
            // TODO: Once I factor out the different windows to structs, get the window ID from there
            active_window_id: None,
            cpu_debugger_window,
            memory_view_window,
        }
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
            .show_with_shortcuts(ctx, paused, self.active_window_id);
        self.memory_view_window
            .show(ctx, paused, self.active_window_id);

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
