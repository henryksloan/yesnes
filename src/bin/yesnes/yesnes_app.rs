use super::app_window::{AppWindow, ShortcutWindow};
use super::debugger_window::DebuggerWindow;
use super::emu_thread::run_emu_thread;
use super::memory_view_window::MemoryViewWindow;
use super::screen_window::ScreenWindow;
use super::tile_view_window::TileViewWindow;

use yesnes::disassembler::{DebugCpu, DebugSmp, Disassembler};
use yesnes::frame_history::FrameHistory;
use yesnes::snes::SNES;

use crossbeam::channel;
use eframe::egui;

use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

struct YesnesApp {
    emu_paused: Arc<AtomicBool>,
    active_window_id: Option<egui::Id>,
    // TODO: Maybe own e.g. a DebugCPU here and use Rc in e.g. Disassembler... actually,
    // it would make sense to have them behind mutexes! Right now the only mutual exclusion is disabling fields :|
    cpu_debugger_window: DebuggerWindow<DebugCpu>,
    smp_debugger_window: DebuggerWindow<DebugSmp>,
    cpu_memory_view_window: MemoryViewWindow<DebugCpu>,
    smp_memory_view_window: MemoryViewWindow<DebugSmp>,
    tile_view_window: TileViewWindow,
    screen_window: ScreenWindow,
    frame_history: FrameHistory,
}

impl Default for YesnesApp {
    fn default() -> Self {
        let snes = Arc::new(Mutex::new(SNES::new()));
        let cart_path = std::env::args().nth(1).expect("Expected a rom file");
        snes.lock().unwrap().load_cart(&cart_path);
        snes.lock().unwrap().reset();
        let cpu_disassembler = Arc::new(Mutex::new(Disassembler::new(
            snes.lock().unwrap().make_debug_cpu(),
        )));
        cpu_disassembler.lock().unwrap().disassemble();
        let smp_disassembler = Arc::new(Mutex::new(Disassembler::new(
            snes.lock().unwrap().make_debug_smp(),
        )));
        smp_disassembler.lock().unwrap().disassemble();
        let (sender, receiver) = channel::bounded(1024);
        // TODO: This should be a smarter cancellation mechanism. As a start, could use an atomic bool.
        let emu_paused = Arc::new(AtomicBool::new(true));
        let frame_ready = Arc::new(AtomicBool::new(true));
        run_emu_thread(
            snes.clone(),
            cpu_disassembler.clone(),
            smp_disassembler.clone(),
            receiver,
            emu_paused.clone(),
            frame_ready.clone(),
        );
        let cpu_debugger_window = DebuggerWindow::new(
            "CPU Debugger".to_string(),
            snes.clone(),
            cpu_disassembler,
            emu_paused.clone(),
            sender.clone(),
        );
        let smp_debugger_window = DebuggerWindow::new(
            "SMP Debugger".to_string(),
            snes.clone(),
            smp_disassembler,
            emu_paused.clone(),
            sender.clone(),
        );
        let cpu_memory_view_window = MemoryViewWindow::new(
            "CPU Memory Viewer".to_string(),
            snes.lock().unwrap().make_debug_cpu(),
        );
        let smp_memory_view_window = MemoryViewWindow::new(
            "SMP Memory Viewer".to_string(),
            snes.lock().unwrap().make_debug_smp(),
        );
        let tile_view_window = TileViewWindow::new("Tile Viewer".to_string(), snes.clone());
        let screen_window =
            ScreenWindow::new("Screen".to_string(), snes.clone(), frame_ready.clone());
        Self {
            emu_paused: emu_paused.clone(),
            // TODO: Once I factor out the different windows to structs, get the window ID from there
            active_window_id: None,
            cpu_debugger_window,
            smp_debugger_window,
            cpu_memory_view_window,
            smp_memory_view_window,
            tile_view_window,
            screen_window,
            frame_history: FrameHistory::new(),
        }
    }
}

impl eframe::App for YesnesApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        self.frame_history
            .on_new_frame(ctx.input(|i| i.time), frame.info().cpu_usage);

        let paused = self.emu_paused.load(std::sync::atomic::Ordering::Acquire);
        // TODO: It might be reasonable to handle mutex poisoning here (e.g. other thread panics)
        // TODO: We also might want to handle panics from the emulator on the frontend thread (e.g. trace)
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.label(format!(
                "Mean frame time: {:.2}ms",
                1e3 * self.frame_history.mean_frame_time()
            ));
        });

        // TODO: Initially focus the CPU debugger
        self.cpu_debugger_window
            .show_with_shortcuts(ctx, paused, self.active_window_id);
        self.smp_debugger_window
            .show_with_shortcuts(ctx, paused, self.active_window_id);
        self.cpu_memory_view_window
            .show_with_shortcuts(ctx, paused, self.active_window_id);
        self.smp_memory_view_window
            .show_with_shortcuts(ctx, paused, self.active_window_id);
        self.tile_view_window
            .show(ctx, paused, self.active_window_id);
        self.screen_window.show(ctx, paused, self.active_window_id);

        ctx.memory_mut(|memory| {
            // Set the active window ID to the topmost (last-in-order) Area in the Middle order class
            self.active_window_id = memory
                .layer_ids()
                .into_iter()
                .filter(|layer_id| layer_id.order == egui::Order::Middle)
                .map(|layer_id| layer_id.id)
                .last();
        });

        // Effectively puts egui in "continuous mode", where we repaint as quickly as possible (up to refresh rate)
        ctx.request_repaint();
    }
}

pub fn run() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1600.0, 1000.0]),
        ..Default::default()
    };
    eframe::run_native(
        "yesnes",
        options,
        Box::new(|_cc| Ok(Box::<YesnesApp>::default())),
    )
}
