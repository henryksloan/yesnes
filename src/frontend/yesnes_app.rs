use super::keyboard_shortcuts::*;
use super::line_input_window::*;
use super::registers::*;

use std::sync::{Arc, Mutex};
use std::thread;

use crossbeam::channel;
use eframe::egui;
use egui_extras::{Column, TableBuilder};

use crate::cpu::registers::Registers;
use crate::disassembler::Disassembler;
use crate::snes::SNES;
use crate::u24::u24;

// TODO: This should probably be done internally. It might be best if it
// didn't require `impl Send for Disassembler`, i.e. if Disassembler didn't have an
// Rc<RefCell<Bus>>; maybe that could be passed straight from cpu at the end of each instruction.
fn run_instruction_and_disassemble(snes: &mut SNES, disassembler: &Mutex<Disassembler>) -> bool {
    let breakpoint = snes.run_instruction_debug();
    let cpu = snes.cpu.borrow();
    let registers = cpu.registers();
    disassembler
        .lock()
        .unwrap()
        .update_disassembly_at(registers.pc, &registers.p);
    breakpoint
}

enum EmuThreadMessage {
    Continue,
    RunToAddress(u24),
}

fn run_emu_thread(
    snes: Arc<Mutex<SNES>>,
    disassembler: Arc<Mutex<Disassembler>>,
    receiver: channel::Receiver<EmuThreadMessage>,
    paused: Arc<Mutex<bool>>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || loop {
        let Ok(message) = receiver.recv() else {
            log::debug!("Exiting emulation thread");
            return;
        };
        match message {
            EmuThreadMessage::Continue => {
                *paused.lock().unwrap() = false;
                while !*paused.lock().unwrap() {
                    let should_break =
                        run_instruction_and_disassemble(&mut snes.lock().unwrap(), &disassembler);
                    if should_break {
                        *paused.lock().unwrap() = true;
                    }
                }
            }
            EmuThreadMessage::RunToAddress(addr) => {
                *paused.lock().unwrap() = false;
                loop {
                    if *paused.lock().unwrap() {
                        break;
                    }
                    let mut snes = snes.lock().unwrap();
                    if snes.cpu.borrow().registers().pc == addr {
                        *paused.lock().unwrap() = true;
                        break;
                    } else {
                        run_instruction_and_disassemble(&mut snes, &disassembler);
                    }
                }
            }
        }
    })
}

struct YesnesApp {
    snes: Arc<Mutex<SNES>>,
    disassembler: Arc<Mutex<Disassembler>>,
    // A copy of the CPU's registers, frozen while unpaused.
    registers_mirror: Registers,
    scroll_to_row: Option<(usize, egui::Align)>,
    prev_top_row: Option<usize>,
    prev_bottom_row: Option<usize>,
    go_to_address_window: LineInputWindow,
    run_to_address_window: LineInputWindow,
    emu_message_sender: channel::Sender<EmuThreadMessage>,
    emu_paused: Arc<Mutex<bool>>,
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
        let mut app = Self {
            snes: snes.clone(),
            disassembler,
            registers_mirror: Registers::new(),
            scroll_to_row: None,
            prev_top_row: None,
            prev_bottom_row: None,
            go_to_address_window: LineInputWindow::new("Go to address".to_string()),
            run_to_address_window: LineInputWindow::new("Run to address".to_string()),
            emu_message_sender: sender,
            emu_paused: emu_paused.clone(),
        };
        app.scroll_pc_near_top();
        app
    }
}

impl YesnesApp {
    fn scroll_pc_near_top(&mut self) {
        if let Ok(snes) = self.snes.lock() {
            let pc = snes.cpu.borrow().registers().pc;
            let cpu_pc_line = self
                .disassembler
                .lock()
                .unwrap()
                .get_line_index(pc)
                .saturating_sub(1);
            self.scroll_to_row = Some((cpu_pc_line, egui::Align::TOP));
        }
    }

    fn handle_shortcut(&mut self, shortcut: Shortcut) {
        match shortcut {
            Shortcut::GoToAddress => {
                self.go_to_address_window.open();
            }
            Shortcut::GoToProgramCounter => {
                if *self.emu_paused.lock().unwrap() {
                    let snes = self.snes.lock().unwrap();
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                }
            }
            Shortcut::Continue => {
                let _ = self.emu_message_sender.send(EmuThreadMessage::Continue);
            }
            Shortcut::Pause => {
                *self.emu_paused.lock().unwrap() = true;
                if let Ok(snes) = self.snes.lock() {
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                }
            }
            Shortcut::Trace => {
                *self.emu_paused.lock().unwrap() = true;
                if let Ok(mut snes) = self.snes.lock() {
                    run_instruction_and_disassemble(&mut snes, &self.disassembler);
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    if let Some(prev_top_row) = self.prev_top_row {
                        if let Some(prev_bottom_row) = self.prev_bottom_row {
                            if (cpu_pc_line < prev_top_row) || (cpu_pc_line > prev_bottom_row) {
                                // Recenter PC if it jumped off-screen
                                self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                            } else if cpu_pc_line >= (prev_bottom_row - 1) {
                                // Keep the PC just above the bottom of the disassembly output
                                let new_bottom_line = cpu_pc_line + 1;
                                self.scroll_to_row = Some((new_bottom_line, egui::Align::BOTTOM));
                            }
                        }
                    }
                }
            }
            Shortcut::RunToAddress => {
                self.run_to_address_window.open();
            }
            Shortcut::Reset => {
                *self.emu_paused.lock().unwrap() = true;
                if let Ok(mut snes) = self.snes.lock() {
                    snes.reset();
                }
                self.scroll_pc_near_top();
            }
        }
    }

    fn shortcut_enabled(&mut self, shortcut: Shortcut) -> bool {
        match shortcut {
            Shortcut::Continue => *self.emu_paused.lock().unwrap(),
            Shortcut::Pause => !(*self.emu_paused.lock().unwrap()),
            _ => true,
        }
    }

    fn button_with_shortcut_impl(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Shortcut,
        text: impl Into<egui::WidgetText>,
        is_menu_button: bool,
    ) {
        let enabled = self.shortcut_enabled(shortcut);
        let mut button = egui::Button::new(text);
        if is_menu_button {
            button = button.shortcut_text(ui.ctx().format_shortcut(&shortcut.to_egui_shortcut()));
        }
        if ui.add_enabled(enabled, button).clicked() {
            self.handle_shortcut(shortcut);
            if is_menu_button {
                ui.close_menu();
            }
        }
    }

    fn button_with_shortcut(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Shortcut,
        text: impl Into<egui::WidgetText>,
    ) {
        self.button_with_shortcut_impl(ui, shortcut, text, false);
    }

    fn menu_button_with_shortcut(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Shortcut,
        text: impl Into<egui::WidgetText>,
    ) {
        self.button_with_shortcut_impl(ui, shortcut, text, true);
    }

    fn menu_bar(&mut self, ctx: &egui::Context, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            // Consume all shortcuts. This must be done in an unconditionally visible UI element so shortcuts always work.
            for shortcut in ALL_SHORTCUTS {
                if ctx.input_mut(|i| i.consume_shortcut(&shortcut.to_egui_shortcut())) {
                    if self.shortcut_enabled(*shortcut) {
                        self.handle_shortcut(*shortcut);
                    }
                }
            }
            use Shortcut::*;
            ui.menu_button("Run", |ui| {
                self.menu_button_with_shortcut(ui, Continue, "Continue");
                self.menu_button_with_shortcut(ui, Pause, "Pause");
                self.menu_button_with_shortcut(ui, Trace, "Trace");
                self.menu_button_with_shortcut(ui, RunToAddress, "To address...");
                self.menu_button_with_shortcut(ui, Reset, "Reset");
            });
            ui.menu_button("Search", |ui| {
                ui.menu_button("Go to", |ui| {
                    self.menu_button_with_shortcut(ui, GoToAddress, "Address...");
                    self.menu_button_with_shortcut(ui, GoToProgramCounter, "Program Counter");
                });
            });
        });
    }

    fn register_area(&mut self, ui: &mut egui::Ui, paused: bool) {
        if paused {
            let snes = self.snes.lock().unwrap();
            self.registers_mirror = *snes.cpu.borrow().registers();
        }
        ui.horizontal(|ui| {
            ui.set_enabled(paused);
            ui.vertical(|ui| {
                egui::Frame::group(ui.style())
                    .outer_margin(egui::Margin {
                        right: 4.0,
                        bottom: 6.0,
                        ..Default::default()
                    })
                    .show(ui, |ui| {
                        registers_panel(ui, &mut self.registers_mirror);
                        ui.set_width(75.0);
                    });
            });
            ui.vertical(|ui| {
                egui::Frame::group(ui.style())
                    .outer_margin(egui::Margin {
                        bottom: 6.0,
                        ..Default::default()
                    })
                    .show(ui, |ui| {
                        status_register_panel(ui, &mut self.registers_mirror.p);
                    });
            });
        });
        if paused {
            let snes = self.snes.lock().unwrap();
            *snes.cpu.borrow_mut().registers_mut() = self.registers_mirror;
        }
    }

    fn control_area(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            self.button_with_shortcut(ui, Shortcut::Continue, "Continue");
            self.button_with_shortcut(ui, Shortcut::Pause, "Pause");
            self.button_with_shortcut(ui, Shortcut::Trace, "Trace");
            self.button_with_shortcut(ui, Shortcut::Reset, "Reset");
        });
    }

    fn show_windows(&mut self, ctx: &egui::Context) {
        self.go_to_address_window.show(ctx, |text| {
            let lower_input = text.to_lowercase();
            let trimmed_input = lower_input.trim().trim_start_matches("0x");
            let parsed_addr = u32::from_str_radix(trimmed_input, 16);
            if let Ok(addr) = parsed_addr {
                let addr_line = self.disassembler.lock().unwrap().get_line_index(u24(addr));
                self.scroll_to_row = Some((addr_line, egui::Align::Center));
            }
        });

        self.run_to_address_window.show(ctx, |text| {
            let lower_input = text.to_lowercase();
            let trimmed_input = lower_input.trim().trim_start_matches("0x");
            let parsed_addr = u32::from_str_radix(trimmed_input, 16);
            if let Ok(addr) = parsed_addr {
                // TODO: I think we should somehow recenter once we get to the right place.
                // Probably worth making that a message from emu thread to this thread.
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::RunToAddress(u24(addr)));
            }
        });
    }

    fn disassembly_row(
        disassembler: &Disassembler,
        paused: bool,
        registers_mirror: &Registers,
        prev_top_row: &mut Option<usize>,
        prev_bottom_row: &mut Option<usize>,
        row_index: usize,
        mut row: egui_extras::TableRow,
    ) {
        if prev_top_row.is_none() {
            *prev_top_row = Some(row_index);
        }
        *prev_bottom_row = Some(row_index);
        let disassembly_line = disassembler.get_line(row_index);
        let row_addr = disassembly_line.addr;
        row.col(|ui| {
            if paused && row_addr == registers_mirror.pc {
                ui.style_mut().visuals.override_text_color = Some(egui::Color32::KHAKI);
            }
            ui.label(format!("{:08X}", row_addr));
        });
        row.col(|ui| {
            ui.label(format!(
                "{} {:?}({:08X})",
                disassembly_line.instruction.instruction_data.mnemonic(),
                disassembly_line.instruction.instruction_data.mode,
                disassembly_line.instruction.operand,
            ));
        });
    }

    fn disassembly_table(&mut self, ui: &mut egui::Ui, paused: bool) {
        let total_lines = self.disassembler.lock().unwrap().get_num_lines();
        let text_height = egui::TextStyle::Body.resolve(ui.style()).size;
        let mut table = TableBuilder::new(ui)
            .striped(true)
            .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
            .column(Column::initial(60.0))
            .column(Column::remainder())
            .min_scrolled_height(0.0);
        if let Some((row_nr, align)) = self.scroll_to_row.take() {
            table = table.scroll_to_row(row_nr, Some(align));
        }
        self.prev_top_row = None;
        self.prev_bottom_row = None;
        table
            .header(20.0, |mut header| {
                header.col(|ui| {
                    ui.strong("Address");
                });
                header.col(|ui| {
                    ui.strong("Instruction");
                });
            })
            .body(|body| {
                let disassembler = self.disassembler.lock().unwrap();
                body.rows(text_height, total_lines, |row_index, row| {
                    Self::disassembly_row(
                        &disassembler,
                        paused,
                        &self.registers_mirror,
                        &mut self.prev_top_row,
                        &mut self.prev_bottom_row,
                        row_index,
                        row,
                    )
                });
            });
    }
}

impl eframe::App for YesnesApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // TODO: It might be reasonable to handle mutex poisoning here (e.g. other thread panics)
        egui::CentralPanel::default().show(ctx, |_ui| {});

        egui::Window::new("CPU Debugger")
            .default_width(480.0)
            .default_height(640.0)
            .show(ctx, |ui| {
                self.show_windows(ctx);
                egui::TopBottomPanel::top("menu_bar").show_inside(ui, |ui| self.menu_bar(ctx, ui));
                // TODO: When does `update` get called? We might not immediately get an `update` when paused changes.
                let paused = *self.emu_paused.lock().unwrap();
                self.register_area(ui, paused);
                self.control_area(ui);
                self.disassembly_table(ui, paused);
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
