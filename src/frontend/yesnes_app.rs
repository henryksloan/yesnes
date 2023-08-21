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

enum EmuThreadMessage {
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
                        snes.run_instruction();
                        let cpu = snes.cpu.borrow();
                        let registers = cpu.registers();
                        // TODO: This should probably be called automatically. It might be best if it
                        // didn't require `impl Send for Disassembler`, i.e. if Disassembler didn't have an
                        // Rc<RefCell<Bus>>; maybe that could be passed straight from cpu at the end of each instruction.
                        disassembler
                            .lock()
                            .unwrap()
                            .update_disassembly_at(registers.pc, &registers.p);
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
        if let Ok(snes) = self.snes.try_lock() {
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
            Shortcut::RunToAddress => {
                self.run_to_address_window.open();
            }
            Shortcut::GoToAddress => {
                self.go_to_address_window.open();
            }
        }
    }

    fn menu_button_with_shortcut(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Shortcut,
        text: impl Into<egui::WidgetText>,
    ) {
        let button = egui::Button::new(text)
            .shortcut_text(ui.ctx().format_shortcut(&shortcut.to_egui_shortcut()));
        if ui.add(button).clicked() {
            self.handle_shortcut(shortcut);
            ui.close_menu();
        }
    }

    fn menu_bar(&mut self, ctx: &egui::Context, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            // Consume all shortcuts. This must be done in an unconditionally visible UI element so shortcuts always work.
            for shortcut in ALL_SHORTCUTS {
                if ctx.input_mut(|i| i.consume_shortcut(&shortcut.to_egui_shortcut())) {
                    self.handle_shortcut(shortcut);
                }
            }
            ui.menu_button("Run", |ui| {
                self.menu_button_with_shortcut(ui, Shortcut::RunToAddress, "To address...");
            });
            ui.menu_button("Search", |ui| {
                ui.menu_button("Go to", |ui| {
                    self.menu_button_with_shortcut(ui, Shortcut::GoToAddress, "Address...");
                    if ui.button("Program Counter").clicked() {
                        ui.close_menu();
                    }
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
                        // DO NOT SUBMIT: We don't necessarily want to be locking all throughout these functions.
                        // And certainly don't want to return if they're unavailable. Once I have an emulation thread:
                        // 1) Controls like "trace" and "run to address" should first pause emulation.
                        // 2) Register controls/views should refer to a frontend copy which is frozen (with
                        //    controls disabled) while emulating, and forwards changes to the emulator when paused.
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
        let prev_top_row = self.prev_top_row.take();
        let prev_bottom_row = self.prev_bottom_row.take();
        ui.horizontal(|ui| {
            if ui.button("Go to PC").clicked() {
                if let Ok(snes) = self.snes.try_lock() {
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                }
            }
            if ui.button("Trace").clicked() {
                *self.emu_paused.lock().unwrap() = true;
                if let Ok(mut snes) = self.snes.try_lock() {
                    snes.run_instruction();
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    self.disassembler
                        .lock()
                        .unwrap()
                        .update_disassembly_at(cpu_pc, &snes.cpu.borrow().registers().p);
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    if let Some(prev_top_row) = prev_top_row {
                        if let Some(prev_bottom_row) = prev_bottom_row {
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
            if ui.button("Reset").clicked() {
                if let Ok(mut snes) = self.snes.try_lock() {
                    snes.reset();
                }
                self.scroll_pc_near_top();
            }
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
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::RunToAddress(u24(addr)));
            }
        });
    }

    fn disassembly_row(
        disassembler: &Disassembler,
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
            if row_addr == registers_mirror.pc {
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

    fn disassembly_table(&mut self, ui: &mut egui::Ui) {
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
        self.show_windows(ctx);
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| self.menu_bar(ctx, ui));
        egui::CentralPanel::default().show(ctx, |ui| {
            let paused = *self.emu_paused.lock().unwrap();
            self.register_area(ui, paused);
            self.control_area(ui);
            self.disassembly_table(ui);
        });
    }
}

pub fn run() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        initial_window_size: Some(egui::vec2(860.0, 620.0)),
        ..Default::default()
    };
    eframe::run_native(
        "yesnes",
        options,
        Box::new(|_cc| Box::<YesnesApp>::default()),
    )
}
