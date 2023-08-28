mod shortcuts;

use shortcuts::{DisassemblerShortcut, DISASSEMBLER_SHORTCUTS};

use super::app_window::{AppWindow, ShortcutWindow};
use super::emu_thread::{run_instruction_and_disassemble, EmuThreadMessage};
use super::line_input_window::*;
use super::registers::*;

use std::sync::{Arc, Mutex};

use crossbeam::channel;
use egui_extras::{Column, TableBuilder};

use crate::cpu::registers::Registers;
use crate::disassembler::Disassembler;
use crate::snes::SNES;
use crate::u24::u24;

pub struct DebuggerWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    disassembler: Arc<Mutex<Disassembler>>,
    emu_paused: Arc<Mutex<bool>>,
    registers_mirror: Registers,
    scroll_to_row: Option<(usize, egui::Align)>,
    prev_top_row: Option<usize>,
    prev_bottom_row: Option<usize>,
    go_to_address_window: LineInputWindow,
    run_to_address_window: LineInputWindow,
    emu_message_sender: channel::Sender<EmuThreadMessage>,
}

impl DebuggerWindow {
    pub fn new(
        title: String,
        snes: Arc<Mutex<SNES>>,
        disassembler: Arc<Mutex<Disassembler>>,
        emu_paused: Arc<Mutex<bool>>,
        emu_message_sender: channel::Sender<EmuThreadMessage>,
    ) -> Self {
        let id = egui::Id::new(&title);
        let mut window = Self {
            id,
            title,
            snes,
            disassembler,
            emu_paused,
            registers_mirror: Registers::new(),
            scroll_to_row: None,
            prev_top_row: None,
            prev_bottom_row: None,
            go_to_address_window: LineInputWindow::new("Go to address".to_string(), Some(id)),
            run_to_address_window: LineInputWindow::new("Run to address".to_string(), Some(id)),
            emu_message_sender,
        };
        window.scroll_pc_near_top();
        window
    }

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

    fn menu_bar(&mut self, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            use DisassemblerShortcut::*;
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
            self.button_with_shortcut(ui, DisassemblerShortcut::Continue, "Continue");
            self.button_with_shortcut(ui, DisassemblerShortcut::Pause, "Pause");
            self.button_with_shortcut(ui, DisassemblerShortcut::Trace, "Trace");
            self.button_with_shortcut(ui, DisassemblerShortcut::Reset, "Reset");
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
            .column(Column::exact(60.0))
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

impl AppWindow for DebuggerWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        self.show_windows(ctx);

        egui::Window::new(&self.title)
            .default_width(480.0)
            .default_height(640.0)
            .show(ctx, |ui| {
                if !focused {
                    ui.set_enabled(false);
                }
                egui::TopBottomPanel::top(self.id.with("menu_bar"))
                    .show_inside(ui, |ui| self.menu_bar(ui));
                self.register_area(ui, paused);
                self.control_area(ui);
                self.disassembly_table(ui, paused);
            });
    }
}

impl ShortcutWindow for DebuggerWindow {
    type Shortcut = DisassemblerShortcut;

    const WINDOW_SHORTCUTS: &'static [Self::Shortcut] = DISASSEMBLER_SHORTCUTS;

    fn handle_shortcut(&mut self, shortcut: &Self::Shortcut) {
        match shortcut {
            Self::Shortcut::GoToAddress => {
                self.go_to_address_window.open();
            }
            Self::Shortcut::GoToProgramCounter => {
                if *self.emu_paused.lock().unwrap() {
                    let snes = self.snes.lock().unwrap();
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                }
            }
            Self::Shortcut::Continue => {
                let _ = self.emu_message_sender.send(EmuThreadMessage::Continue);
            }
            Self::Shortcut::Pause => {
                *self.emu_paused.lock().unwrap() = true;
                if let Ok(snes) = self.snes.lock() {
                    let cpu_pc = snes.cpu.borrow().registers().pc;
                    let cpu_pc_line = self.disassembler.lock().unwrap().get_line_index(cpu_pc);
                    self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                }
            }
            Self::Shortcut::Trace => {
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
            Self::Shortcut::RunToAddress => {
                self.run_to_address_window.open();
            }
            Self::Shortcut::Reset => {
                *self.emu_paused.lock().unwrap() = true;
                if let Ok(mut snes) = self.snes.lock() {
                    snes.reset();
                }
                self.scroll_pc_near_top();
            }
        }
    }

    fn shortcut_enabled(&mut self, shortcut: &Self::Shortcut) -> bool {
        match *shortcut {
            Self::Shortcut::Continue => *self.emu_paused.lock().unwrap(),
            Self::Shortcut::Pause => !(*self.emu_paused.lock().unwrap()),
            _ => true,
        }
    }
}
