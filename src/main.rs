#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![feature(generators, generator_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]

mod bus;
mod cpu;
mod disassembler;
mod memory;
mod ppu;
mod scheduler;
mod smp;
mod snes;
mod u24;

use disassembler::Disassembler;
use eframe::egui;
use egui_extras::{Column, TableBuilder};
use snes::SNES;

const GO_TO_ADDRESS_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::COMMAND, egui::Key::G);
const RUN_TO_ADDRESS_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::COMMAND, egui::Key::R);

fn main() -> Result<(), eframe::Error> {
    env_logger::init();
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

impl eframe::emath::Numeric for u24::u24 {
    const INTEGRAL: bool = true;
    const MIN: Self = u24::u24(0);
    const MAX: Self = u24::u24(0xFF_FFFF);

    fn to_f64(self) -> f64 {
        self.0 as f64
    }

    fn from_f64(num: f64) -> Self {
        Self(num as u32)
    }
}

impl eframe::emath::Numeric for cpu::registers::StatusRegister {
    const INTEGRAL: bool = true;
    const MIN: Self = cpu::registers::StatusRegister::new(0x00);
    const MAX: Self = cpu::registers::StatusRegister::new(0xFF);

    fn to_f64(self) -> f64 {
        (self.get() as u32 | ((self.e as u32) << 8)) as f64
    }

    fn from_f64(num: f64) -> Self {
        let mut new_register = Self::default();
        new_register.set(num as u32 as u8);
        new_register.e = ((num as u32) >> 8) & 1 == 1;
        new_register
    }
}

/// Add an editable view of the given CPU register, masked to its bottom `hex_digits` nybbles
fn register_drag_value<T: eframe::emath::Numeric>(
    ui: &mut egui::Ui,
    register: &mut T,
    prefix: &str,
    hex_digits: usize,
) {
    let mask = (1 << (hex_digits * 4)) - 1;
    let mut reg_ui_val = (register.to_f64() as usize) & mask;
    ui.add(
        egui::DragValue::new(&mut reg_ui_val)
            .prefix(prefix)
            .speed(0)
            .clamp_range(0..=mask)
            .hexadecimal(hex_digits, false, true)
            .custom_parser(|s| {
                u32::from_str_radix(s, 16)
                    .map(|n| (n as usize & mask) as f64)
                    .ok()
            }),
    );
    let mut reg_val = register.to_f64() as usize;
    reg_val &= !mask;
    reg_val |= reg_ui_val;
    *register = T::from_f64(reg_val as f64);
}

/// Adds editable view of all CPU registers
fn registers_panel(ui: &mut egui::Ui, registers: &mut cpu::registers::Registers) {
    let x_y_16_bits = registers.index_reg_16_bits();
    let x_y_width = if x_y_16_bits { 4 } else { 2 };
    let a_16_bits = registers.accumulator_16_bits();
    let a_width = if a_16_bits { 4 } else { 2 };
    let sp_16_bits = registers.stack_pointer_16_bits();
    let sp_width = if sp_16_bits { 4 } else { 2 };
    register_drag_value(ui, &mut registers.a, "A=", a_width);
    register_drag_value(ui, &mut registers.x, "X=", x_y_width);
    register_drag_value(ui, &mut registers.y, "Y=", x_y_width);
    register_drag_value(ui, &mut registers.pc, "PC=", 6);
    register_drag_value(ui, &mut registers.sp, "SP=", sp_width);
    register_drag_value(ui, &mut registers.p, "P=", 2);
    register_drag_value(ui, &mut registers.d, "D=", 4);
    register_drag_value(ui, &mut registers.b, "B=", 2);
}

/// Add checkboxes for the bits of the status register
fn status_register_panel(ui: &mut egui::Ui, status_register: &mut cpu::registers::StatusRegister) {
    ui.horizontal(|ui| {
        ui.vertical(|ui| {
            ui.checkbox(&mut status_register.n, "N");
            ui.checkbox(&mut status_register.v, "V");
            ui.checkbox(&mut status_register.m, "M");
            ui.checkbox(&mut status_register.x_or_b, "X");
            ui.checkbox(&mut status_register.d, "D");
            ui.checkbox(&mut status_register.i, "I");
            ui.checkbox(&mut status_register.z, "Z");
            ui.checkbox(&mut status_register.c, "C");
        });
        ui.vertical(|ui| {
            ui.checkbox(&mut status_register.e, "E");
        });
    });
}

struct LineInputWindow {
    title: String,
    open: bool,
    text: String,
    request_focus_once: bool,
}

impl LineInputWindow {
    pub fn new(title: String) -> Self {
        Self {
            title,
            open: false,
            text: String::new(),
            request_focus_once: false,
        }
    }

    // If the window is closed, open it and clear+focus the textbox.
    pub fn open(&mut self) {
        self.text.clear();
        self.open = true;
        self.request_focus_once = true;
    }

    // If open, show the input window, calling the callback with the
    // input text if it was just submitted.
    pub fn show<F>(&mut self, ctx: &egui::Context, callback: F)
    where
        F: FnOnce(&str),
    {
        let mut submitted = false;
        egui::Window::new(&self.title)
            .open(&mut self.open)
            .resizable(false)
            .collapsible(false)
            .show(ctx, |ui| {
                let text_edit = ui.add(egui::TextEdit::singleline(&mut self.text));
                if text_edit.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                    submitted = true;
                    callback(&self.text);
                }
                if std::mem::take(&mut self.request_focus_once) {
                    text_edit.request_focus();
                }
            });
        if submitted {
            self.open = false;
        }
    }
}

struct YesnesApp {
    snes: SNES,
    disassembler: Disassembler,
    scroll_to_row: Option<(usize, egui::Align)>,
    prev_top_row: Option<usize>,
    prev_bottom_row: Option<usize>,
    go_to_address_window: LineInputWindow,
    run_to_address_window: LineInputWindow,
}

impl Default for YesnesApp {
    fn default() -> Self {
        let snes = SNES::new();
        let mut disassembler = Disassembler::new(snes.bus.clone());
        disassembler.disassemble();
        let mut app = Self {
            snes,
            disassembler,
            scroll_to_row: None,
            prev_top_row: None,
            prev_bottom_row: None,
            go_to_address_window: LineInputWindow::new("Go to address".to_string()),
            run_to_address_window: LineInputWindow::new("Run to address".to_string()),
        };
        app.scroll_pc_near_top();
        app
    }
}

impl YesnesApp {
    fn scroll_pc_near_top(&mut self) {
        let new_pc = self.snes.cpu.borrow_mut().registers().pc;
        let cpu_pc_line = self.disassembler.get_line_index(new_pc).saturating_sub(1);
        self.scroll_to_row = Some((cpu_pc_line, egui::Align::TOP));
    }
}

impl eframe::App for YesnesApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("Run", |ui| {
                    let address_shortcut_pressed =
                        ctx.input_mut(|i| i.consume_shortcut(&RUN_TO_ADDRESS_SHORTCUT));
                    let address_button = egui::Button::new("To address...")
                        .shortcut_text(ui.ctx().format_shortcut(&RUN_TO_ADDRESS_SHORTCUT));
                    if ui.add(address_button).clicked() || address_shortcut_pressed {
                        self.run_to_address_window.open();
                        ui.close_menu();
                    }
                });
                ui.menu_button("Search", |ui| {
                    ui.menu_button("Go to", |ui| {
                        let address_shortcut_pressed =
                            ctx.input_mut(|i| i.consume_shortcut(&GO_TO_ADDRESS_SHORTCUT));
                        let address_button = egui::Button::new("Address...")
                            .shortcut_text(ui.ctx().format_shortcut(&GO_TO_ADDRESS_SHORTCUT));
                        if ui.add(address_button).clicked() || address_shortcut_pressed {
                            self.go_to_address_window.open();
                            ui.close_menu();
                        }
                        if ui.button("Program Counter").clicked() {
                            ui.close_menu();
                        }
                    });
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    egui::Frame::group(ui.style())
                        .outer_margin(egui::Margin {
                            right: 4.0,
                            bottom: 6.0,
                            ..Default::default()
                        })
                        .show(ui, |ui| {
                            registers_panel(ui, self.snes.cpu.borrow_mut().registers());
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
                            status_register_panel(ui, &mut self.snes.cpu.borrow_mut().registers().p)
                        });
                });
            });
            let prev_top_row = self.prev_top_row.take();
            let prev_bottom_row = self.prev_bottom_row.take();
            let cpu_pc = self.snes.cpu.borrow_mut().registers().pc;
            ui.horizontal(|ui| {
                if ui.button("Go to PC").clicked() {
                    let cpu_pc_line = self.disassembler.get_line_index(cpu_pc);
                    self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                }
                if ui.button("Trace").clicked() {
                    self.snes.run_instruction();
                    let cpu_pc_line = self.disassembler.get_line_index(cpu_pc);
                    if let Some(prev_top_row) = prev_top_row {
                        if let Some(prev_bottom_row) = prev_bottom_row {
                            if cpu_pc_line < (prev_top_row + 2) {
                                self.scroll_to_row = Some((cpu_pc_line, egui::Align::Center));
                            } else if cpu_pc_line >= (prev_bottom_row - 2) {
                                let new_bottom_line = cpu_pc_line + 2;
                                self.scroll_to_row = Some((new_bottom_line, egui::Align::BOTTOM));
                            }
                        }
                    }
                }
                if ui.button("Reset").clicked() {
                    self.snes.reset();
                    self.scroll_pc_near_top();
                }
            });

            if ctx.input_mut(|i| i.consume_shortcut(&GO_TO_ADDRESS_SHORTCUT)) {
                self.go_to_address_window.open();
            }
            self.go_to_address_window.show(ctx, |text| {
                let lower_input = text.to_lowercase();
                let trimmed_input = lower_input.trim().trim_start_matches("0x");
                let parsed_addr = u32::from_str_radix(trimmed_input, 16);
                if let Ok(addr) = parsed_addr {
                    let addr_line = self.disassembler.get_line_index(u24::u24(addr));
                    self.scroll_to_row = Some((addr_line, egui::Align::Center));
                }
            });

            if ctx.input_mut(|i| i.consume_shortcut(&RUN_TO_ADDRESS_SHORTCUT)) {
                self.run_to_address_window.open();
            }
            self.run_to_address_window.show(ctx, |text| {
                let lower_input = text.to_lowercase();
                let trimmed_input = lower_input.trim().trim_start_matches("0x");
                let parsed_addr = u32::from_str_radix(trimmed_input, 16);
                if let Ok(addr) = parsed_addr {
                    while self.snes.cpu.borrow_mut().registers().pc != u24::u24(addr) {
                        self.snes.run_instruction();
                    }
                }
            });

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
                    body.rows(
                        text_height,
                        self.disassembler.get_num_lines(),
                        |row_index, mut row| {
                            if self.prev_top_row.is_none() {
                                self.prev_top_row = Some(row_index);
                            }
                            self.prev_bottom_row = Some(row_index);
                            let disassembly_line = self.disassembler.get_line(row_index);
                            let row_addr = disassembly_line.addr;
                            row.col(|ui| {
                                if row_addr == cpu_pc {
                                    ui.style_mut().visuals.override_text_color =
                                        Some(egui::Color32::KHAKI);
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
                        },
                    );
                });
        });
    }
}
