use super::keyboard_shortcuts::*;
use super::line_input_window::*;
use super::registers::*;

use eframe::egui;
use egui_extras::{Column, TableBuilder};

use crate::disassembler::Disassembler;
use crate::snes::SNES;
use crate::u24::u24;

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

    fn menu_bar(&mut self, ctx: &egui::Context, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            ui.menu_button("Run", |ui| {
                if button_with_shortcut(ctx, ui, "To address...", &RUN_TO_ADDRESS_SHORTCUT) {
                    self.run_to_address_window.open();
                    ui.close_menu();
                }
            });
            ui.menu_button("Search", |ui| {
                ui.menu_button("Go to", |ui| {
                    if button_with_shortcut(ctx, ui, "Address...", &GO_TO_ADDRESS_SHORTCUT) {
                        self.go_to_address_window.open();
                        ui.close_menu();
                    }
                    if ui.button("Program Counter").clicked() {
                        ui.close_menu();
                    }
                });
            });
        });
    }

    fn register_area(&mut self, ui: &mut egui::Ui) {
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
    }

    fn control_area(&mut self, ui: &mut egui::Ui) {
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
    }
}

impl eframe::App for YesnesApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| self.menu_bar(ctx, ui));

        egui::CentralPanel::default().show(ctx, |ui| {
            self.register_area(ui);
            self.control_area(ui);

            if ctx.input_mut(|i| i.consume_shortcut(&GO_TO_ADDRESS_SHORTCUT)) {
                self.go_to_address_window.open();
            }
            self.go_to_address_window.show(ctx, |text| {
                let lower_input = text.to_lowercase();
                let trimmed_input = lower_input.trim().trim_start_matches("0x");
                let parsed_addr = u32::from_str_radix(trimmed_input, 16);
                if let Ok(addr) = parsed_addr {
                    let addr_line = self.disassembler.get_line_index(u24(addr));
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
                    while self.snes.cpu.borrow_mut().registers().pc != u24(addr) {
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
                                let cpu_pc = self.snes.cpu.borrow_mut().registers().pc;
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
