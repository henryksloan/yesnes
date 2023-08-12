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

use bus::Bus;
use eframe::egui;
use egui_extras::{Column, TableBuilder};
use snes::SNES;

fn main() -> Result<(), eframe::Error> {
    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions {
        initial_window_size: Some(egui::vec2(860.0, 620.0)),
        ..Default::default()
    };
    eframe::run_native("yesnes", options, Box::new(|_cc| Box::<MyApp>::default()))
}

struct MyApp {
    snes: SNES,
    scroll_to_row: Option<usize>,
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
        self.get() as f64
    }

    fn from_f64(num: f64) -> Self {
        let mut new_register = Self::default();
        new_register.set(num as u8);
        new_register
    }
}

fn register_drag_value<T: eframe::emath::Numeric>(
    ui: &mut egui::Ui,
    register: &mut T,
    prefix: &str,
    min_width: usize,
) {
    ui.add(
        egui::DragValue::new(register)
            .prefix(prefix)
            .speed(0)
            .clamp_range(0..=((T::MAX).to_f64() as usize))
            .hexadecimal(min_width, false, true),
    );
}

fn registers_panel(ui: &mut egui::Ui, registers: &mut cpu::registers::Registers) {
    // DO NOT SUBMIT: The min width should depend on the M, X and E flags
    register_drag_value(ui, &mut registers.a, "A=", 4);
    register_drag_value(ui, &mut registers.x, "X=", 4);
    register_drag_value(ui, &mut registers.y, "Y=", 4);
    register_drag_value(ui, &mut registers.pc, "PC=", 6);
    register_drag_value(ui, &mut registers.sp, "SP=", 4);
    register_drag_value(ui, &mut registers.p, "P=", 2);
    // DO NOT SUBMIT: Figure out if D is supposed to be 8 bits; if so, change this
    register_drag_value(ui, &mut registers.d, "D=", 4);
    register_drag_value(ui, &mut registers.b, "B=", 2);
}

fn status_register_panel(ui: &mut egui::Ui, status_register: &mut cpu::registers::StatusRegister) {
    ui.checkbox(&mut status_register.n, "N");
    ui.checkbox(&mut status_register.v, "V");
    ui.checkbox(&mut status_register.m, "M");
    ui.checkbox(&mut status_register.x_or_b, "X");
    ui.checkbox(&mut status_register.d, "D");
    ui.checkbox(&mut status_register.i, "I");
    ui.checkbox(&mut status_register.z, "B");
    ui.checkbox(&mut status_register.c, "C");
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            snes: SNES::new(),
            scroll_to_row: None,
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
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
                            ui.set_min_width(75.0);
                            registers_panel(ui, self.snes.cpu.borrow_mut().registers())
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
            let cpu_pc = self.snes.cpu.borrow_mut().registers().pc;
            ui.horizontal(|ui| {
                if ui.button("Go to PC").clicked() {
                    self.scroll_to_row = Some(cpu_pc.raw());
                }
                if ui.button("Trace").clicked() {
                    self.snes.run_instruction();
                }
            });
            let text_height = egui::TextStyle::Body.resolve(ui.style()).size;
            let mut table = TableBuilder::new(ui)
                .striped(true)
                .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                .column(Column::auto())
                .column(Column::initial(100.0).range(40.0..=300.0))
                .column(Column::initial(100.0).at_least(40.0).clip(true))
                .column(Column::remainder())
                .min_scrolled_height(0.0);
            if let Some(row_nr) = self.scroll_to_row.take() {
                table = table.scroll_to_row(row_nr, Some(egui::Align::Center));
            }
            table
                .header(20.0, |mut header| {
                    header.col(|ui| {
                        ui.strong("Row");
                    });
                    header.col(|ui| {
                        ui.strong("Expanding content");
                    });
                    header.col(|ui| {
                        ui.strong("Clipped text");
                    });
                    header.col(|ui| {
                        ui.strong("Content");
                    });
                })
                .body(|mut body| {
                    body.rows(text_height, 0xFF_FFFF, |row_index, mut row| {
                        let row_addr = u24::u24(row_index as u32);
                        row.col(|ui| {
                            if row_addr == cpu_pc {
                                ui.style_mut().visuals.override_text_color =
                                    Some(egui::Color32::KHAKI);
                            }
                            ui.label(format!("{:08X}", row_addr.raw()));
                        });
                        row.col(|ui| {
                            ui.label(format!(
                                "{:02X}",
                                Bus::peak_u8(self.snes.bus.clone(), row_addr)
                            ));
                        });
                        row.col(|ui| {
                            ui.label(row_index.to_string());
                        });
                        row.col(|ui| {
                            ui.add(
                                egui::Label::new("Thousands of rows of even height").wrap(false),
                            );
                        });
                    });
                });
        });
    }
}
