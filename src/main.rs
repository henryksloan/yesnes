#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![feature(generators, generator_trait)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(never_type)]

mod bus;
mod cpu;
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
        initial_window_size: Some(egui::vec2(320.0, 240.0)),
        ..Default::default()
    };
    eframe::run_native(
        "My egui App",
        options,
        Box::new(|_cc| Box::<MyApp>::default()),
    )
}

struct MyApp {
    name: String,
    age: u32,
    snes: SNES,
    scroll_to_row: Option<usize>,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            name: "Arthur".to_owned(),
            age: 42,
            snes: SNES::new(),
            scroll_to_row: None,
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("My egui Application");
            ui.horizontal(|ui| {
                let name_label = ui.label("Your name: ");
                ui.text_edit_singleline(&mut self.name)
                    .labelled_by(name_label.id);
            });
            ui.add(egui::Slider::new(&mut self.age, 0..=120).text("age"));
            let cpu_pc = self.snes.cpu.borrow().registers().pc;
            if ui.button("Go to PC").clicked() {
                self.age += 1;
                self.scroll_to_row = Some(cpu_pc.raw());
            }
            if ui.button("Trace").clicked() {
                self.snes.run_instruction();
            }
            ui.label(format!("Hello '{}', age {}", self.name, self.age));
            let text_height = egui::TextStyle::Body.resolve(ui.style()).size;
            let mut table = TableBuilder::new(ui)
                .striped(true)
                // .resizable(self.resizable)
                .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                .column(Column::auto())
                .column(Column::initial(100.0).range(40.0..=300.0))
                .column(Column::initial(100.0).at_least(40.0).clip(true))
                .column(Column::remainder())
                .min_scrolled_height(0.0);
            if let Some(row_nr) = self.scroll_to_row.take() {
                table = table.scroll_to_row(row_nr, None);
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
