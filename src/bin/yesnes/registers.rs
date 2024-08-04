//! GUI elements to display and edit processer registers.
use yesnes::debug_cpu;
use yesnes::debug_smp;
use yesnes::disassembler::{DebugCpu, DebugProcessor, DebugSmp};

// TODO: It's not actually great to not show some of the nybbles; maybe just gray them out?
/// Adds an editable view of the given CPU register, masked to its bottom `hex_digits` nybbles.
pub fn register_drag_value<T: eframe::emath::Numeric>(
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
            .range(0..=mask)
            .hexadecimal(hex_digits, false, true)
            .custom_parser(|s| {
                u32::from_str_radix(s, 16)
                    .map(|n| (n as usize & mask) as f64)
                    .ok()
            })
            .update_while_editing(false),
    );
    let mut reg_val = register.to_f64() as usize;
    reg_val &= !mask;
    reg_val |= reg_ui_val;
    *register = T::from_f64(reg_val as f64);
}

pub trait RegisterArea: DebugProcessor {
    fn registers_area(ui: &mut egui::Ui, registers: &mut <Self as DebugProcessor>::Registers);
}

/// Adds editable view of all CPU registers.
fn cpu_registers_panel(ui: &mut egui::Ui, registers: &mut debug_cpu::Registers) {
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

/// Adds checkboxes for the bits of the CPU status register.
fn cpu_status_register_panel(ui: &mut egui::Ui, status_register: &mut debug_cpu::StatusRegister) {
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

impl RegisterArea for DebugCpu {
    fn registers_area(ui: &mut egui::Ui, registers: &mut debug_cpu::Registers) {
        ui.vertical(|ui| {
            egui::Frame::group(ui.style())
                .outer_margin(egui::Margin {
                    right: 4.0,
                    bottom: 6.0,
                    ..Default::default()
                })
                .show(ui, |ui| {
                    cpu_registers_panel(ui, registers);
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
                    cpu_status_register_panel(ui, &mut registers.p);
                });
        });
    }
}

/// Adds editable view of all SMP registers.
fn smp_registers_panel(ui: &mut egui::Ui, registers: &mut debug_smp::Registers) {
    register_drag_value(ui, &mut registers.a, "A=", 2);
    register_drag_value(ui, &mut registers.x, "X=", 2);
    register_drag_value(ui, &mut registers.y, "Y=", 2);
    register_drag_value(ui, &mut registers.pc, "PC=", 4);
    register_drag_value(ui, &mut registers.sp, "SP=", 2);
    register_drag_value(ui, &mut registers.psw, "PSW=", 2);
}

/// Adds checkboxes for the bits of the SMP status register.
fn smp_status_register_panel(ui: &mut egui::Ui, status_register: &mut debug_smp::StatusRegister) {
    ui.vertical(|ui| {
        ui.checkbox(&mut status_register.n, "N");
        ui.checkbox(&mut status_register.v, "V");
        ui.checkbox(&mut status_register.p, "P");
        ui.checkbox(&mut status_register.b, "B");
        ui.checkbox(&mut status_register.h, "H");
        ui.checkbox(&mut status_register.i, "I");
        ui.checkbox(&mut status_register.z, "Z");
        ui.checkbox(&mut status_register.c, "C");
    });
}

impl RegisterArea for DebugSmp {
    fn registers_area(ui: &mut egui::Ui, registers: &mut debug_smp::Registers) {
        ui.vertical(|ui| {
            egui::Frame::group(ui.style())
                .outer_margin(egui::Margin {
                    right: 4.0,
                    bottom: 6.0,
                    ..Default::default()
                })
                .show(ui, |ui| {
                    smp_registers_panel(ui, registers);
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
                    smp_status_register_panel(ui, &mut registers.psw);
                });
        });
    }
}
