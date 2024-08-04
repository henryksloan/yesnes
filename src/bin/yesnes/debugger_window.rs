mod shortcuts;

use shortcuts::*;

use super::app_window::{AppWindow, ShortcutWindow};
use super::emu_thread::{run_instruction_and_disassemble, EmuThreadMessage};
use super::line_input_window::*;
use super::registers::*;

use std::collections::HashSet;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use crossbeam::channel;
use egui_extras::{Column, TableBuilder};

use yesnes::debug_point::DebugPoint;
use yesnes::disassembler::{DebugProcessor, DisassembledInstruction, Disassembler};
use yesnes::snes::SNES;

pub struct DebuggerWindow<D: DebugProcessor> {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    disassembler: Arc<Mutex<Disassembler<D>>>,
    emu_paused: Arc<AtomicBool>,
    registers_mirror: D::Registers,
    scroll_to_row: Option<(usize, egui::Align)>,
    prev_top_row: Option<usize>,
    prev_bottom_row: Option<usize>,
    go_to_address_window: LineInputWindow,
    run_to_address_window: LineInputWindow,
    emu_message_sender: channel::Sender<EmuThreadMessage>,
    // TODO: A BTreeSet might help if we use sorted traversal
    breakpoint_addrs: HashSet<usize>,
}

impl<D: DebugProcessor + RegisterArea> DebuggerWindow<D> {
    pub fn new(
        title: String,
        snes: Arc<Mutex<SNES>>,
        disassembler: Arc<Mutex<Disassembler<D>>>,
        emu_paused: Arc<AtomicBool>,
        emu_message_sender: channel::Sender<EmuThreadMessage>,
    ) -> Self {
        let id = egui::Id::new(&title);
        let mut window = Self {
            id,
            title,
            snes,
            disassembler,
            emu_paused,
            registers_mirror: D::Registers::default(),
            scroll_to_row: None,
            prev_top_row: None,
            prev_bottom_row: None,
            go_to_address_window: LineInputWindow::new("Go to address".to_string(), Some(id)),
            run_to_address_window: LineInputWindow::new("Run to address".to_string(), Some(id)),
            emu_message_sender,
            breakpoint_addrs: HashSet::new(),
        };
        window.scroll_pc_near_top();
        window
    }

    fn scroll_pc_near_top(&mut self) {
        if let Ok(snes) = self.snes.lock() {
            let pc = D::pc(&D::registers(&snes));
            let pc_line = self
                .disassembler
                .lock()
                .unwrap()
                .get_line_index(pc)
                .saturating_sub(1);
            self.scroll_to_row = Some((pc_line, egui::Align::TOP));
        }
    }

    fn menu_bar(&mut self, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            use DisassemblerShortcut::*;
            ui.menu_button("Run", |ui| {
                // TODO: Not necessarily a fan of duplicating these in the UI/menu.
                self.menu_button_with_shortcut(ui, Continue, "Continue");
                self.menu_button_with_shortcut(ui, Pause, "Pause");
                self.menu_button_with_shortcut(ui, Trace, "Trace");
                self.menu_button_with_shortcut(ui, RunToAddress, "To address...");
                self.menu_button_with_shortcut(ui, UntilInterrupt, "Until interrupt");
                self.menu_button_with_shortcut(ui, FinishInterrupt, "Finish interrupt");
                ui.separator();
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
        // If `paused`, then lock the SNES, potentially update registers from the UI, and write them back.
        // `paused` can be invalidated mid-frame, so it serves as a hint. We still have to exclude the lock
        // for this whole critical section.
        // TODO: Revise this if I come up with a better way to handle frontend pausing
        let mut maybe_snes_guard = None;
        if paused {
            let snes = self.snes.lock().unwrap();
            self.registers_mirror = D::registers(&snes);
            maybe_snes_guard = Some(snes);
        }
        ui.horizontal(|ui| {
            ui.set_enabled(paused);
            D::registers_area(ui, &mut self.registers_mirror);
        });
        if let Some(snes) = maybe_snes_guard {
            D::set_registers(&snes, &self.registers_mirror);
        }
    }

    fn control_area(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            self.button_with_shortcut(ui, DisassemblerShortcut::Continue, "Continue");
            self.button_with_shortcut(ui, DisassemblerShortcut::Pause, "Pause");
            self.button_with_shortcut(ui, DisassemblerShortcut::Trace, "Trace");
            // TODO: Implement step over more completely
            let button = egui::Button::new("Step over");
            if ui
                .add_enabled(self.emu_paused.load(Ordering::Acquire), button)
                .clicked()
            {
                let snes = self.snes.lock().unwrap();
                let pc = D::pc(&D::registers(&snes));
                let pc_line = self.disassembler.lock().unwrap().get_line_index(pc);
                let next_line = self.disassembler.lock().unwrap().get_line(pc_line + 1);
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::RunToAddress(D::DEVICE, next_line.0));
            }
            self.button_with_shortcut(ui, DisassemblerShortcut::Reset, "Reset");
        });
    }

    fn show_windows(&mut self, ctx: &egui::Context) {
        self.go_to_address_window.show(ctx, |text| {
            let lower_input = text.to_lowercase();
            let trimmed_input = lower_input.trim().trim_start_matches("0x");
            let parsed_addr = u32::from_str_radix(trimmed_input, 16);
            if let Ok(addr) = parsed_addr {
                let addr = D::Address::try_from(addr as usize).unwrap_or_default();
                let addr_line = self.disassembler.lock().unwrap().get_line_index(addr);
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
                // TODO: This should work like a do-while; if we start on the right address, run until we hit it again
                // (FWIW this whole feature could be replaced with breakpoints, for which the above is more obvious)
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::RunToAddress(D::DEVICE, addr as usize));
            }
        });
    }

    // If clicked, returns the address of the row
    fn disassembly_row(
        disassembler: &Disassembler<D>,
        paused: bool,
        registers_mirror: &D::Registers,
        breakpoint_addrs: &HashSet<usize>,
        prev_top_row: &mut Option<usize>,
        prev_bottom_row: &mut Option<usize>,
        row_index: usize,
        mut row: egui_extras::TableRow,
    ) -> Option<usize> {
        let mut clicked = false;
        if prev_top_row.is_none() {
            *prev_top_row = Some(row_index);
        }
        *prev_bottom_row = Some(row_index);
        let disassembly_line = disassembler.get_line(row_index);
        let row_addr = disassembly_line.0;
        row.col(|ui| {
            if breakpoint_addrs.contains(&row_addr) {
                let max_rect = ui.available_rect_before_wrap();
                let item_spacing = ui.spacing().item_spacing;
                let gapless_rect = max_rect
                    .expand2(0.5 * item_spacing)
                    .expand2(egui::vec2(1., 1.05));
                ui.painter()
                    .rect_filled(gapless_rect, egui::Rounding::ZERO, egui::Color32::RED);
                ui.style_mut().visuals.override_text_color = Some(egui::Color32::WHITE);
            }
            if paused && row_addr == D::pc(&registers_mirror).into() {
                ui.style_mut().visuals.override_text_color = Some(egui::Color32::KHAKI);
            }
            let label = ui.label(format!("{:06X}", row_addr));
            if label.contains_pointer() {
                ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
            }
            if label.clicked() {
                clicked = true;
            }
        });
        row.col(|ui| {
            ui.label(format!(
                "{} {}",
                disassembly_line.1.mnemonic(),
                disassembly_line.1.mode_str(),
            ));
        });
        clicked.then_some(row_addr)
    }

    fn disassembly_table(&mut self, ui: &mut egui::Ui, paused: bool) {
        let total_lines = self.disassembler.lock().unwrap().get_num_lines();
        let text_height = egui::TextStyle::Body.resolve(ui.style()).size;
        let mut table = TableBuilder::new(ui)
            .striped(true)
            .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
            .column(Column::exact(45.0))
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
                // TODO: I'd like these lines to include the hex data of the opcode and operand
                header.col(|ui| {
                    ui.strong("Instruction");
                });
            })
            .body(|body| {
                let disassembler = self.disassembler.lock().unwrap();
                body.rows(text_height, total_lines, |row| {
                    let clicked_addr = Self::disassembly_row(
                        &disassembler,
                        paused,
                        &self.registers_mirror,
                        &self.breakpoint_addrs,
                        &mut self.prev_top_row,
                        &mut self.prev_bottom_row,
                        row.index(),
                        row,
                    );
                    if let Some(clicked_addr) = clicked_addr {
                        if !self.breakpoint_addrs.remove(&clicked_addr) {
                            self.breakpoint_addrs.insert(clicked_addr);
                        }
                    }
                });
            });
    }
}

impl<D: DebugProcessor + RegisterArea> AppWindow for DebuggerWindow<D> {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        self.show_windows(ctx);

        egui::Window::new(&self.title)
            .default_width(320.0)
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

impl<D: DebugProcessor + RegisterArea> ShortcutWindow for DebuggerWindow<D> {
    type Shortcut = DisassemblerShortcut;

    const WINDOW_SHORTCUTS: &'static [Self::Shortcut] = DISASSEMBLER_SHORTCUTS;

    fn handle_shortcut(&mut self, shortcut: &Self::Shortcut) {
        match shortcut {
            Self::Shortcut::GoToAddress => {
                self.go_to_address_window.open();
            }
            Self::Shortcut::GoToProgramCounter => {
                if self.emu_paused.load(Ordering::Acquire) {
                    let snes = self.snes.lock().unwrap();
                    let pc = D::pc(&D::registers(&snes));
                    let pc_line = self.disassembler.lock().unwrap().get_line_index(pc);
                    self.scroll_to_row = Some((pc_line, egui::Align::Center));
                }
            }
            Self::Shortcut::Continue => {
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::Continue(D::DEVICE));
            }
            Self::Shortcut::Pause => {
                self.emu_paused.store(true, Ordering::SeqCst);
                if let Ok(snes) = self.snes.lock() {
                    let pc = D::pc(&D::registers(&snes));
                    let pc_line = self.disassembler.lock().unwrap().get_line_index(pc);
                    self.scroll_to_row = Some((pc_line, egui::Align::Center));
                }
            }
            // TODO: We should probably eagerly sync processors when tracing/stepping, etc.
            Self::Shortcut::Trace => {
                self.emu_paused.store(true, Ordering::SeqCst);
                if let Ok(mut snes) = self.snes.lock() {
                    run_instruction_and_disassemble(&mut snes, &self.disassembler, None);
                    let pc = D::pc(&D::registers(&snes));
                    let pc_line = self.disassembler.lock().unwrap().get_line_index(pc);
                    if let Some(prev_top_row) = self.prev_top_row {
                        if let Some(prev_bottom_row) = self.prev_bottom_row {
                            if (pc_line < prev_top_row) || (pc_line > prev_bottom_row) {
                                // Recenter PC if it jumped off-screen
                                self.scroll_to_row = Some((pc_line, egui::Align::Center));
                            } else if pc_line >= (prev_bottom_row - 1) {
                                // Keep the PC just above the bottom of the disassembly output
                                let new_bottom_line = pc_line + 1;
                                self.scroll_to_row = Some((new_bottom_line, egui::Align::BOTTOM));
                            }
                        }
                    }
                }
            }
            Self::Shortcut::RunToAddress => {
                self.run_to_address_window.open();
            }
            Self::Shortcut::UntilInterrupt => {
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::UntilDebugPoint(
                        D::DEVICE,
                        DebugPoint::StartedInterrupt,
                    ));
            }
            Self::Shortcut::FinishInterrupt => {
                let _ = self
                    .emu_message_sender
                    .send(EmuThreadMessage::UntilDebugPoint(
                        D::DEVICE,
                        DebugPoint::FinishedInterrupt,
                    ));
            }
            Self::Shortcut::Reset => {
                self.emu_paused.store(true, Ordering::SeqCst);
                if let Ok(mut snes) = self.snes.lock() {
                    snes.reset();
                }
                self.scroll_pc_near_top();
            }
        }
    }

    fn shortcut_enabled(&mut self, shortcut: &Self::Shortcut) -> bool {
        let paused = self.emu_paused.load(Ordering::Acquire);
        use DisassemblerShortcut::*;
        match *shortcut {
            Continue | UntilInterrupt | FinishInterrupt => paused,
            Pause => !paused,
            _ => true,
        }
    }
}
