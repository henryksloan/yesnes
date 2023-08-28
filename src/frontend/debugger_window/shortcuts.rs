use crate::frontend::app_window::ToEguiShortcut;

use egui::{Key, KeyboardShortcut, Modifiers};

#[derive(Clone, Copy)]
pub enum DisassemblerShortcut {
    GoToAddress,
    GoToProgramCounter,
    Continue,
    Pause,
    Trace,
    RunToAddress,
    Reset,
}

use DisassemblerShortcut::*;

// Iterated over by the frontend to register and consume all shortcuts each frame.
pub const DISASSEMBLER_SHORTCUTS: &'static [DisassemblerShortcut] = &[
    GoToAddress,
    GoToProgramCounter,
    Continue,
    Pause,
    Trace,
    RunToAddress,
    Reset,
];

const GO_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::G);
const GO_TO_PROGRAM_COUNTER: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::P);
const CONTINUE: KeyboardShortcut = KeyboardShortcut::new(Modifiers::NONE, Key::F5);
const PAUSE: KeyboardShortcut = KeyboardShortcut::new(Modifiers::NONE, Key::F6);
const TRACE: KeyboardShortcut = KeyboardShortcut::new(Modifiers::NONE, Key::F7);
const RUN_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::T);
const RESET: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::R);

impl ToEguiShortcut for DisassemblerShortcut {
    fn to_egui_shortcut(&self) -> &'static KeyboardShortcut {
        match *self {
            GoToAddress => &GO_TO_ADDRESS,
            GoToProgramCounter => &GO_TO_PROGRAM_COUNTER,
            Continue => &CONTINUE,
            Pause => &PAUSE,
            Trace => &TRACE,
            RunToAddress => &RUN_TO_ADDRESS,
            Reset => &RESET,
        }
    }
}
