use egui::{Key, KeyboardShortcut, Modifiers};

#[derive(Clone, Copy)]
pub enum Shortcut {
    GoToAddress,
    GoToProgramCounter,
    Continue,
    Pause,
    Trace,
    RunToAddress,
    Reset,
}

use Shortcut::*;
// Iterated over by the frontend to register and consume all shortcuts each frame.
pub const ALL_SHORTCUTS: &'static [Shortcut] = &[
    GoToAddress,
    GoToProgramCounter,
    Continue,
    Pause,
    Trace,
    RunToAddress,
    Reset,
];

impl Shortcut {
    const GO_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::G);
    const GO_TO_PROGRAM_COUNTER: KeyboardShortcut =
        KeyboardShortcut::new(Modifiers::COMMAND, Key::P);
    const CONTINUE: KeyboardShortcut = KeyboardShortcut::new(Modifiers::NONE, Key::F5);
    const PAUSE: KeyboardShortcut = KeyboardShortcut::new(Modifiers::NONE, Key::F6);
    const TRACE: KeyboardShortcut = KeyboardShortcut::new(Modifiers::NONE, Key::F7);
    const RUN_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::T);
    const RESET: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::R);

    pub fn to_egui_shortcut(&self) -> &'static KeyboardShortcut {
        match *self {
            GoToAddress => &Self::GO_TO_ADDRESS,
            GoToProgramCounter => &Self::GO_TO_PROGRAM_COUNTER,
            Continue => &Self::CONTINUE,
            Pause => &Self::PAUSE,
            Trace => &Self::TRACE,
            RunToAddress => &Self::RUN_TO_ADDRESS,
            Reset => &Self::RESET,
        }
    }
}
