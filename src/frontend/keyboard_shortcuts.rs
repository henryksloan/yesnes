use egui::{Key, KeyboardShortcut, Modifiers};

pub enum Shortcut {
    GoToAddress,
    RunToAddress,
}

use Shortcut::*;
// Iterated over by the frontend to register and consume all shortcuts each frame.
pub const ALL_SHORTCUTS: [Shortcut; 2] = [GoToAddress, RunToAddress];

impl Shortcut {
    const GO_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::G);
    const RUN_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::R);

    pub fn to_egui_shortcut(&self) -> &'static KeyboardShortcut {
        match *self {
            GoToAddress => &Self::GO_TO_ADDRESS,
            RunToAddress => &Self::RUN_TO_ADDRESS,
        }
    }
}
