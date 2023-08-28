use crate::frontend::app_window::ToEguiShortcut;

use egui::{Key, KeyboardShortcut, Modifiers};

#[derive(Clone, Copy)]
pub enum MemoryViewShortcut {
    GoToAddress,
}

use MemoryViewShortcut::*;

// Iterated over by the frontend to register and consume all shortcuts each frame.
pub const MEMORY_VIEW_SHORTCUTS: &'static [MemoryViewShortcut] = &[GoToAddress];

const GO_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::G);

impl ToEguiShortcut for MemoryViewShortcut {
    fn to_egui_shortcut(&self) -> &'static KeyboardShortcut {
        match *self {
            GoToAddress => &GO_TO_ADDRESS,
        }
    }
}
