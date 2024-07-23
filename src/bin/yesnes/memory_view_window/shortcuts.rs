use crate::app_window::ToEguiShortcut;

use egui::{Key, KeyboardShortcut, Modifiers};

#[derive(Clone, Copy)]
pub enum MemoryViewShortcut {
    GoToAddress,
    Refresh,
}

use MemoryViewShortcut::*;

// Iterated over by the frontend to register and consume all shortcuts each frame.
pub const MEMORY_VIEW_SHORTCUTS: &'static [MemoryViewShortcut] = &[GoToAddress, Refresh];

const GO_TO_ADDRESS: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::G);
const REFRESH: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::R);

impl ToEguiShortcut for MemoryViewShortcut {
    fn to_egui_shortcut(&self) -> &'static KeyboardShortcut {
        match *self {
            GoToAddress => &GO_TO_ADDRESS,
            Refresh => &REFRESH,
        }
    }
}
