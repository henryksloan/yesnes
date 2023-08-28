//! A window with a `TextEdit::singleline` that detects and handles submission.
use eframe::egui;

pub struct LineInputWindow {
    id: egui::Id,
    title: String,
    open: bool,
    text: String,
    request_focus_once: bool,
    parent_window_id: Option<egui::Id>,
}

impl LineInputWindow {
    pub fn new(title: String, parent_window_id: Option<egui::Id>) -> Self {
        let id = if let Some(parent_window_id) = parent_window_id {
            parent_window_id.with(&title)
        } else {
            egui::Id::new(&title)
        };
        Self {
            id,
            title,
            open: false,
            text: String::new(),
            request_focus_once: false,
            parent_window_id,
        }
    }

    // If the window is closed, open it and clear+focus the textbox.
    pub fn open(&mut self) {
        self.text.clear();
        self.open = true;
        self.request_focus_once = true;
    }

    // If open, show the input window, calling the callback with the
    // input text if it was just submitted.
    pub fn show<F>(&mut self, ctx: &egui::Context, callback: F)
    where
        F: FnOnce(&str),
    {
        let open_before = self.open;
        let mut submitted = false;
        egui::Window::new(&self.title)
            .id(self.id)
            .open(&mut self.open)
            .resizable(false)
            .collapsible(false)
            .show(ctx, |ui| {
                let text_edit = ui.add(egui::TextEdit::singleline(&mut self.text));
                if text_edit.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                    submitted = true;
                    callback(&self.text);
                }
                if std::mem::take(&mut self.request_focus_once) {
                    text_edit.request_focus();
                }
            });
        if submitted {
            self.open = false;
        }
        if open_before && !self.open {
            // If submitted or closed, focus the parent window (if specified)
            if let Some(parent_window_id) = self.parent_window_id {
                ctx.move_to_top(egui::LayerId::new(egui::Order::Middle, parent_window_id))
            }
        }
    }
}
