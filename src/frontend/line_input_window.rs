//! A window with a `TextEdit::singleline` that detects and handles submission.
use eframe::egui;

pub struct LineInputWindow {
    title: String,
    open: bool,
    text: String,
    request_focus_once: bool,
}

impl LineInputWindow {
    pub fn new(title: String) -> Self {
        Self {
            title,
            open: false,
            text: String::new(),
            request_focus_once: false,
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
        let mut submitted = false;
        egui::Window::new(&self.title)
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
    }
}
