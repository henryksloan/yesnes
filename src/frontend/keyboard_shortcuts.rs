pub const GO_TO_ADDRESS_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::COMMAND, egui::Key::G);
pub const RUN_TO_ADDRESS_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::COMMAND, egui::Key::R);

pub fn button_with_shortcut(
    ctx: &egui::Context,
    ui: &mut egui::Ui,
    text: impl Into<egui::WidgetText>,
    shortcut: &egui::KeyboardShortcut,
) -> bool {
    let shortcut_pressed = ctx.input_mut(|i| i.consume_shortcut(shortcut));
    let button = egui::Button::new(text).shortcut_text(ui.ctx().format_shortcut(&shortcut));
    let response = ui.add(button);
    response.clicked() || shortcut_pressed
}
