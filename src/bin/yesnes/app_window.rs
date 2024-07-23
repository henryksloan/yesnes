use egui::KeyboardShortcut;

pub trait AppWindow {
    fn id(&self) -> egui::Id;
    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool);

    fn show(&mut self, ctx: &egui::Context, paused: bool, active_window_id: Option<egui::Id>) {
        let focused = Some(self.id()) == active_window_id;
        self.show_impl(ctx, paused, focused);
    }
}

pub trait ToEguiShortcut {
    fn to_egui_shortcut(&self) -> &'static KeyboardShortcut;
}

pub trait ShortcutWindow: AppWindow {
    type Shortcut: 'static + ToEguiShortcut;
    const WINDOW_SHORTCUTS: &'static [Self::Shortcut];

    fn handle_shortcut(&mut self, shortcut: &Self::Shortcut);
    fn shortcut_enabled(&mut self, shortcut: &Self::Shortcut) -> bool;

    fn consume_shortcuts(&mut self, ctx: &egui::Context) {
        for shortcut in Self::WINDOW_SHORTCUTS {
            if ctx.input_mut(|i| i.consume_shortcut(&shortcut.to_egui_shortcut())) {
                if self.shortcut_enabled(shortcut) {
                    self.handle_shortcut(shortcut);
                }
            }
        }
    }

    fn button_with_shortcut_impl(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Self::Shortcut,
        text: impl Into<egui::WidgetText>,
        is_menu_button: bool,
    ) {
        let enabled = self.shortcut_enabled(&shortcut);
        let mut button = egui::Button::new(text);
        if is_menu_button {
            button = button.shortcut_text(ui.ctx().format_shortcut(&shortcut.to_egui_shortcut()));
        }
        if ui.add_enabled(enabled, button).clicked() {
            self.handle_shortcut(&shortcut);
            if is_menu_button {
                ui.close_menu();
            }
        }
    }

    fn button_with_shortcut(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Self::Shortcut,
        text: impl Into<egui::WidgetText>,
    ) {
        self.button_with_shortcut_impl(ui, shortcut, text, false);
    }

    fn menu_button_with_shortcut(
        &mut self,
        ui: &mut egui::Ui,
        shortcut: Self::Shortcut,
        text: impl Into<egui::WidgetText>,
    ) {
        self.button_with_shortcut_impl(ui, shortcut, text, true);
    }

    fn show_with_shortcuts(
        &mut self,
        ctx: &egui::Context,
        paused: bool,
        active_window_id: Option<egui::Id>,
    ) {
        let focused = Some(self.id()) == active_window_id;
        if focused {
            self.consume_shortcuts(ctx);
        }
        self.show_impl(ctx, paused, focused);
    }
}
