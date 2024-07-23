mod app_window;
mod debugger_window;
mod emu_thread;
mod line_input_window;
mod memory_view_window;
mod registers;
mod screen_window;
mod yesnes_app;

fn main() -> Result<(), eframe::Error> {
    env_logger::init();
    yesnes_app::run()
}
