use super::app_window::AppWindow;

use yesnes::frame_history::FrameHistory;
use yesnes::snes::SNES;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{SampleRate, Stream, StreamConfig};
use eframe::egui::{Color32, ColorImage, TextureHandle};

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

fn write_data(output: &mut [f32], channels: usize, audio_buffer: Arc<Mutex<VecDeque<(f32, f32)>>>) {
    let mut values = vec![(0.0, 0.0); output.len() / channels];
    {
        let mut buffer = audio_buffer.lock().unwrap();
        for i in 0..values.len() {
            if let Some(value) = buffer.pop_front() {
                values[i] = value;
            } else {
                break;
            }
        }
    }
    for (i, frame) in output.chunks_mut(channels).enumerate() {
        let value = values[i];
        for sample in frame.iter_mut() {
            *sample = if i % 2 == 0 { value.0 } else { value.1 };
        }
    }
}

fn make_audio_stream(audio_buffer: Arc<Mutex<VecDeque<(f32, f32)>>>) -> (Stream, SampleRate) {
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("no output device available");
    let supported_configs_range = device
        .supported_output_configs()
        .expect("error while querying configs");
    let supported_config = supported_configs_range
        // DO NOT SUBMIT: Will forcing f32 not work on some systems?
        .filter(|config| config.sample_format() == cpal::SampleFormat::F32)
        // .find_map(|config| config.try_with_sample_rate(cpal::SampleRate(32000)))
        // .find_map(|config| config.try_with_sample_rate(cpal::SampleRate(44100)))
        .next()
        .expect("No supported audio config")
        .with_max_sample_rate();
    let err_fn = |err| eprintln!("an error occurred on the output audio stream: {}", err);
    let config: StreamConfig = supported_config.into();

    let sample_rate = config.sample_rate;
    let channels = config.channels as usize;

    let stream = device
        .build_output_stream(
            &config,
            move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                // write_data(data, channels, &mut next_value)
                write_data(data, channels, audio_buffer.clone())
            },
            err_fn,
            None,
        )
        .unwrap();
    (stream, sample_rate)
}

pub struct ScreenWindow {
    id: egui::Id,
    title: String,
    snes: Arc<Mutex<SNES>>,
    image: ColorImage,
    texture: Option<TextureHandle>,
    frame_ready: Arc<AtomicBool>,
    frame_history: FrameHistory,
    previous_frame_instant: Option<Instant>,
    lock_fps: bool,
    debug_audio_generator: Box<dyn FnMut(f32) -> (f32, f32)>,
    debug_audio_buffer: Arc<Mutex<VecDeque<(f32, f32)>>>,
    audio_sample_rate: SampleRate,
    audio_stream: Stream,
}

impl ScreenWindow {
    pub fn new(title: String, snes: Arc<Mutex<SNES>>, frame_ready: Arc<AtomicBool>) -> Self {
        let id = egui::Id::new(&title);
        let image = ColorImage::new([256, 224], Color32::BLACK);

        let debug_audio_buffer = Arc::new(Mutex::new(VecDeque::with_capacity(32000)));

        let (audio_stream, sample_rate) = make_audio_stream(debug_audio_buffer.clone());
        audio_stream.pause().unwrap();

        let amplitude = 0.01; // up to 1.0
        let mut sample_clock = 0f32;
        let next_value = move |freq: f32| {
            sample_clock = (sample_clock + 1.0) % sample_rate.0 as f32;
            let result = amplitude
                // * (sample_clock * 440.0 * 2.0 * std::f32::consts::PI / sample_rate.0 as f32).sin();
                * (sample_clock * freq * 2.0 * std::f32::consts::PI / sample_rate.0 as f32).sin();
            (result, result)
        };

        Self {
            id,
            title,
            snes,
            image,
            texture: None,
            frame_ready,
            frame_history: FrameHistory::new(),
            previous_frame_instant: None,
            lock_fps: true,
            debug_audio_generator: Box::new(next_value),
            debug_audio_buffer,
            audio_sample_rate: sample_rate,
            audio_stream,
        }
    }
}

impl AppWindow for ScreenWindow {
    fn id(&self) -> egui::Id {
        self.id
    }

    fn show_impl(&mut self, ctx: &egui::Context, paused: bool, focused: bool) {
        if paused {
            self.audio_stream.pause().unwrap();
        } else {
            self.audio_stream.play().unwrap();
        }

        if !paused {
            while !self.frame_ready.load(Ordering::Relaxed) {
                std::hint::spin_loop();
            }
            // TODO: Much of this need not happen under the lock
            let mut frame = None;
            if let Ok(mut snes) = self.snes.lock() {
                self.frame_ready.store(false, Ordering::Relaxed);
                frame = snes.take_frame();
                if focused && !paused {
                    // TODO: Analyze input latency
                    let controller_state = ctx.input(|input_state| {
                        const KEYS: &[egui::Key] = &[
                            egui::Key::C,          // B
                            egui::Key::X,          // Y
                            egui::Key::Enter,      // Select
                            egui::Key::Space,      // Start
                            egui::Key::ArrowUp,    // Up
                            egui::Key::ArrowDown,  // Down
                            egui::Key::ArrowLeft,  // Left
                            egui::Key::ArrowRight, // Right
                            egui::Key::V,          // A
                            egui::Key::D,          // X
                            egui::Key::A,          // L
                            egui::Key::S,          // R
                        ];
                        let mut controller_state = 0;
                        for (i, key) in KEYS.iter().enumerate() {
                            controller_state |= (input_state.key_down(*key) as u16) << (15 - i);
                        }
                        controller_state
                    });
                    snes.set_controller_state(0, controller_state);
                }

                if let Ok(mut debug_audio_buffer) = self.debug_audio_buffer.lock() {
                    // for i in 0..self.audio_sample_rate.0 / 60 {
                    // while debug_audio_buffer.len() < 3 * (self.audio_sample_rate.0 as usize / 60) / 2 {
                    // println!("{}", debug_audio_buffer.len());
                    let num_samples = (2 * (self.audio_sample_rate.0 as usize / 60))
                        .saturating_sub(debug_audio_buffer.len());
                    // let num_samples = (self.audio_sample_rate.0 as usize / 60)
                    //     .saturating_sub(debug_audio_buffer.len());
                    // let smp_pitch = snes.debug_take_audio(num_samples);
                    let sample_ratio = 32000. / self.audio_sample_rate.0 as f32;
                    // let sample_ratio = 1.;
                    let adjusted_samples = (sample_ratio * num_samples as f32) as usize;
                    // let smp_pitch = snes.debug_take_pitch(adjusted_samples);
                    // while debug_audio_buffer.len() < 2 * (self.audio_sample_rate.0 as usize / 60) {
                    // for i in 0..num_samples {
                    //     // while debug_audio_buffer.len() < self.audio_sample_rate.0 as usize {
                    //     // debug_audio_buffer.push_back((self.debug_audio_generator)());
                    //     // debug_audio_buffer.push_back((self.debug_audio_generator)(440.));
                    //     // debug_audio_buffer.push_back((self.debug_audio_generator)(smp_pitch[i].0));
                    //     debug_audio_buffer.push_back((self.debug_audio_generator)(
                    //         smp_pitch[(i as f32 * sample_ratio) as usize % adjusted_samples].0,
                    //     ));
                    // }

                    let smp_val = snes.debug_take_audio(adjusted_samples);
                    // for i in 0..num_samples {
                    //     // while debug_audio_buffer.len() < self.audio_sample_rate.0 as usize {
                    //     // debug_audio_buffer.push_back((self.debug_audio_generator)());
                    //     // debug_audio_buffer.push_back((self.debug_audio_generator)(440.));
                    //     // debug_audio_buffer.push_back((self.debug_audio_generator)(smp_pitch[i].0));
                    //     debug_audio_buffer.push_back(
                    //         smp_val[(i as f32 * sample_ratio) as usize % adjusted_samples],
                    //     );
                    // }

                    // Simple linear interpolation
                    for i in 0..num_samples {
                        let intermediate_i = i as f32 * sample_ratio;
                        let fract = (i as f32 * sample_ratio).fract();
                        let val = (smp_val[(intermediate_i.floor()) as usize % adjusted_samples].0
                            * fract)
                            + (smp_val[(intermediate_i.ceil()) as usize % adjusted_samples].0
                                * (1.0 - fract));
                        debug_audio_buffer.push_back((val, val));
                    }
                }
            }
            if let Some(frame) = frame {
                let render_delta = self
                    .previous_frame_instant
                    .map(|previous| (Instant::now() - previous).as_secs_f32());
                if let Some(delta) = render_delta {
                    if self.lock_fps && delta < 0.016 {
                        std::thread::sleep(std::time::Duration::from_secs_f32(0.016 - delta));
                    }
                }
                let display_delta = self
                    .previous_frame_instant
                    .map(|previous| (Instant::now() - previous).as_secs_f32());
                self.frame_history
                    .on_new_frame(ctx.input(|i| i.time), display_delta);
                self.previous_frame_instant = Some(Instant::now());
                for y in 0..224 {
                    for x in 0..256 {
                        let color = frame[y][x];
                        self.image[(x, y)] = Color32::from_rgb(color[0], color[1], color[2]);
                    }
                }
            }
        }
        match &mut self.texture {
            Some(t) => t.set(self.image.clone(), egui::TextureOptions::NEAREST),
            None => {
                self.texture = Some(ctx.load_texture(
                    "snes-screen",
                    self.image.clone(),
                    egui::TextureOptions::NEAREST,
                ))
            }
        };

        egui::Window::new(&self.title)
            .default_width(512.0)
            .default_height(512.0)
            .resizable(true)
            .scroll(egui::Vec2b::FALSE)
            .show(ctx, |ui| {
                if !focused {
                    ui.disable();
                }
                ui.horizontal(|ui| {
                    ui.label(format!(
                        "Mean frame time: {:.2}ms",
                        1e3 * self.frame_history.mean_frame_time()
                    ));
                    ui.checkbox(&mut self.lock_fps, "Lock FPS");
                });
                // TODO: This is a janky resizing implementation to maintain aspect ratio
                let rect = ui.available_rect_before_wrap();
                let new_size = {
                    let available_size = ui.available_size();
                    egui::Vec2::new(available_size.max_elem(), available_size.max_elem())
                };
                ui.allocate_space(new_size);
                ui.ctx().request_repaint();
                egui::Image::new(self.texture.as_ref().unwrap())
                    .maintain_aspect_ratio(true)
                    .paint_at(ui, rect);
            });
    }
}
