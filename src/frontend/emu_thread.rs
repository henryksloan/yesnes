use crate::disassembler::{DebugCpu, DebugProcessor, Disassembler};
use crate::snes::SNES;

use crossbeam::channel;

use std::sync::{Arc, Mutex};
use std::thread;

// TODO: This should probably be done internally. It might be best if it
// didn't require `impl Send for Disassembler`, i.e. if Disassembler didn't have an
// Rc<RefCell<Bus>>; maybe that could be passed straight from cpu at the end of each instruction.
pub fn run_instruction_and_disassemble<D: DebugProcessor>(
    snes: &mut SNES,
    disassembler: &Mutex<Disassembler<D>>,
) -> bool {
    let breakpoint = snes.run_instruction_debug(D::DEVICE);
    let registers = D::registers(snes);
    disassembler
        .lock()
        .unwrap()
        .update_disassembly_at(D::pc(&registers), D::to_analysis_state(&registers));
    breakpoint
}

pub enum EmuThreadMessage {
    Continue,
    // TODO: Make this per-processor (?)
    RunToAddress(usize),
}

pub fn run_emu_thread(
    snes: Arc<Mutex<SNES>>,
    disassembler: Arc<Mutex<Disassembler<DebugCpu>>>,
    receiver: channel::Receiver<EmuThreadMessage>,
    paused: Arc<Mutex<bool>>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || loop {
        let Ok(message) = receiver.recv() else {
            log::debug!("Exiting emulation thread");
            return;
        };
        match message {
            EmuThreadMessage::Continue => {
                *paused.lock().unwrap() = false;
                while !*paused.lock().unwrap() {
                    let should_break =
                        run_instruction_and_disassemble(&mut snes.lock().unwrap(), &disassembler);
                    if should_break {
                        *paused.lock().unwrap() = true;
                    }
                }
            }
            // TODO: This should correctly handle mirrored PC addresses
            EmuThreadMessage::RunToAddress(addr) => {
                *paused.lock().unwrap() = false;
                loop {
                    if *paused.lock().unwrap() {
                        break;
                    }
                    let mut snes = snes.lock().unwrap();
                    if Into::<usize>::into(snes.cpu.borrow().registers().pc) == addr {
                        *paused.lock().unwrap() = true;
                        break;
                    } else {
                        let should_break =
                            run_instruction_and_disassemble(&mut snes, &disassembler);
                        if should_break {
                            *paused.lock().unwrap() = true;
                        }
                    }
                }
            }
        }
    })
}
