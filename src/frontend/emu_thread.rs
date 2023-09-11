use crate::disassembler::{DebugCpu, DebugProcessor, DebugSmp, Disassembler};
use crate::scheduler::Device;
use crate::snes::SNES;

use crossbeam::channel;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

// TODO: This should probably be done internally. It might be best if it
// didn't require `impl Send for Disassembler`, i.e. if Disassembler didn't have an
// Rc<RefCell<Bus>>; maybe that could be passed straight from cpu at the end of each instruction.
pub fn run_instruction_and_disassemble<D: DebugProcessor>(
    snes: &mut SNES,
    disassembler: &Mutex<Disassembler<D>>,
) -> bool {
    let (breakpoint, _) = snes.run_instruction_debug(D::DEVICE);
    let registers = D::registers(snes);
    disassembler
        .lock()
        .unwrap()
        .update_disassembly_at(D::pc(&registers), D::to_analysis_state(&registers));
    breakpoint
}

pub fn run_frame_and_disassemble<D: DebugProcessor>(
    snes: &mut SNES,
    disassembler: &Mutex<Disassembler<D>>,
) -> bool {
    let mut breakpoint = false;
    let mut frame_ready = false;
    while !frame_ready {
        (breakpoint, frame_ready) = snes.run_instruction_debug(D::DEVICE);
        let registers = D::registers(snes);
        disassembler
            .lock()
            .unwrap()
            .update_disassembly_at(D::pc(&registers), D::to_analysis_state(&registers));
        if breakpoint {
            break;
        }
    }
    breakpoint
}

pub enum EmuThreadMessage {
    Continue(Device),
    RunToAddress(Device, usize),
}

pub fn run_emu_thread(
    snes: Arc<Mutex<SNES>>,
    // TODO: This could potentially be simplified by having the client sending dynamic Arcs (?) on-demand
    // But for now Disassembler is not a trait, but a template...
    cpu_disassembler: Arc<Mutex<Disassembler<DebugCpu>>>,
    smp_disassembler: Arc<Mutex<Disassembler<DebugSmp>>>,
    receiver: channel::Receiver<EmuThreadMessage>,
    paused: Arc<AtomicBool>,
    frame_ready: Arc<AtomicBool>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || loop {
        let Ok(message) = receiver.recv() else {
            log::debug!("Exiting emulation thread");
            return;
        };
        match message {
            EmuThreadMessage::Continue(device) => {
                paused.store(false, Ordering::SeqCst);
                while !paused.load(Ordering::Relaxed) {
                    // TODO: I think a capacityless channel might be better, though the consumer side shouldn't block.
                    while frame_ready.load(Ordering::Acquire) {
                        std::hint::spin_loop();
                    }
                    let snes = &mut snes.lock().unwrap();
                    let should_break = match device {
                        Device::CPU => run_frame_and_disassemble(snes, &cpu_disassembler),
                        Device::SMP => run_frame_and_disassemble(snes, &smp_disassembler),
                        _ => panic!(),
                    };
                    frame_ready.store(true, Ordering::Release);
                    if should_break {
                        paused.store(true, Ordering::SeqCst);
                    }
                }
            }
            // TODO: This should correctly handle mirrored PC addresses
            EmuThreadMessage::RunToAddress(device, addr) => {
                paused.store(false, Ordering::SeqCst);
                loop {
                    if paused.load(Ordering::Relaxed) {
                        break;
                    }
                    let mut snes = snes.lock().unwrap();
                    // TODO: It would be best if 1) we could just use D::Registers, D::pc, and
                    // 2) if D::Registers somehow returned a reference, not a clone
                    let pc: usize = match device {
                        Device::CPU => snes.cpu.borrow().registers().pc.into(),
                        Device::SMP => snes.smp.borrow().registers().pc.into(),
                        _ => panic!(),
                    };
                    if pc == addr {
                        paused.store(true, Ordering::SeqCst);
                        break;
                    } else {
                        let should_break = match device {
                            Device::CPU => {
                                run_instruction_and_disassemble(&mut snes, &cpu_disassembler)
                            }
                            Device::SMP => {
                                run_instruction_and_disassemble(&mut snes, &smp_disassembler)
                            }
                            _ => panic!(),
                        };
                        if should_break {
                            paused.store(true, Ordering::SeqCst);
                        }
                    }
                }
            }
        }
    })
}
