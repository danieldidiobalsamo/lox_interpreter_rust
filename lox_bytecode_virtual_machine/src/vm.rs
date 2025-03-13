use crate::chunk::{Chunk, OpCode};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Vm {
    chunk: Chunk,
    ip: usize, // instruction pointer (next instruction, not the current one)
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum InterpreterResult {
    #[default]
    Ok,
    CompileError,
    RuntimeError,
}

impl Vm {
    pub fn new(chunk: &Chunk) -> Self {
        Self {
            chunk: chunk.clone(),
            ip: 0,
        }
    }
    pub fn run(&mut self) -> InterpreterResult {
        loop {
            let instruction = match self.chunk.get_instruction(self.ip) {
                Some(instruction) => instruction,
                None => break InterpreterResult::CompileError,
            };

            self.ip += 1;

            if cfg!(feature = "debug_trace") {
                println!("{instruction}");
            }

            match instruction {
                OpCode::OpReturn => return InterpreterResult::Ok,
                OpCode::OpConstant(c) => {
                    println!("{c}");
                }
            }
        }
    }
}
