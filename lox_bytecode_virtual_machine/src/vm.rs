use crate::chunk::{Chunk, OpCode, Value};

#[cfg(test)]
use std::{fs::OpenOptions, io::Write};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Vm {
    chunk: Chunk,
    ip: usize, // instruction pointer (next instruction, not the current one)
    stack: Vec<Value>,
    #[cfg(test)]
    log_file: String,
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
            stack: Vec::new(),
            #[cfg(test)]
            log_file: "".to_owned(),
        }
    }
    pub fn run(&mut self) -> InterpreterResult {
        #[cfg(any(feature = "debug_trace", test))]
        println!("[STACK] INSTRUCTION");

        loop {
            let instruction = match self.chunk.get_instruction(self.ip) {
                Some(instruction) => instruction,
                None => break InterpreterResult::CompileError,
            };

            self.ip += 1;

            #[cfg(any(feature = "debug_trace", test))]
            {
                let mut stack_trace = String::new();

                for val in &self.stack {
                    stack_trace += &format!("[ {val} ]");
                }

                if stack_trace.is_empty() {
                    stack_trace = "[]".to_owned();
                }

                println!("{stack_trace} {instruction}");
            }

            match instruction {
                OpCode::OpReturn => {
                    let val = self.pop().unwrap_or(Value::Nil);
                    println!("{}", val);

                    #[cfg(test)]
                    {
                        let mut file = OpenOptions::new()
                            .append(true)
                            .open(&self.log_file)
                            .unwrap();
                        file.write_all((val.to_string() + "\n").as_bytes()).unwrap();
                    }

                    return InterpreterResult::Ok;
                }
                OpCode::OpConstant(c) => match self.chunk.read_constant(*c) {
                    Some(val) => self.stack.push(val.clone()),
                    None => return InterpreterResult::RuntimeError,
                },

                OpCode::OpNegate => match self.pop() {
                    Some(Value::Number(val)) => self.push(Value::Number(-val)),
                    _ => return InterpreterResult::RuntimeError,
                },
                OpCode::OpAdd => {
                    if let (Some(Value::Number(b)), Some(Value::Number(a))) =
                        (self.pop(), self.pop())
                    {
                        self.stack.push(Value::Number(a + b));
                    } else {
                        return InterpreterResult::RuntimeError;
                    }
                }
                OpCode::OpSubtract => {
                    if let (Some(Value::Number(b)), Some(Value::Number(a))) =
                        (self.pop(), self.pop())
                    {
                        self.stack.push(Value::Number(a - b));
                    } else {
                        return InterpreterResult::RuntimeError;
                    }
                }
                OpCode::OpMultiply => {
                    if let (Some(Value::Number(b)), Some(Value::Number(a))) =
                        (self.pop(), self.pop())
                    {
                        self.stack.push(Value::Number(a * b));
                    } else {
                        return InterpreterResult::RuntimeError;
                    }
                }
                OpCode::OpDivide => {
                    if let (Some(Value::Number(b)), Some(Value::Number(a))) =
                        (self.pop(), self.pop())
                    {
                        if b == 0. {
                            return InterpreterResult::RuntimeError;
                        } else {
                            self.stack.push(Value::Number(a / b));
                        }
                    } else {
                        return InterpreterResult::RuntimeError;
                    }
                }
            }
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    #[cfg(test)]
    pub fn set_log_file(&mut self, log_file: &str) {
        self.log_file = log_file.to_string();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::{
        fs::{self, File},
        path::Path,
        sync::{Mutex, OnceLock},
    };

    struct Setup {
        id: usize,
    }

    impl Setup {
        fn new() -> &'static Mutex<Self> {
            static SETUP: OnceLock<Mutex<Setup>> = OnceLock::new();
            SETUP.get_or_init(|| Mutex::new(Setup { id: 0 }))
        }

        fn id(&mut self) -> usize {
            self.id += 1;
            self.id
        }

        fn create_logs_folder(&mut self) -> String {
            let dir = String::from("unit_tests_tmp_logs");
            if !Path::new(&dir).exists() {
                fs::create_dir(&dir).unwrap();
            }

            let name = format!("{}/unit_tests_{}.log", dir, self.id());
            let _ = File::create(&name).unwrap();

            name
        }
    }

    fn check_results(log_filename: &str, expected: &[&str]) {
        let content = fs::read_to_string(&log_filename).unwrap();

        let results = content.lines().collect::<Vec<&str>>();
        let n = results.len();

        if n != expected.len() {
            assert!(
                false,
                "number of results isn't good : expected {}, have {}; log: {log_filename}",
                expected.len(),
                n,
            );
        }

        for i in 0..n {
            assert_eq!(results[i], expected[i]);
        }
    }

    #[test]
    fn negate() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().create_logs_folder();

        let mut vm = Vm::default();
        vm.set_log_file(&file_name);
        let index = vm.chunk.add_constant(Value::Number(1.2));
        vm.chunk.write(OpCode::OpConstant(index), 0);
        vm.chunk.write(OpCode::OpNegate, 0);
        vm.chunk.write(OpCode::OpReturn, 0);

        vm.run();
        check_results(&file_name, &vec!["-1.2"]);
    }

    #[test]
    fn add() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().create_logs_folder();

        let mut vm = Vm::default();
        vm.set_log_file(&file_name);

        let mut index = vm.chunk.add_constant(Value::Number(2.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        index = vm.chunk.add_constant(Value::Number(3.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        vm.chunk.write(OpCode::OpAdd, 0);
        vm.chunk.write(OpCode::OpReturn, 0);

        vm.run();
        check_results(&file_name, &vec!["5"]);
    }

    #[test]
    fn subtract() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().create_logs_folder();

        let mut vm = Vm::default();
        vm.set_log_file(&file_name);

        let mut index = vm.chunk.add_constant(Value::Number(2.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        index = vm.chunk.add_constant(Value::Number(3.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        vm.chunk.write(OpCode::OpSubtract, 0);
        vm.chunk.write(OpCode::OpReturn, 0);

        vm.run();
        check_results(&file_name, &vec!["-1"]);
    }

    #[test]
    fn multiply() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().create_logs_folder();

        let mut vm = Vm::default();
        vm.set_log_file(&file_name);

        let mut index = vm.chunk.add_constant(Value::Number(2.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        index = vm.chunk.add_constant(Value::Number(3.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        vm.chunk.write(OpCode::OpMultiply, 0);
        vm.chunk.write(OpCode::OpReturn, 0);

        vm.run();
        check_results(&file_name, &vec!["6"]);
    }

    #[test]
    fn divide() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().create_logs_folder();

        let mut vm = Vm::default();
        vm.set_log_file(&file_name);

        let mut index = vm.chunk.add_constant(Value::Number(1.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        index = vm.chunk.add_constant(Value::Number(2.));
        vm.chunk.write(OpCode::OpConstant(index), 0);

        vm.chunk.write(OpCode::OpDivide, 0);
        vm.chunk.write(OpCode::OpReturn, 0);

        vm.run();
        check_results(&file_name, &vec!["0.5"]);
    }
}
