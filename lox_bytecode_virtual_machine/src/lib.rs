#![forbid(unsafe_code)]

use std::fs;
use std::io::{stdin, stdout, Write};
use std::{env, process};

use lox_error::LoxError;
use vm::Vm;

mod chunk;
mod compiler;
mod lox_error;
mod scanner;
mod token;
mod vm;

pub struct Config {
    filename: String,
    pub prompt_mode: bool,
}

impl Config {
    pub fn new(mut args: env::Args) -> Result<Config, &'static str> {
        args.next(); // ignores application name

        let filename = match args.next() {
            Some(filename) => filename,
            None => {
                return Ok(Config {
                    filename: String::new(),
                    prompt_mode: true,
                })
            }
        };

        if args.next().is_some() {
            return Err("Too many arguments");
        }

        Ok(Config {
            filename,
            prompt_mode: false,
        })
    }
}

#[derive(Default)]
pub struct Lox {
    vm: Vm,
}

impl Lox {
    fn handle_lox_error(&self, error: LoxError) {
        eprintln!("{error}");

        match error {
            LoxError::Compiler(_) => process::exit(65), // C sysexits.h EX_DATAERR data format error
            LoxError::Runtime(_) => process::exit(70), // C sysexits.h EX_SOFTWARE internal software error
        }
    }

    pub fn run_file(&mut self, config: Config) {
        let source = fs::read_to_string(config.filename).unwrap_or_else(|err| {
            eprintln!("{err}");
            process::exit(66); // C sysexits.h EX_NOINPUT error
        });

        if let Err(e) = self.run(&source) {
            self.handle_lox_error(e);
        }
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!(">");
            stdout().flush().unwrap();
            let mut line = String::new();
            let _ = stdin().read_line(&mut line);

            if let Err(e) = self.run(line.trim()) {
                self.handle_lox_error(e);
            }
        }
    }

    fn run(&mut self, source: &str) -> Result<(), LoxError> {
        println!("run");
        println!("{source}");

        self.vm.run(source)?;

        Ok(())
    }
}
