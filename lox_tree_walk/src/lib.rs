#![forbid(unsafe_code)]

use std::fs;
use std::io::{stdin, stdout, Write};
use std::{env, process};

mod environment;
mod expr;
mod interpreter;
mod lox_callable;
mod lox_error;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod token;

use interpreter::Interpreter;
use lox_error::LoxError;
use parser::Parser;
use resolver::Resolver;

use crate::scanner::Scanner;

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
    interpreter: Interpreter,
}

impl Lox {
    fn handle_lox_error(&self, error: LoxError) {
        eprintln!("{error}");

        match error {
            LoxError::Scanner(_) => process::exit(65), // C sysexits.h EX_DATAERR data format error
            LoxError::Parser(_) => process::exit(65),  // C sysexits.h EX_DATAERR data format error
            LoxError::Resolver(_) => process::exit(70), // C sysexits.h EX_SOFTWARE internal software error
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
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;

        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;

        let mut resolver = Resolver::new(&mut self.interpreter);
        resolver.resolve(&statements)?;

        self.interpreter.interpret(&statements)?;

        Ok(())
    }
}
