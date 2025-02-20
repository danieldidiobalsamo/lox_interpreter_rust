#![forbid(unsafe_code)]

use std::error::Error;
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
    pub fn run_file(&mut self, config: Config) -> Result<(), Box<dyn Error>> {
        let source = fs::read_to_string(config.filename)?;
        self.run(&source);

        Ok(())
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!(">");
            stdout().flush().unwrap();
            let mut line = String::new();
            let _ = stdin().read_line(&mut line);

            self.run(line.trim());
        }
    }

    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(source);
        match scanner.scan_tokens() {
            Err(err) => eprintln!("{err}"),
            Ok(tokens) => {
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(statements) => {
                        let mut resolver = Resolver::new(&mut self.interpreter);
                        if let Err(err) = resolver.resolve(&statements) {
                            eprintln!("{err}");
                            process::exit(70); // C sysexits.h EX_SOFTWARE internal software error
                        }

                        match self.interpreter.interpret(&statements) {
                            Ok(_) => (),
                            Err(err) => {
                                eprintln!("{err}");
                                process::exit(70); // C sysexits.h EX_SOFTWARE internal software error
                            }
                        }
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        process::exit(65); // C sysexits.h EX_DATAERR data format error
                    }
                }
            }
        };
    }
}
