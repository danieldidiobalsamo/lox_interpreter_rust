use std::error::Error;
use std::fs;
use std::io::{stdin, stdout, Write};
use std::{env, process};

pub mod ast_printer;
pub mod expr;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod stmt;
pub mod token;

use ast_printer::AstPrinter;
use interpreter::Interpreter;
use parser::Parser;

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

pub fn run_file(config: Config) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(config.filename)?;
    run(&source);

    Ok(())
}

pub fn run_prompt() {
    loop {
        print!(">");
        stdout().flush().unwrap();
        let mut line = String::new();
        let _ = stdin().read_line(&mut line);

        run(line.trim());
    }
}

fn run(source: &str) {
    let mut scanner = Scanner::new(source);
    match scanner.scan_tokens() {
        Err(err) => eprintln!("{err}"),
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(statements) => {
                    let mut interpreter = Interpreter::default();

                    match interpreter.interpret(&statements) {
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

pub fn print_error(line: usize, msg: &str) {
    eprintln!("{}", build_error_message(line, "", msg));
}

pub fn build_error_message(line: usize, location: &str, msg: &str) -> String {
    format!("[line {line}] Error {location}: {msg}")
}
