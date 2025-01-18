use lox_interpreter_rust::{Config, Lox};
use std::env;
use std::process;

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("{err}");
        println!("Usage : lox_interpreter_rust [script]");
        process::exit(64); // C sysexits.h EX_USAGE error
    });

    let mut lox = Lox::default();

    match config.prompt_mode {
        true => lox.run_prompt(),
        false => {
            if let Err(err) = lox.run_file(config) {
                eprintln!("{err}");
                process::exit(66); // C sysexits.h EX_NOINPUT error
            }
        }
    }
}
