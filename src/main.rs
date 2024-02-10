use lox_interpreter_rust::Config;
use std::env;
use std::process;

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("{err}");
        println!("Usage : lox_interpreter_rust [script]");
        process::exit(64); // C sysexits.h EX_USAGE error
    });

    match config.prompt_mode {
        true => lox_interpreter_rust::run_prompt(),
        false => {
            if let Err(err) = lox_interpreter_rust::run_file(config) {
                eprintln!("{err}");
                process::exit(66); // C sysexits.h EX_NOINPUT error
            }
        }
    }
}
