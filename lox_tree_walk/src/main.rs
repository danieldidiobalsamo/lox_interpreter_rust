use lox_tree_walk::{Config, Lox};
use std::env;
use std::process;

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("{err}");
        println!("Usage : lox_tree_walk [script]");
        process::exit(64); // C sysexits.h EX_USAGE error
    });

    let mut lox = Lox::default();

    if config.prompt_mode {
        lox.run_prompt();
    } else {
        lox.run_file(config);
    }
}
