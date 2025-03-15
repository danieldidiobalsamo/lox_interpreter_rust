use crate::{
    scanner::Scanner,
    token::{Token, TokenType},
};

#[derive(Debug, Clone, PartialEq)]
struct Compiler {
    scanner: Scanner,
}

impl Compiler {
    pub fn compile(&mut self, source: &str) {
        let mut line = -1isize;

        loop {
            // TODO: remove unwrap()
            let token = self.scanner.scan_token().unwrap();

            if token.line() as isize != line {
                println!("{}", token.line());
                line = token.line() as isize;
            } else {
                println!(" | ");
            }

            println!("{token:?}");

            if *token.token_type() == TokenType::Eof {
                break;
            }
        }
    }
}
