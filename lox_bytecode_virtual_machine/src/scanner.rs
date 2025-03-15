use thiserror::Error;

use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum ScannerError {
    #[error("Unexpected character")]
    UnexpectedCharacter { c: char },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scanner {
    source: Vec<char>,
    start: usize,
    last: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            last: source.len(),
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, ScannerError> {
        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof));
        }

        Err(ScannerError::UnexpectedCharacter {
            c: self.get_current_char(),
        })
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.start, self.current - self.start, self.line)
    }

    fn get_current_char(&self) -> char {
        self.source[self.current]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_code(source: &str) -> Result<Token, ScannerError> {
        let mut scanner = Scanner::new(source);
        scanner.scan_token()
    }

    #[test]
    fn unexpected_char() {
        assert!(scan_code("~").is_err());
    }

    #[test]
    fn eof() {
        assert_eq!(scan_code("").unwrap(), Token::new(TokenType::Eof, 0, 0, 1));
    }
}
