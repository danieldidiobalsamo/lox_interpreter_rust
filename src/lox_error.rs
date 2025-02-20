use thiserror::Error;

use crate::parser::ParserError;
use crate::scanner::ScannerErrorList;
use crate::token::LiteralType;

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Scanning error: {0}")]
    Scanner(ScannerErrorList),
    #[error("Parsing error: {0}")]
    Parser(ParserError),
    #[error("Runtime error: {0}")]
    Interpreter(String),
}

#[derive(Debug, Clone)]
pub enum Exit {
    Return(LiteralType),
    Error(String),
}
