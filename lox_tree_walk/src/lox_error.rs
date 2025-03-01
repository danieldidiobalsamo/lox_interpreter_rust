use thiserror::Error;

use crate::interpreter::RuntimeError;
use crate::parser::ParserError;
use crate::resolver::ResolverError;
use crate::scanner::ScannerErrorList;
use crate::token::LiteralType;

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Scanning error: {0}")]
    Scanner(ScannerErrorList),
    #[error("Parsing error: {0}")]
    Parser(ParserError),
    #[error("Resolver error: {0}")]
    Resolver(ResolverError),
    #[error("Runtime error: {0}")]
    Runtime(RuntimeError),
}

#[derive(Debug, Clone)]
pub enum Exit {
    Return(LiteralType),
    Error(RuntimeError),
}
