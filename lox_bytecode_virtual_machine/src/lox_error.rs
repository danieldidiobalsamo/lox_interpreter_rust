use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error)]
pub enum LoxError {
    #[error("Compiler error: {0}")]
    Compiler(CompilerError),
    #[error("Runtime error: {0}")]
    Runtime(RuntimeError),
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum CompilerError {
    #[error("Instructor pointer out of chunk bounds: {ip}")]
    IpOutOfBounds { ip: usize },
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum RuntimeError {
    #[error("Can't divide by zero")]
    ZeroDivision,
    #[error("Undefined constant index {index}")]
    UndefinedConstant { index: u8 },
    #[error("VM stack is empty, failed to pop value.")]
    EmptyStack,
}
