use crate::{interpreter::Interpreter, token::LiteralType};
use std::time::{SystemTime, UNIX_EPOCH};

pub trait LoxCallable {
    fn call(&self, interpreter: &Interpreter, arguments: &[LiteralType]) -> LiteralType;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Clock(Clock),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {}

impl LoxCallable for Function {
    fn call(&self, interpreter: &Interpreter, arguments: &[LiteralType]) -> LiteralType {
        todo!()
    }

    fn arity(&self) -> usize {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clock {}

impl LoxCallable for Clock {
    fn call(&self, _interpreter: &Interpreter, _arguments: &[LiteralType]) -> LiteralType {
        let now = SystemTime::now();
        let since_epoch = now.duration_since(UNIX_EPOCH).unwrap();
        LiteralType::FloatLiteral(since_epoch.as_millis() as f64 / 1000.0)
    }

    fn arity(&self) -> usize {
        0
    }
}
