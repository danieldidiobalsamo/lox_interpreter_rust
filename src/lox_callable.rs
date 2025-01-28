use crate::{
    environment::Environment,
    interpreter::Interpreter,
    stmt::{self},
    token::LiteralType,
};
use std::{
    borrow::BorrowMut,
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

pub trait LoxCallable {
    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: &[LiteralType],
    ) -> Result<LiteralType, String>;
    fn arity(&self) -> usize;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Clock(Clock),
    Function(Function),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub declaration: Box<stmt::Function>,
}

impl LoxCallable for Function {
    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: &[LiteralType],
    ) -> Result<LiteralType, String> {
        let mut env = Environment::new(Some(Rc::new(RefCell::clone(&interpreter.globals))));

        for i in 0..self.declaration.params.len() {
            env.borrow_mut()
                .define(&self.declaration.params[i].get_lexeme(), &arguments[i]);
        }

        interpreter.execute_block(&self.declaration.body, env)?;

        Ok(LiteralType::NilLiteral)
    }

    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Clock {}

impl LoxCallable for Clock {
    fn call(
        &mut self,
        _interpreter: &mut Interpreter,
        _arguments: &[LiteralType],
    ) -> Result<LiteralType, String> {
        let now = SystemTime::now();
        let since_epoch = now.duration_since(UNIX_EPOCH).map_err(|e| e.to_string())?;
        Ok(LiteralType::FloatLiteral(
            since_epoch.as_millis() as f64 / 1000.0,
        ))
    }

    fn arity(&self) -> usize {
        0
    }
}
