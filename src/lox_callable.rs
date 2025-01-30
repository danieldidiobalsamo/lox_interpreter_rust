use crate::{
    environment::Environment,
    interpreter::Interpreter,
    stmt::{self, Exit},
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
    ) -> Result<LiteralType, Exit>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone)]
pub enum Callable {
    Clock(Clock),
    Function(Function),
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub declaration: Box<stmt::Function>,
    pub closure: Rc<RefCell<Environment>>,
}

impl LoxCallable for Function {
    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: &[LiteralType],
    ) -> Result<LiteralType, Exit> {
        let mut env = Environment::new(Some(Rc::clone(&self.closure)));

        for i in 0..self.declaration.params.len() {
            env.borrow_mut()
                .define(&self.declaration.params[i].get_lexeme(), &arguments[i]);
        }

        return match interpreter.execute_block(&self.declaration.body, env) {
            Ok(_) => Ok(LiteralType::NilLiteral),
            Err(e) => match e {
                Exit::Return(literal_type) => Ok(literal_type),
                Exit::Error(s) => Err(Exit::Error(s)),
            },
        };
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
    ) -> Result<LiteralType, Exit> {
        let now = SystemTime::now();
        let since_epoch = now
            .duration_since(UNIX_EPOCH)
            .map_err(|e| Exit::Error(e.to_string()))?;

        Ok(LiteralType::FloatLiteral(
            since_epoch.as_millis() as f64 / 1000.0,
        ))
    }

    fn arity(&self) -> usize {
        0
    }
}
