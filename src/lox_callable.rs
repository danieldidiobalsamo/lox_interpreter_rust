use crate::{
    environment::Environment,
    interpreter::Interpreter,
    stmt::{self, Exit},
    token::{LiteralType, Token},
};
use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::HashMap,
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
    LoxClass(LoxClass),
    LoxInstance(Rc<RefCell<LoxInstance>>),
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

        match interpreter.execute_block(&self.declaration.body, env) {
            Ok(_) => Ok(LiteralType::NilLiteral),
            Err(e) => match e {
                Exit::Return(literal_type) => Ok(literal_type),
                Exit::Error(s) => Err(Exit::Error(s)),
            },
        }
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

#[derive(Debug, PartialEq, Clone)]
pub struct LoxClass {
    pub name: String,
}

impl LoxCallable for LoxClass {
    fn call(
        &mut self,
        _interpreter: &mut Interpreter,
        _arguments: &[LiteralType],
    ) -> Result<LiteralType, Exit> {
        let instance = Rc::new(RefCell::new(LoxInstance {
            class: Rc::new(self.clone().into()),
            fields: HashMap::new(),
        }));

        Ok(LiteralType::Callable(Callable::LoxInstance(Rc::clone(
            &instance,
        ))))
    }

    fn arity(&self) -> usize {
        0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoxInstance {
    pub class: Rc<RefCell<LoxClass>>,
    pub fields: HashMap<String, LiteralType>,
}

impl LoxInstance {
    pub fn get(&self, name: &Token) -> Result<LiteralType, String> {
        match self.fields.get(&name.get_lexeme()) {
            Some(val) => Ok(val.clone()),
            None => Err(format!("Undefined property: {}", name.get_lexeme())),
        }
    }
    pub fn set(&mut self, name: &Token, value: &LiteralType) {
        // lox allows creating new fields on instances, if the key doesn't exists then it's created.

        self.fields.insert(name.get_lexeme(), value.clone());
    }
}
