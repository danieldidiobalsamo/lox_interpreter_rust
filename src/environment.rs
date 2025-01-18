use std::collections::HashMap;

use crate::token::{LiteralType, Token};

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, LiteralType>,
    enclosing: Box<Option<Environment>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Box::new(None),
        }
    }
}

impl Environment {
    pub fn new(enclosing: Environment) -> Self {
        Self {
            enclosing: Box::new(Some(enclosing)),
            ..Default::default()
        }
    }

    pub fn define(&mut self, name: &str, value: &LiteralType) {
        let _ = self.values.insert(name.to_string(), value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<LiteralType, String> {
        let var_name = name.get_lexeme();

        match self.values.get(&var_name) {
            Some(val) => Ok(val.clone()),
            None => match *self.enclosing {
                Some(ref env) => env.get(name),
                None => Err(format!("Undefined variable: '{var_name}'.")),
            },
        }
    }

    pub fn assign(&mut self, name: &Token, value: &LiteralType) -> Result<(), String> {
        let var_name = name.get_lexeme();

        match self.values.get(&var_name) {
            Some(_) => {
                *self.values.get_mut(&var_name).unwrap() = value.clone();
                Ok(())
            }
            None => match *self.enclosing {
                Some(ref mut env) => env.assign(name, value),
                None => Err(format!("Undefined variable: '{var_name}'.")),
            },
        }
    }
}
