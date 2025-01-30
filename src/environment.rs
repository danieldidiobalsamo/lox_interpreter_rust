use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::token::{LiteralType, Token};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    values: HashMap<String, LiteralType>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        match enclosing {
            None => Self::default(),
            Some(env) => Self {
                enclosing: Some(env),
                ..Default::default()
            },
        }
    }

    pub fn define(&mut self, name: &str, value: &LiteralType) {
        let _ = self.values.insert(name.to_string(), value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<LiteralType, String> {
        let var_name = name.get_lexeme();

        match self.values.get(&var_name) {
            Some(val) => Ok(val.clone()),
            None => match &self.enclosing {
                Some(env) => env.borrow().get(name),
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
            None => match &self.enclosing {
                Some(env) => env.borrow_mut().assign(name, value),
                None => Err(format!("Undefined variable: '{var_name}'.")),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn env_nested() {
        // this test is to check this kind of nested blocks
        // var i=1; while (i < 5){print i;i+=1;}

        let mut map = HashMap::new();
        map.insert("a".to_owned(), LiteralType::FloatLiteral(1.));

        let outer = Environment::new(Some(Rc::new(RefCell::new(Environment {
            values: map,
            enclosing: None,
        }))));

        let mut inner = Environment::default();
        inner.define("a", &LiteralType::FloatLiteral(4.));

        let token = Token::Simple(TokenType::Identifier, "a".to_owned(), 1);

        assert_eq!(inner.get(&token).unwrap(), LiteralType::FloatLiteral(4.));
        assert_eq!(outer.get(&token).unwrap(), LiteralType::FloatLiteral(1.));
    }
}
