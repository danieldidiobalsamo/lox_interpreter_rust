use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    interpreter::RuntimeError,
    lox_error::Exit,
    token::{LiteralType, Token, TokenType},
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    values: HashMap<String, LiteralType>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
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

    pub fn get(&self, name: &Token) -> Result<LiteralType, Exit> {
        let var_name = name.get_lexeme();

        match self.values.get(&var_name) {
            Some(val) => Ok(val.clone()),
            None => match &self.enclosing {
                Some(env) => env.borrow().get(name),
                None => Err(Exit::Error(RuntimeError::UndefinedVariable {
                    lexeme: name.get_lexeme(),
                    line: name.get_line(),
                    var_name,
                })),
            },
        }
    }

    pub fn assign(&mut self, name: &Token, value: &LiteralType) -> Result<(), Exit> {
        let var_name = name.get_lexeme();

        match self.values.get(&var_name) {
            Some(_) => {
                *self.values.get_mut(&var_name).unwrap() = value.clone();
                Ok(())
            }
            None => match &self.enclosing {
                Some(env) => env.borrow_mut().assign(name, value),
                None => Err(Exit::Error(RuntimeError::UndefinedVariable {
                    lexeme: name.get_lexeme(),
                    line: name.get_line(),
                    var_name,
                })),
            },
        }
    }

    pub fn get_at_str(
        &self,
        distance: usize,
        name: &str,
        line: usize,
    ) -> Result<LiteralType, Exit> {
        let token = Token::Literal(
            TokenType::String,
            name.to_owned(),
            LiteralType::String(name.to_owned()),
            line,
        );

        self.get_at(distance, &token)
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<LiteralType, Exit> {
        if distance == 0 {
            self.get(name)
        } else if let Some(e) = self.enclosing.as_ref() {
            e.borrow().get_at(distance - 1, name)
        } else {
            Err(Exit::Error(RuntimeError::UndefinedVariableInEnclosing {
                lexeme: name.get_lexeme(),
                line: name.get_line(),
                var_name: name.get_lexeme(),
            }))
        }
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: &Token,
        value: &LiteralType,
    ) -> Result<(), Exit> {
        if distance == 0 {
            self.define(&name.get_lexeme(), value);
        } else if let Some(e) = self.enclosing.as_ref() {
            e.borrow_mut().assign_at(distance - 1, name, value)?;
        } else {
            return Err(Exit::Error(RuntimeError::UndefinedVariableInEnclosing {
                lexeme: name.get_lexeme(),
                line: name.get_line(),
                var_name: name.get_lexeme(),
            }));
        }

        Ok(())
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
        map.insert("a".to_owned(), LiteralType::Float(1.));

        let outer = Environment::new(Some(Rc::new(RefCell::new(Environment {
            values: map,
            enclosing: None,
        }))));

        let mut inner = Environment::default();
        inner.define("a", &LiteralType::Float(4.));

        let token = Token::Simple(TokenType::Identifier, "a".to_owned(), 1);

        assert_eq!(inner.get(&token).unwrap(), LiteralType::Float(4.));
        assert_eq!(outer.get(&token).unwrap(), LiteralType::Float(1.));
    }
}
