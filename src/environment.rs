use std::collections::HashMap;

use crate::token::{LiteralType, Token};

#[derive(Default, Debug)]
pub struct Environment {
    values: HashMap<String, LiteralType>,
}

impl Environment {
    pub fn define(&mut self, name: &str, value: &LiteralType) {
        let _ = self.values.insert(name.to_string(), value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<LiteralType, String> {
        let var_name = name.get_lexeme();

        match self.values.get(&var_name) {
            Some(val) => Ok(val.clone()),
            None => Err(format!("Undefined variable: '{var_name}'.")),
        }
    }
}
