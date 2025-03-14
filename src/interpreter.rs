use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[cfg(test)]
use std::{fs::OpenOptions, io::Write};

use thiserror::Error;

use crate::environment::Environment;
use crate::expr::{AstVisitor, Expr, Variable};
use crate::lox_callable::{Callable, Clock, Function, LoxCallable, LoxClass};
use crate::lox_error::{Exit, LoxError};
use crate::stmt::{Stmt, StmtVisitor};
use crate::token::{LiteralType, Token, TokenType};

#[derive(Debug, Clone, Error)]
pub enum RuntimeError {
    #[error("{lexeme} at line {line} : Operand must be a number")]
    OperandNumber { lexeme: String, line: usize },
    #[error("{lexeme} at line {line} : Operand must be a String")]
    OperandString { lexeme: String, line: usize },
    #[error("{lexeme} at line {line} : Can't divide by zero")]
    ZeroDivision { lexeme: String, line: usize },
    #[error("{lexeme} at line {line} : Expected {expected} arguments but got {got}")]
    BadNbArgs {
        lexeme: String,
        line: usize,
        expected: usize,
        got: usize,
    },
    #[error("{lexeme} at line {line} : Undefined variable: '{var_name}'.")]
    UndefinedVariable {
        lexeme: String,
        line: usize,
        var_name: String,
    },
    #[error("{lexeme} at line {line} : no enclosing env containing {var_name}")]
    UndefinedVariableInEnclosing {
        lexeme: String,
        line: usize,
        var_name: String,
    },
    #[error("{lexeme} at line {line} : {operator:?} is not a logical operator")]
    NotLogicalOperator {
        lexeme: String,
        line: usize,
        operator: String,
    },
    #[error("Can only call functions and classes")]
    OnlyCallFunctionsAndClasses,
    #[error("{lexeme} at line {line} : Only instances have properties")]
    OnlyInstancesHaveProperties { lexeme: String, line: usize },
    #[error("{lexeme} at line {line} : Only instances have fields")]
    OnlyInstancesHaveFields { lexeme: String, line: usize },
    #[error("{lexeme} at line {line} : Undefined property {property}")]
    UndefinedProperty {
        lexeme: String,
        line: usize,
        property: String,
    },

    #[error("{lexeme} at line {line} : Bad super usage")]
    BadSuperUsage { lexeme: String, line: usize },

    #[error("Super class must be a class")]
    SuperClassMustBeClass,
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub env: Rc<RefCell<Environment>>,
    pub globals: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
    #[cfg(test)]
    log_file: String,
}

impl Default for Interpreter {
    fn default() -> Self {
        let globals = Rc::new(RefCell::new(Environment::default()));

        globals
            .borrow_mut()
            .define("clock", &LiteralType::Callable(Callable::Clock(Clock {})));

        Self {
            globals: Rc::clone(&globals),
            locals: HashMap::new(),
            env: Rc::clone(&globals),
            #[cfg(test)]
            log_file: "".to_owned(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), LoxError> {
        for statement in statements {
            if let Err(Exit::Error(e)) = self.execute(statement) {
                return Err(LoxError::Runtime(e));
            }
        }

        Ok(())
    }

    pub fn execute_block(&mut self, statements: &Vec<Stmt>, env: Environment) -> Result<(), Exit> {
        let previous = Rc::clone(&self.env);

        self.env = Rc::new(RefCell::new(env.clone()));

        for statement in statements {
            if let Err(exit) = self.execute(statement) {
                self.env = previous;
                return Err(exit);
            }
        }

        self.env = previous;

        Ok(())
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), Exit> {
        statement.accept(self)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LiteralType, Exit> {
        expr.accept(self)
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }

    fn is_truthy(&self, literal_type: &LiteralType) -> bool {
        match literal_type {
            LiteralType::Bool(b) => *b,
            LiteralType::Nil => false,
            _ => true,
        }
    }

    fn check_number_operand(
        &self,
        operand: &LiteralType,
        operator: &Token,
    ) -> Result<f64, RuntimeError> {
        operand
            .get_float()
            .map_err(|_| RuntimeError::OperandNumber {
                lexeme: operator.get_lexeme(),
                line: operator.get_line(),
            })
    }

    fn check_number_operands(
        &self,
        left: &LiteralType,
        right: &LiteralType,
        operator: &Token,
    ) -> Result<(f64, f64), RuntimeError> {
        Ok((
            self.check_number_operand(left, operator)?,
            self.check_number_operand(right, operator)?,
        ))
    }

    fn check_string_operand(
        &self,
        operand: &LiteralType,
        operator: &Token,
    ) -> Result<String, RuntimeError> {
        operand
            .get_string()
            .map_err(|_| RuntimeError::OperandString {
                lexeme: operator.get_lexeme(),
                line: operator.get_line(),
            })
    }

    fn check_string_operands(
        &self,
        left: &LiteralType,
        right: &LiteralType,
        operator: &Token,
    ) -> Result<(String, String), RuntimeError> {
        Ok((
            self.check_string_operand(left, operator)?,
            self.check_string_operand(right, operator)?,
        ))
    }

    fn look_up_variable(&mut self, name: &Token, expr: &Expr) -> Result<LiteralType, Exit> {
        match self.locals.get(expr) {
            Some(distance) => self.env.borrow().get_at(*distance, name),
            None => self.globals.borrow().get(name),
        }
    }
}

impl AstVisitor<Result<LiteralType, Exit>> for Interpreter {
    fn visit_literal(&mut self, expr: &crate::expr::Literal) -> Result<LiteralType, Exit> {
        Ok(expr.value.clone())
    }

    fn visit_binary_expr(&mut self, expr: &crate::expr::Binary) -> Result<LiteralType, Exit> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.get_type() {
            TokenType::Minus => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Float(l - r))
            }
            TokenType::Slash => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;

                if r == 0. {
                    return Err(Exit::Error(RuntimeError::ZeroDivision {
                        lexeme: expr.operator.get_lexeme(),
                        line: expr.operator.get_line(),
                    }));
                }

                Ok(LiteralType::Float(l / r))
            }
            TokenType::Star => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Float(l * r))
            }
            TokenType::Plus => match self.check_number_operands(&left, &right, &expr.operator) {
                Ok((l, r)) => Ok(LiteralType::Float(l + r)),
                Err(_) => match self.check_string_operands(&left, &right, &expr.operator) {
                    Ok((l, r)) => Ok(LiteralType::String(l + &r)),
                    Err(e_2) => Err(Exit::Error(e_2)),
                },
            },
            TokenType::Greater => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Bool(l > r))
            }
            TokenType::GreaterEqual => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Bool(l >= r))
            }
            TokenType::Less => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Bool(l < r))
            }
            TokenType::LessEqual => {
                let (l, r) = self
                    .check_number_operands(&left, &right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Bool(l <= r))
            }
            TokenType::EqualEqual => Ok(LiteralType::Bool(left == right)),
            TokenType::BangEqual => Ok(LiteralType::Bool(left != right)),
            _ => panic!("not a binary operator : {:?}", expr.operator),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &crate::expr::Grouping) -> Result<LiteralType, Exit> {
        self.evaluate(&expr.expression)
    }

    fn visit_unary_expr(&mut self, expr: &crate::expr::Unary) -> Result<LiteralType, Exit> {
        let right = self.evaluate(&expr.right)?;

        match expr.operator.get_type() {
            TokenType::Bang => Ok(LiteralType::Bool(!self.is_truthy(&right))),
            TokenType::Minus => {
                let v = self
                    .check_number_operand(&right, &expr.operator)
                    .map_err(Exit::Error)?;
                Ok(LiteralType::Float(-v))
            }
            _ => Ok(LiteralType::Nil),
        }
    }

    fn visit_variable_expr(&mut self, expr: &Variable) -> Result<LiteralType, Exit> {
        self.look_up_variable(&expr.name, &Expr::Variable(expr.clone()))
    }

    fn visit_assign_expr(&mut self, expr: &crate::expr::Assign) -> Result<LiteralType, Exit> {
        let value = self.evaluate(&expr.value)?;

        if let Some(distance) = self.locals.get(&Expr::Assign(expr.clone())) {
            self.env
                .borrow_mut()
                .assign_at(*distance, &expr.name, &value)?;
        } else {
            self.globals.borrow_mut().assign(&expr.name, &value)?;
        }

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &crate::expr::Logical) -> Result<LiteralType, Exit> {
        let left = self.evaluate(&expr.left)?;

        // check if left operand is false to check if we can short-circuit
        match expr.operator.get_type() {
            TokenType::Or => {
                if self.is_truthy(&left) {
                    return Ok(left);
                }
            }
            TokenType::And => {
                if !self.is_truthy(&left) {
                    return Ok(left);
                }
            }
            _ => {
                return Err(Exit::Error(RuntimeError::NotLogicalOperator {
                    lexeme: expr.operator.get_lexeme(),
                    line: expr.operator.get_line(),
                    operator: expr.operator.get_lexeme(),
                }));
            }
        };

        self.evaluate(&expr.right)
    }

    fn visit_call_expr(&mut self, expr: &crate::expr::Call) -> Result<LiteralType, Exit> {
        let callee = self.evaluate(&expr.callee)?;

        let mut arguments = Vec::new();

        for arg in &expr.arguments {
            arguments.push(self.evaluate(arg)?);
        }

        if let LiteralType::Callable(c) = callee {
            match c {
                Callable::Function(mut f) => {
                    if arguments.len() != f.arity() {
                        Err(Exit::Error(RuntimeError::BadNbArgs {
                            lexeme: expr.paren.get_lexeme(),
                            line: expr.paren.get_line(),
                            expected: f.arity(),
                            got: arguments.len(),
                        }))
                    } else {
                        Ok(f.call(self, &arguments)?)
                    }
                }
                Callable::Clock(mut c) => Ok(c.call(self, &arguments)?),
                Callable::LoxClass(mut lox_class) => Ok(lox_class.call(self, &[])?),
                _ => Err(Exit::Error(RuntimeError::OnlyCallFunctionsAndClasses)),
            }
        } else {
            Err(Exit::Error(RuntimeError::OnlyCallFunctionsAndClasses))
        }
    }

    fn visit_get_expr(&mut self, expr: &crate::expr::Get) -> Result<LiteralType, Exit> {
        let object = self.evaluate(&expr.object)?;

        if let LiteralType::Callable(Callable::LoxInstance(object)) = object {
            return object.borrow().get(&expr.name).map_err(Exit::Error);
        }

        Err(Exit::Error(RuntimeError::OnlyInstancesHaveProperties {
            lexeme: expr.name.get_lexeme(),
            line: expr.name.get_line(),
        }))
    }

    fn visit_set_expr(&mut self, expr: &crate::expr::Set) -> Result<LiteralType, Exit> {
        let object = self.evaluate(&expr.object)?;

        if let LiteralType::Callable(Callable::LoxInstance(instance)) = object {
            let value = self.evaluate(&expr.value)?;
            instance.borrow_mut().set(&expr.name, &value);

            return Ok(value);
        }
        Err(Exit::Error(RuntimeError::OnlyInstancesHaveFields {
            lexeme: expr.name.get_lexeme(),
            line: expr.name.get_line(),
        }))
    }

    fn visit_this_expr(&mut self, expr: &crate::expr::This) -> Result<LiteralType, Exit> {
        self.look_up_variable(&expr.keyword, &Expr::This(expr.clone()))
    }

    fn visit_super_expr(&mut self, expr: &crate::expr::Super) -> Result<LiteralType, Exit> {
        let sup = &Expr::Super(expr.clone());
        let Some(distance) = self.locals.get(sup) else {
            panic!("Super hasn't been resolved correctly {}", sup);
        };

        let super_class =
            self.env
                .borrow()
                .get_at_str(*distance, "super", expr.method.get_line())?;
        let object = self
            .env
            .borrow()
            .get_at_str(*distance - 1, "this", expr.method.get_line())?;

        match (&super_class, object) {
            (
                LiteralType::Callable(Callable::LoxClass(c)),
                LiteralType::Callable(Callable::LoxInstance(instance)),
            ) => match c.find_method(&expr.method.get_lexeme()) {
                Some(m) => Ok(LiteralType::Callable(Callable::Function(m.bind(instance)))),

                None => Err(Exit::Error(RuntimeError::UndefinedProperty {
                    lexeme: expr.method.get_lexeme(),
                    line: expr.method.get_line(),
                    property: expr.method.get_lexeme(),
                })),
            },

            _ => Err(Exit::Error(RuntimeError::BadSuperUsage {
                lexeme: expr.method.get_lexeme(),
                line: expr.method.get_line(),
            })),
        }
    }
}

impl StmtVisitor<Result<(), Exit>> for Interpreter {
    fn visit_expression(&mut self, expr: &crate::stmt::Expression) -> Result<(), Exit> {
        let _ = self.evaluate(&expr.expression)?;

        Ok(())
    }

    fn visit_print(&mut self, expr: &crate::stmt::Print) -> Result<(), Exit> {
        let value = self.evaluate(&expr.expression)?;

        println!("{}", value);

        #[cfg(test)]
        {
            let mut file = OpenOptions::new()
                .append(true)
                .open(&self.log_file)
                .unwrap();
            file.write_all((value.to_string() + "\n").as_bytes())
                .unwrap();
        }

        Ok(())
    }

    fn visit_var(&mut self, stmt: &crate::stmt::Var) -> Result<(), Exit> {
        let value = match *stmt.initializer {
            Some(ref init) => self.evaluate(init)?,
            None => LiteralType::Nil,
        };

        self.env
            .borrow_mut()
            .define(&stmt.name.get_lexeme(), &value);

        Ok(())
    }

    fn visit_block(&mut self, stmt: &crate::stmt::Block) -> Result<(), Exit> {
        self.execute_block(
            &stmt.statements,
            Environment::new(Some(Rc::clone(&self.env))),
        )
    }

    fn visit_if(&mut self, stmt: &crate::stmt::If) -> Result<(), Exit> {
        let cond = self.evaluate(&stmt.condition)?;

        if self.is_truthy(&cond) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(ref else_branch) = *stmt.else_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }

    fn visit_while(&mut self, stmt: &crate::stmt::While) -> Result<(), Exit> {
        let mut cond = self.evaluate(&stmt.condition)?;

        while self.is_truthy(&cond) {
            let _ = self.execute(&stmt.body);

            cond = self.evaluate(&stmt.condition)?;
        }

        Ok(())
    }

    fn visit_function(&mut self, stmt: &crate::stmt::Function) -> Result<(), Exit> {
        let function = Callable::Function(Function {
            declaration: Box::new(stmt.clone()),
            closure: Rc::clone(&self.env),
            is_initializer: false,
        });

        self.env
            .borrow_mut()
            .define(&stmt.name.get_lexeme(), &LiteralType::Callable(function));

        Ok(())
    }

    fn visit_return(&mut self, stmt: &crate::stmt::Return) -> Result<(), Exit> {
        if let Some(ref val) = *stmt.value {
            let evaluated = self.evaluate(val)?;

            // return Err to go to where the function call began, instead of propagating through Ok()
            // and continue the execution
            return Err(Exit::Return(evaluated));
        }

        Ok(())
    }

    fn visit_class(&mut self, stmt: &crate::stmt::Class) -> Result<(), Exit> {
        let super_class = if let Some(ref super_class) = *stmt.super_class {
            if let LiteralType::Callable(Callable::LoxClass(c)) = self.evaluate(super_class)? {
                Some(c)
            } else {
                return Err(Exit::Error(RuntimeError::SuperClassMustBeClass));
            }
        } else {
            None
        };

        self.env
            .borrow_mut()
            .define(&stmt.name.get_lexeme(), &LiteralType::Nil);

        if stmt.super_class.is_some() {
            self.env = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&self.env)))));
            self.env.borrow_mut().define(
                "super",
                &LiteralType::Callable(Callable::LoxClass(super_class.clone().unwrap())),
            );
        }

        let mut methods = HashMap::new();

        for method in &stmt.methods {
            if let Stmt::Function(m) = &method {
                let function = Function {
                    declaration: Box::new(m.clone()),
                    closure: Rc::clone(&self.env),
                    is_initializer: m.name.get_lexeme() == "init",
                };

                methods.insert(m.name.get_lexeme(), function);
            }
        }

        let class = Callable::LoxClass(LoxClass {
            name: stmt.name.get_lexeme(),
            methods,
            super_class: Box::new(super_class),
        });

        if stmt.super_class.is_some() {
            let enclosing = Rc::clone(self.env.borrow_mut().enclosing.as_ref().unwrap());
            self.env = enclosing;
        }

        self.env
            .borrow_mut()
            .assign(&stmt.name, &LiteralType::Callable(class))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs::{self, File},
        path::Path,
    };

    use super::*;

    use crate::{lox_error::LoxError, scanner::Scanner};
    use crate::{parser::Parser, resolver::Resolver};

    struct Setup {
        id: usize,
    }
    use std::sync::{Mutex, OnceLock};

    impl Setup {
        pub fn new() -> &'static Mutex<Self> {
            static SETUP: OnceLock<Mutex<Setup>> = OnceLock::new();
            SETUP.get_or_init(|| Mutex::new(Setup { id: 0 }))
        }

        pub fn id(&mut self) -> usize {
            self.id += 1;
            self.id
        }

        fn get_statements(&self, code: &str) -> Result<Vec<Stmt>, LoxError> {
            let mut scanner = Scanner::new(code);
            let tokens = scanner.scan_tokens().unwrap();

            let mut parser = Parser::new(tokens);
            parser.parse()
        }

        fn interpret_code(&mut self, code: &str) -> Result<String, LoxError> {
            let dir = String::from("unit_tests_tmp_logs");
            if !Path::new(&dir).exists() {
                fs::create_dir(&dir).unwrap();
            }

            let name = format!("{}/unit_tests_{}.log", dir, self.id());
            let mut i = Interpreter {
                log_file: name.clone(),
                ..Default::default()
            };

            let _ = File::create(&name).unwrap();

            let statements = self.get_statements(code)?;

            let mut resolver = Resolver::new(&mut i);
            resolver.resolve(&statements)?;
            i.interpret(&statements)?;

            Ok(name)
        }
    }

    pub fn check_results(log_filename: &str, expected: &[&str]) {
        let content = fs::read_to_string(&log_filename).unwrap();

        let results = content.lines().collect::<Vec<&str>>();
        let n = results.len();

        if n != expected.len() {
            assert!(
                false,
                "number of results isn't good : expected {}, have {}; log: {log_filename}",
                expected.len(),
                n,
            );
        }

        for i in 0..n {
            assert_eq!(results[i], expected[i]);
        }
    }

    #[test]
    fn is_truthy() {
        let i = Interpreter::default();
        assert_eq!(i.is_truthy(&LiteralType::Nil), false);
        assert_eq!(i.is_truthy(&LiteralType::Float(5.0)), true);
        assert_eq!(i.is_truthy(&LiteralType::String("abc".to_owned())), true);
        assert_eq!(i.is_truthy(&LiteralType::Bool(true)), true);
        assert_eq!(i.is_truthy(&LiteralType::Bool(false)), false);
    }

    #[test]
    fn bang() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print !true;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print !false;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print !!true;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup.lock().unwrap().interpret_code("print !nil;").unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup.lock().unwrap().interpret_code("print !5.0;").unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print !\"abc\";")
            .unwrap();
        check_results(&file_name, &vec!["false"]);
    }

    #[test]
    fn unary_minus() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print -2;").unwrap();
        check_results(&file_name, &vec!["-2"]);

        let file_name = setup.lock().unwrap().interpret_code("print --2;").unwrap();
        check_results(&file_name, &vec!["2"]);

        let file_name = setup.lock().unwrap().interpret_code("print ---2;").unwrap();
        check_results(&file_name, &vec!["-2"]);
    }

    #[test]
    fn binary_minus() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2-2;").unwrap();
        check_results(&file_name, &vec!["0"]);

        let file_name = setup.lock().unwrap().interpret_code("print -2-2;").unwrap();
        check_results(&file_name, &vec!["-4"]);
    }

    #[test]
    fn grouping() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print -(-2-2)-(1*2);")
            .unwrap();
        check_results(&file_name, &vec!["2"]);
    }

    #[test]
    fn slash() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2/2;").unwrap();
        check_results(&file_name, &vec!["1"]);

        let file_name = setup.lock().unwrap().interpret_code("print 1/2;").unwrap();
        check_results(&file_name, &vec!["0.5"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 10.5/2;")
            .unwrap();
        check_results(&file_name, &vec!["5.25"]);

        assert!(setup.lock().unwrap().interpret_code("2.5/0;").is_err());
    }

    #[test]
    fn star() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2*2;").unwrap();
        check_results(&file_name, &vec!["4"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 0.5*2;")
            .unwrap();
        check_results(&file_name, &vec!["1"]);
    }

    #[test]
    fn binary_plus() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2+2;").unwrap();
        check_results(&file_name, &vec!["4"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print \"Hello \"+\"world\"+\"!\";")
            .unwrap();
        check_results(&file_name, &vec!["Hello world!"]);

        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("print true + false;")
            .is_err());
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("print true + 1;")
            .is_err());
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("print 1 + \"abc\";")
            .is_err());
    }

    #[test]
    fn greater() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2>2;").unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup.lock().unwrap().interpret_code("print 3>2;").unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup.lock().unwrap().interpret_code("print 2>3;").unwrap();
        check_results(&file_name, &vec!["false"]);

        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("print false>2;")
            .is_err());
    }

    #[test]
    fn greater_equal() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2>=2;").unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup.lock().unwrap().interpret_code("print 3>=2;").unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup.lock().unwrap().interpret_code("print 2>=3;").unwrap();
        check_results(&file_name, &vec!["false"]);

        assert!(setup.lock().unwrap().interpret_code("false>=2;").is_err());
    }

    #[test]
    fn less() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2<2;").unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup.lock().unwrap().interpret_code("print 3<2;").unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup.lock().unwrap().interpret_code("print 2<3;").unwrap();
        check_results(&file_name, &vec!["true"]);

        assert!(setup.lock().unwrap().interpret_code("false<2;").is_err());
    }

    #[test]
    fn less_equal() {
        let setup = Setup::new();

        let file_name = setup.lock().unwrap().interpret_code("print 2<=2;").unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup.lock().unwrap().interpret_code("print 3<=2;").unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup.lock().unwrap().interpret_code("print 2<=3;").unwrap();
        check_results(&file_name, &vec!["true"]);

        assert!(setup.lock().unwrap().interpret_code("false<=2;").is_err());
    }

    #[test]
    fn equalequal() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 2 == 2;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print \"2\" == 2;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 2 == false;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print false == false;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 2 == 2 == 3;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);
    }

    #[test]
    fn bang_equal() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 2 != 2;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print \"2\" != 2;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print 2 != false;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print false != false;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);
    }

    #[test]
    fn var() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=1; var b=2;print a+b;")
            .unwrap();
        check_results(&file_name, &vec!["3"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a; print a;")
            .unwrap();
        check_results(&file_name, &vec!["nil"]);
    }

    #[test]
    fn assignment() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=1; var b=a;print a+b;")
            .unwrap();
        check_results(&file_name, &vec!["2"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=\"a\"; var b=\"b\"; var c=a+b; print c;")
            .unwrap();
        check_results(&file_name, &vec!["ab"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=0; var b=1; var c = a = b; print c;")
            .unwrap();
        check_results(&file_name, &vec!["1"]);
    }

    #[test]
    fn block() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=1;{print a;var a=2; print a;a = a+1;}print a;")
            .unwrap();
        check_results(&file_name, &vec!["1", "2", "1"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=1;{a=a+1;print a;}print a;")
            .unwrap();
        check_results(&file_name, &vec!["2", "2"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=-5; if (a < 0){a=1;}print a;")
            .unwrap();
        check_results(&file_name, &vec!["1"]);
    }

    #[test]
    fn if_statement() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=true; if (a){print \"if\";}else{print \"else\";}")
            .unwrap();
        check_results(&file_name, &vec!["if"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=false; if (a){print \"if\";}else{print \"else\";}")
            .unwrap();
        check_results(&file_name, &vec!["else"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=1; if (a == 2){print \"if\";}")
            .unwrap();
        check_results(&file_name, &vec![]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=1; var b=3; if (a+b >=4 and b <= 3){print \"if\";}")
            .unwrap();
        check_results(&file_name, &vec!["if"]);
    }

    #[test]
    fn logical() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=((true or false) and true); print a;")
            .unwrap();
        check_results(&file_name, &vec!["true"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var a=((true or false) and (true and false)); print a;")
            .unwrap();
        check_results(&file_name, &vec!["false"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print \"hi\" or 2;")
            .unwrap();
        check_results(&file_name, &vec!["hi"]);

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("print nil or \"yes\";")
            .unwrap();
        check_results(&file_name, &vec!["yes"]);
    }

    #[test]
    fn while_statement() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("var i=0; while (i < 5){print i; i = i + 1;}")
            .unwrap();
        check_results(&file_name, &vec!["0", "1", "2", "3", "4"]);
    }

    #[test]
    fn for_statement() {
        let setup = Setup::new();

        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("for(var i=0; i < 5; i=i+1){print i;}")
            .unwrap();
        check_results(&file_name, &vec!["0", "1", "2", "3", "4"]);
    }

    #[test]
    fn hi() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/hi.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["hi"]);
    }

    #[test]
    fn function_hello_world() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/hello_world.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["Hi, Dear User!"]);
    }

    #[test]
    fn function_add_pair() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/simple.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["4"]);
    }

    #[test]
    fn unreachable_code() {
        let setup = Setup::new();
        let file_name = setup
            .lock()
            .unwrap()
            .interpret_code("fun test(){return;print \"unreachable\";}")
            .unwrap();
        check_results(&file_name, &vec![]);
    }

    #[test]
    fn function_fibonacci() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/fibonacci.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["55"]);
    }

    #[test]
    fn function_optimized_fibonacci() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/optimized_fibonacci.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["190392490709135"]);
    }

    #[test]
    fn closures() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/closures.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["1", "2"]);
    }

    #[test]
    fn closures_scope() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/scope.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["global", "global"]);
    }

    #[test]
    fn forbidden_ref_var_in_its_initializer() {
        let setup = Setup::new();
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("var a=\"outer\";{var a=a;}")
            .is_err());
    }

    #[test]
    fn same_var_name_same_local_scope() {
        // same variable name in the same local scope (global would be ok) is an error
        let setup = Setup::new();
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("fun bad(){var a=1;var a=2;}")
            .is_err());
    }

    #[test]
    fn invalid_return() {
        // return outside a function declaration is an error
        let setup = Setup::new();
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("return \"hello\";")
            .is_err());
    }

    #[test]
    fn simple_class() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/simple_class.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(
            &file_name,
            &vec!["Player", "Player instance", "nobody", "I'm taking 'book'"],
        );
    }

    #[test]
    fn add_method_class() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/add_method_class.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["I'm taking 'book'"]);
    }

    #[test]
    fn refer_to_method_class() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/refer_to_method_class.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["hello world!"]);
    }

    #[test]
    fn this() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/this.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["hello nobody"]);
    }

    #[test]
    fn callback_with_this() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/callback_with_this.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["Player instance"]);
    }

    #[test]
    fn this_outside_method() {
        // 'this' outside a method is an error
        let setup = Setup::new();
        assert!(setup.lock().unwrap().interpret_code("print this;").is_err());
    }

    #[test]
    fn this_outside_class_method() {
        // 'this' outside in a function (not method) is an error
        let setup = Setup::new();

        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("fun bad(){print this;}")
            .is_err());
    }

    #[test]
    fn class_init() {
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/init.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(
            &file_name,
            &vec!["Player instance", "Player instance", "Player instance"],
        );
    }

    #[test]
    fn return_value_within_init() {
        // return a value within a constructor is an error
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/return_value_within_init.lox").unwrap();

        assert!(setup.lock().unwrap().interpret_code(&code).is_err());
    }

    #[test]
    fn return_within_init() {
        // return without a value inside a constructor is valid
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/return_within_init.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["Player instance"]);
    }

    #[test]
    fn inheritance_simple() {
        // return without a value inside a constructor is valid
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/inheritance_simple.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(
            &file_name,
            &vec!["hello from Entity!", "hello from Player!"],
        );
    }

    #[test]
    fn inheritance_harder() {
        // return without a value inside a constructor is valid
        let setup = Setup::new();
        let code = fs::read_to_string("./test_lox_scripts/inheritance_harder.lox").unwrap();

        let file_name = setup.lock().unwrap().interpret_code(&code).unwrap();
        check_results(&file_name, &vec!["A method"]);
    }

    #[test]
    fn superclass_must_be_a_class() {
        let setup = Setup::new();
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("var a;class B < a{}")
            .is_err());
    }

    #[test]
    fn super_outside_method() {
        let setup = Setup::new();
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("print super;")
            .is_err());

        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("super.test();")
            .is_err());
    }

    #[test]
    fn super_with_class_not_having_superclass() {
        let setup = Setup::new();
        assert!(setup
            .lock()
            .unwrap()
            .interpret_code("class Player{test(){super.test();}}")
            .is_err());
    }
}
