use std::fs::OpenOptions;
use std::io::Write;

use crate::environment::Environment;
use crate::expr::{AstVisitor, Expr, Variable};
use crate::stmt::{Stmt, StmtVisitor};
use crate::token::{LiteralType, Token, TokenType};

pub struct Interpreter {
    env: Environment,
    pub write_log: bool,
    log_file: String,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            env: Environment::default(),
            write_log: false,
            log_file: "unit_tests.log".to_owned(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute_block(&mut self, statements: &[Box<Stmt>], env: Environment) -> Result<(), String> {
        let previous = self.env.clone();

        self.env = env;

        for statement in statements {
            self.execute(statement)?;
        }

        self.env = previous;

        Ok(())
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), String> {
        statement.accept(self)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LiteralType, String> {
        expr.accept(self)
    }

    fn is_truthy(&self, literal_type: &LiteralType) -> bool {
        match literal_type {
            LiteralType::BoolLiteral(b) => *b,
            LiteralType::NilLiteral => false,
            _ => true,
        }
    }

    fn check_number_operand(&self, operand: &LiteralType, operator: &Token) -> Result<f64, String> {
        let error = format!("{operator:?} : Operand must be a number");

        operand.get_float().map_err(|_| error)
    }

    fn check_number_operands(
        &self,
        left: &LiteralType,
        right: &LiteralType,
        operator: &Token,
    ) -> Result<(f64, f64), String> {
        Ok((
            self.check_number_operand(left, operator)?,
            self.check_number_operand(right, operator)?,
        ))
    }

    fn check_string_operand(
        &self,
        operand: &LiteralType,
        operator: &Token,
    ) -> Result<String, String> {
        let error = format!("{operator:?} : Operand must be a string");

        operand.get_string().map_err(|_| error)
    }

    fn check_string_operands(
        &self,
        left: &LiteralType,
        right: &LiteralType,
        operator: &Token,
    ) -> Result<(String, String), String> {
        Ok((
            self.check_string_operand(left, operator)?,
            self.check_string_operand(right, operator)?,
        ))
    }
}

impl AstVisitor<Result<LiteralType, String>> for Interpreter {
    fn visit_literal(&mut self, expr: &crate::expr::Literal) -> Result<LiteralType, String> {
        Ok(expr.value.clone())
    }

    fn visit_binary_expr(&mut self, expr: &crate::expr::Binary) -> Result<LiteralType, String> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.get_type() {
            TokenType::Minus => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;
                Ok(LiteralType::FloatLiteral(l - r))
            }
            TokenType::Slash => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;

                if r == 0. {
                    return Err("Can't divide by 0".to_owned());
                }

                Ok(LiteralType::FloatLiteral(l / r))
            }
            TokenType::Star => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;
                Ok(LiteralType::FloatLiteral(l * r))
            }
            TokenType::Plus => match self.check_number_operands(&left, &right, &expr.operator) {
                Ok((l, r)) => Ok(LiteralType::FloatLiteral(l + r)),
                Err(e_1) => match self.check_string_operands(&left, &right, &expr.operator) {
                    Ok((l, r)) => Ok(LiteralType::StringLiteral(l + &r)),
                    Err(e_2) => Err(format!("{e_1}\nOr {e_2}")),
                },
            },
            TokenType::Greater => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;
                Ok(LiteralType::BoolLiteral(l > r))
            }
            TokenType::GreaterEqual => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;
                Ok(LiteralType::BoolLiteral(l >= r))
            }
            TokenType::Less => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;
                Ok(LiteralType::BoolLiteral(l < r))
            }
            TokenType::LessEqual => {
                let (l, r) = self.check_number_operands(&left, &right, &expr.operator)?;
                Ok(LiteralType::BoolLiteral(l <= r))
            }
            TokenType::EqualEqual => Ok(LiteralType::BoolLiteral(left == right)),
            TokenType::BangEqual => Ok(LiteralType::BoolLiteral(left != right)),
            _ => panic!("not a binary operator : {:?}", expr.operator),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &crate::expr::Grouping) -> Result<LiteralType, String> {
        self.evaluate(&expr.expression)
    }

    fn visit_unary_expr(&mut self, expr: &crate::expr::Unary) -> Result<LiteralType, String> {
        let right = self.evaluate(&expr.right)?;

        match expr.operator.get_type() {
            TokenType::Bang => Ok(LiteralType::BoolLiteral(!self.is_truthy(&right))),
            TokenType::Minus => {
                let v = self.check_number_operand(&right, &expr.operator)?;
                Ok(LiteralType::FloatLiteral(-v))
            }
            _ => Ok(LiteralType::NilLiteral),
        }
    }

    fn visit_variable_expr(&mut self, expr: &Variable) -> Result<LiteralType, String> {
        return self.env.get(&expr.name);
    }

    fn visit_assign_expr(&mut self, expr: &crate::expr::Assign) -> Result<LiteralType, String> {
        let value = self.evaluate(&expr.value)?;
        self.env.assign(&expr.name, &value)?;

        Ok(value)
    }
}

impl StmtVisitor<Result<(), String>> for Interpreter {
    fn visit_expression(&mut self, expr: &crate::stmt::Expression) -> Result<(), String> {
        self.evaluate(&expr.expression)?;

        Ok(())
    }

    fn visit_print(&mut self, expr: &crate::stmt::Print) -> Result<(), String> {
        let value = self.evaluate(&expr.expression)?;
        println!("{}", value.to_string());

        if self.write_log {
            let mut file = OpenOptions::new()
                .append(true)
                .open(&self.log_file)
                .unwrap();
            file.write((value.to_string() + "\n").as_bytes()).unwrap();
        }

        Ok(())
    }

    fn visit_var(&mut self, stmt: &crate::stmt::Var) -> Result<(), String> {
        let value = match *stmt.initializer {
            Some(ref init) => self.evaluate(init)?,
            None => LiteralType::NilLiteral,
        };

        self.env.define(&stmt.name.get_lexeme(), &value);

        Ok(())
    }

    fn visit_block(&mut self, stmt: &crate::stmt::Block) -> Result<(), String> {
        self.execute_block(&stmt.statements, Environment::new(self.env.clone()))?;
        Ok(())
    }

    fn visit_if(&mut self, stmt: &crate::stmt::If) -> Result<(), String> {
        let cond = &self.evaluate(&stmt.condition)?;

        if self.is_truthy(cond) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(ref else_branch) = *stmt.else_branch {
            self.execute(&else_branch)?;
        }

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

    use crate::parser::Parser;
    use crate::scanner::Scanner;

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

        fn get_statements(&self, code: &str) -> Vec<Stmt> {
            let mut scanner = Scanner::new(code);
            let tokens = scanner.scan_tokens().unwrap();

            let mut parser = Parser::new(tokens);
            parser.parse().unwrap()
        }

        fn interpret_code(&mut self, code: &str) -> Result<String, String> {
            let dir = String::from("unit_tests_tmp_logs");
            if !Path::new(&dir).exists() {
                fs::create_dir(&dir).unwrap();
            }

            let name = format!("{}/unit_tests_{}.log", dir, self.id());
            let mut i = Interpreter {
                write_log: true,
                log_file: name.clone(),
                ..Default::default()
            };

            let _ = File::create(&name).unwrap();

            let statements = self.get_statements(code);
            i.interpret(&statements)?;

            Ok(name)
        }
    }

    pub fn check_results(log_filename: &str, expected: &[&str]) {
        let content = fs::read_to_string(&log_filename).unwrap();

        if content.is_empty() {
            assert!(expected == vec![""]);
        }

        for (i, line) in content.lines().enumerate() {
            assert_eq!(line, expected[i]);
        }
    }

    #[test]
    fn is_truthy() {
        let i = Interpreter::default();
        assert_eq!(i.is_truthy(&LiteralType::NilLiteral), false);
        assert_eq!(i.is_truthy(&LiteralType::FloatLiteral(5.0)), true);
        assert_eq!(
            i.is_truthy(&LiteralType::StringLiteral("abc".to_owned())),
            true
        );
        assert_eq!(i.is_truthy(&LiteralType::BoolLiteral(true)), true);
        assert_eq!(i.is_truthy(&LiteralType::BoolLiteral(false)), false);
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

        assert_eq!(
            setup.lock().unwrap().interpret_code("2.5/0;"),
            Err("Can't divide by 0".to_owned())
        );
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
        check_results(&file_name, &vec![""]);
    }
}
