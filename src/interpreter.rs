use crate::expr::{AstVisitor, Expr};
use crate::token::{LiteralType, Token, TokenType};

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&mut self, expr: &Expr) -> Result<LiteralType, String> {
        self.evaluate(expr)
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
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::Parser;
    use crate::scanner::Scanner;

    use super::*;

    fn get_expr(code: &str) -> Expr {
        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan_tokens().unwrap();

        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    fn interpret_code(code: &str) -> Result<LiteralType, String> {
        let mut i = Interpreter;
        let expr = get_expr(code);

        i.interpret(&expr)
    }

    #[test]
    fn is_truthy() {
        let i = Interpreter;
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
        assert_eq!(
            interpret_code("!true").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("!false").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("!!true").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("!nil").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("!5.0").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("!\"abc\"").unwrap(),
            LiteralType::BoolLiteral(false)
        );
    }

    #[test]
    fn unary_minus() {
        assert_eq!(
            interpret_code("-2").unwrap(),
            LiteralType::FloatLiteral(-2.)
        );
        assert_eq!(
            interpret_code("--2").unwrap(),
            LiteralType::FloatLiteral(2.)
        );
        assert_eq!(
            interpret_code("---2").unwrap(),
            LiteralType::FloatLiteral(-2.)
        );
    }

    #[test]
    fn binary_minus() {
        assert_eq!(
            interpret_code("2-2").unwrap(),
            LiteralType::FloatLiteral(0.)
        );
        assert_eq!(
            interpret_code("-2-2").unwrap(),
            LiteralType::FloatLiteral(-4.)
        );
    }

    #[test]
    fn grouping() {
        assert_eq!(
            interpret_code("-(-2-2)-(1*2)").unwrap(),
            LiteralType::FloatLiteral(2.)
        );
    }

    #[test]
    fn slash() {
        assert_eq!(
            interpret_code("2/2").unwrap(),
            LiteralType::FloatLiteral(1.)
        );

        assert_eq!(
            interpret_code("1/2").unwrap(),
            LiteralType::FloatLiteral(0.5)
        );

        assert_eq!(
            interpret_code("10.5/2").unwrap(),
            LiteralType::FloatLiteral(5.25)
        );

        assert_eq!(interpret_code("2.5/0"), Err("Can't divide by 0".to_owned()));
    }

    #[test]
    fn star() {
        assert_eq!(
            interpret_code("2*2").unwrap(),
            LiteralType::FloatLiteral(4.)
        );

        assert_eq!(
            interpret_code("0.5*2").unwrap(),
            LiteralType::FloatLiteral(1.)
        );
    }

    #[test]
    fn binary_plus() {
        assert_eq!(
            interpret_code("2+2").unwrap(),
            LiteralType::FloatLiteral(4.)
        );

        assert_eq!(
            interpret_code("\"Hello \"+\"world\"+\"!\"").unwrap(),
            LiteralType::StringLiteral("Hello world!".to_owned())
        );

        assert!(interpret_code("true + false").is_err(),);
        assert!(interpret_code("true + 1").is_err(),);
        assert!(interpret_code("true + \"abc\"").is_err(),);
    }

    #[test]
    fn greater() {
        assert_eq!(
            interpret_code("2>2").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("3>2").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("2>3").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert!(interpret_code("false>2").is_err());
    }

    #[test]
    fn greater_equal() {
        assert_eq!(
            interpret_code("2>=2").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("3>=2").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("2>=3").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert!(interpret_code("false>2").is_err());
    }

    #[test]
    fn less() {
        assert_eq!(
            interpret_code("2<2").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("3<2").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("2<3").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert!(interpret_code("false<2").is_err());
    }

    #[test]
    fn less_equal() {
        assert_eq!(
            interpret_code("2<=2").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("3<=2").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("2<=3").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert!(interpret_code("false<=2").is_err());
    }

    #[test]
    fn equalequal() {
        assert_eq!(
            interpret_code("2 == 2").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("\"2\" == 2").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("2 == false").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("false == false").unwrap(),
            LiteralType::BoolLiteral(true)
        );
    }

    #[test]
    fn bang_equal() {
        assert_eq!(
            interpret_code("2 != 2").unwrap(),
            LiteralType::BoolLiteral(false)
        );

        assert_eq!(
            interpret_code("\"2\" != 2").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("2 != false").unwrap(),
            LiteralType::BoolLiteral(true)
        );

        assert_eq!(
            interpret_code("false != false").unwrap(),
            LiteralType::BoolLiteral(false)
        );
    }
}
