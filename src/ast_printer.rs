use crate::expr::{self, AstVisitor, Expr};

struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expr: Expr) -> String {
        expr.accept(self)
    }
}

impl AstVisitor<String> for AstPrinter {
    fn visit_literal(&mut self, expr: &expr::Literal) -> String {
        expr.value.to_string()
    }

    fn visit_binary_expr(&mut self, expr: &expr::Binary) -> String {
        format!(
            "({} {} {})",
            &expr.operator.get_lexeme(),
            expr.left.accept(self),
            expr.right.accept(self),
        )
    }

    fn visit_grouping_expr(&mut self, expr: &expr::Grouping) -> String {
        format!("(group {})", expr.expression.accept(self))
    }

    fn visit_unary_expr(&mut self, expr: &expr::Unary) -> String {
        format!(
            "({} {})",
            &expr.operator.get_lexeme(),
            expr.right.accept(self),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::{Binary, Grouping, Literal, Unary};
    use crate::token::{LiteralType, Token, TokenType};

    use super::*;

    #[test]
    fn binary() {
        let expr = Expr::Binary(Binary {
            left: Box::new(Expr::Unary(Unary {
                operator: Token::Simple(TokenType::Minus, "-".to_string(), 1),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(123.),
                })),
            })),
            operator: Token::Simple(TokenType::Star, "*".to_string(), 1),
            right: Box::new(Expr::Grouping(Grouping {
                expression: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(45.67),
                })),
            })),
        });

        let mut printer = AstPrinter;

        let expected = "(* (- 123) (group 45.67))";

        assert_eq!(printer.print(expr), expected);
    }
}
