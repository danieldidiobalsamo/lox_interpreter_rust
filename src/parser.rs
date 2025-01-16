use crate::{
    expr::{Binary, Expr, Grouping, Literal, Unary},
    stmt::{Expression, Print, Stmt},
    token::{LiteralType, Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement()?);
        }

        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_token_type(&[TokenType::Print]) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let val = self.expression()?;
        let _ = self.consume(TokenType::Semicolon, "Expect ';' after value.")?;

        return Ok(Stmt::Print(Print {
            expression: Box::new(val),
        }));
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        let _ = self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        return Ok(Stmt::Expression(Expression {
            expression: Box::new(expr),
        }));
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.match_token_type(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.match_token_type(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_token_type(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_token_type(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_token_type(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            return Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(right),
            }));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token_type(&[TokenType::False]) {
            return Ok(Expr::Literal(Literal {
                value: LiteralType::BoolLiteral(false),
            }));
        }

        if self.match_token_type(&[TokenType::True]) {
            return Ok(Expr::Literal(Literal {
                value: LiteralType::BoolLiteral(true),
            }));
        }

        if self.match_token_type(&[TokenType::Nil]) {
            return Ok(Expr::Literal(Literal {
                value: LiteralType::NilLiteral,
            }));
        }

        if self.match_token_type(&[TokenType::Number, TokenType::String]) {
            if let Token::Literal(_, _, literal, _) = self.previous() {
                return Ok(Expr::Literal(Literal {
                    value: literal.clone(),
                }));
            }
        }

        if self.match_token_type(&[TokenType::LeftParen]) {
            let expr = self.expression()?;

            let _ = self.consume(TokenType::RightParen, "Expect ')' after expression.");

            return Ok(Expr::Grouping(Grouping {
                expression: Box::new(expr),
            }));
        }

        Err(self.build_error(&self.peek().clone(), "Expect expression."))
    }

    fn build_error(&mut self, token: &Token, msg: &str) -> String {
        format!("Error with token {token:?} : {msg}\n")
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<Token, String> {
        if self.check(&token_type) {
            return Ok((*self.advance()).clone());
        }

        Err(self.build_error(&self.peek().clone(), msg))
    }

    fn match_token_type(&mut self, tokens_type: &[TokenType]) -> bool {
        for token_type in tokens_type {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        *self.peek().get_type() == TokenType::Eof
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        *self.peek().get_type() == *token_type
    }

    fn synchronize(&mut self) {
        let _ = self.advance();

        while !self.is_at_end() {
            if *self.previous().get_type() == TokenType::Semicolon {
                return;
            }

            match *self.peek().get_type() {
                TokenType::Class
                | TokenType::For
                | TokenType::Fun
                | TokenType::If
                | TokenType::Print
                | TokenType::Return
                | TokenType::Var
                | TokenType::While => return,
                _ => {
                    let _ = self.advance();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;

    use super::*;

    fn get_tokens(code: &str) -> Vec<Token> {
        let mut scanner = Scanner::new(code);
        scanner.scan_tokens().unwrap()
    }

    #[test]
    fn unary_minus() {
        let mut parser = Parser::new(get_tokens("-2;"));

        let expected = vec![Stmt::Expression(Expression {
            expression: Box::new(Expr::Unary(Unary {
                operator: Token::Simple(TokenType::Minus, "-".to_string(), 1),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(2.),
                })),
            })),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn simple_add() {
        let mut parser = Parser::new(get_tokens("2+2;"));

        let expected = vec![Stmt::Expression(Expression {
            expression: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(2.),
                })),
                operator: Token::Simple(TokenType::Plus, "+".to_string(), 1),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(2.),
                })),
            })),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn equality() {
        let mut parser = Parser::new(get_tokens("1 == 2 == 3;"));

        let expected = vec![Stmt::Expression(Expression {
            expression: Box::new(Expr::Binary(Binary {
                operator: Token::Simple(TokenType::EqualEqual, "==".to_string(), 1),
                left: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::Literal(Literal {
                        value: LiteralType::FloatLiteral(1.),
                    })),
                    operator: Token::Simple(TokenType::EqualEqual, "==".to_string(), 1),
                    right: Box::new(Expr::Literal(Literal {
                        value: LiteralType::FloatLiteral(2.),
                    })),
                })),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(3.),
                })),
            })),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn grouping() {
        let mut parser = Parser::new(get_tokens("(2*2);"));

        let expected = vec![Stmt::Expression(Expression {
            expression: Box::new(Expr::Grouping(Grouping {
                expression: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::Literal(Literal {
                        value: LiteralType::FloatLiteral(2.),
                    })),
                    operator: Token::Simple(TokenType::Star, "*".to_string(), 1),
                    right: Box::new(Expr::Literal(Literal {
                        value: LiteralType::FloatLiteral(2.),
                    })),
                })),
            })),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn incomplete_binary() {
        let mut parser = Parser::new(get_tokens("2+;"));
        assert!(parser.parse().is_err());
    }
}
