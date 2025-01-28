use crate::{
    expr::{Assign, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable},
    stmt::{self, Block, Expression, If, Print, Stmt, Var, While},
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
            match self.declaration() {
                Ok(d) => statements.push(d),
                Err(e) => {
                    self.synchronize();
                    return Err(e);
                }
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, String> {
        if self.match_token_type(&[TokenType::Fun]) {
            return self.function();
        }

        if self.match_token_type(&[TokenType::Var]) {
            return match self.var_declaration() {
                Ok(stmt) => Ok(stmt),
                Err(e) => {
                    self.synchronize();
                    return Err(e);
                }
            };
        }

        return self.statement();
    }
    fn function(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, &format!("Expect function name."))?;
        let _ = self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;

        let mut params = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err("Can't have more than 255 parameters.".to_owned());
                }

                params.push(self.consume(TokenType::Identifier, "Expect parameter name")?);

                if !self.match_token_type(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let _ = self.consume(TokenType::RightParen, "Expect ')' after parameter name.")?;
        let _ = self.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;

        let body = self.block()?;

        Ok(Stmt::Function(stmt::Function { name, params, body }))
    }

    fn var_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.match_token_type(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        let _ = self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        Ok(Stmt::Var(Var {
            name,
            initializer: Box::new(initializer),
        }))
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_token_type(&[TokenType::For]) {
            return self.for_statement();
        }

        if self.match_token_type(&[TokenType::If]) {
            return self.if_statement();
        }

        if self.match_token_type(&[TokenType::Print]) {
            return self.print_statement();
        }

        if self.match_token_type(&[TokenType::While]) {
            return self.while_statement();
        }

        if self.match_token_type(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block(Block {
                statements: self.block()?,
            }));
        }

        self.expression_statement()
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_token_type(&[TokenType::Semicolon]) {
            None
        } else if self.match_token_type(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let mut condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expect ';' after for loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(Block {
                statements: vec![
                    Box::new(body),
                    Box::new(Stmt::Expression(Expression {
                        expression: Box::new(inc),
                    })),
                ],
            })
        }

        if condition.is_none() {
            condition = Some(Expr::Literal(Literal {
                value: LiteralType::BoolLiteral(true),
            }));
        }

        body = Stmt::While(While {
            condition: Box::new(condition.unwrap()),
            body: Box::new(body),
        });

        if let Some(init) = initializer {
            body = Stmt::Block(Block {
                statements: vec![Box::new(init), Box::new(body)],
            })
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;

        let body = self.statement()?;

        Ok(Stmt::While(While {
            condition: Box::new(condition),
            body: Box::new(body),
        }))
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        let _ = self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        let _ = self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_token_type(&[TokenType::Else]) {
            // note: in lox, else statement is bound to the nearest if that precedes it
            Some(self.statement()?)
        } else {
            None
        };

        return Ok(Stmt::If(If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }));
    }

    fn block(&mut self) -> Result<Vec<Box<Stmt>>, String> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(Box::new(self.declaration()?));
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block.")?;

        Ok(statements)
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
        self.assigment()
    }

    fn assigment(&mut self) -> Result<Expr, String> {
        let expr = self.or()?;

        if self.match_token_type(&[TokenType::Equal]) {
            let value = self.assigment()?;

            if let Expr::Variable(var) = expr {
                return Ok(Expr::Assign(Assign {
                    name: var.name,
                    value: Box::new(value),
                }));
            } else {
                return Err("Invalid assignment target.".to_owned());
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.match_token_type(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;

            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;

        while self.match_token_type(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;

            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
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

        self.call()
    }

    fn call(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token_type(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break Ok(expr);
            }
        }
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(String::from("Can't have more than 255 arguments."));
                }

                arguments.push(Box::new(self.expression()?));

                if !self.match_token_type(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call(Call {
            callee: Box::new(callee),
            paren,
            arguments,
        }))
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

        if self.match_token_type(&[TokenType::Identifier]) {
            return Ok(Expr::Variable(Variable {
                name: self.previous().clone(),
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
    use crate::{expr::Logical, scanner::Scanner};

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

    #[test]
    fn var_with_identifier() {
        let mut parser = Parser::new(get_tokens("var a = 1;"));

        let expected = vec![Stmt::Var(Var {
            name: Token::Simple(TokenType::Identifier, "a".to_owned(), 1),
            initializer: Box::new(Some(Expr::Literal(Literal {
                value: LiteralType::FloatLiteral(1.),
            }))),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn var_without_identifier() {
        let mut parser = Parser::new(get_tokens("var a;"));

        let expected = vec![Stmt::Var(Var {
            name: Token::Simple(TokenType::Identifier, "a".to_owned(), 1),
            initializer: Box::new(None),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn assigment() {
        let mut parser = Parser::new(get_tokens("var a=1;var b=a;"));

        let expected = vec![
            Stmt::Var(Var {
                name: Token::Simple(TokenType::Identifier, "a".to_owned(), 1),
                initializer: Box::new(Some(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(1.),
                }))),
            }),
            Stmt::Var(Var {
                name: Token::Simple(TokenType::Identifier, "b".to_owned(), 1),
                initializer: Box::new(Some(Expr::Variable(Variable {
                    name: Token::Simple(TokenType::Identifier, "a".to_owned(), 1),
                }))),
            }),
        ];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn block() {
        let mut parser = Parser::new(get_tokens("{var a=1;var b=2;}"));

        let expected = vec![Stmt::Block(Block {
            statements: vec![
                Box::new(Stmt::Var(Var {
                    name: Token::Simple(TokenType::Identifier, "a".to_owned(), 1),
                    initializer: Box::new(Some(Expr::Literal(Literal {
                        value: LiteralType::FloatLiteral(1.),
                    }))),
                })),
                Box::new(Stmt::Var(Var {
                    name: Token::Simple(TokenType::Identifier, "b".to_owned(), 1),
                    initializer: Box::new(Some(Expr::Literal(Literal {
                        value: LiteralType::FloatLiteral(2.),
                    }))),
                })),
            ],
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn if_statement() {
        let mut parser = Parser::new(get_tokens("if (true) {var a;} else{var b;}"));

        let expected = vec![Stmt::If({
            If {
                condition: Box::new(Expr::Literal(Literal {
                    value: LiteralType::BoolLiteral(true),
                })),
                then_branch: Box::new(Stmt::Block(Block {
                    statements: vec![Box::new(Stmt::Var(Var {
                        name: Token::Simple(TokenType::Identifier, "a".to_owned(), 1),
                        initializer: Box::new(None),
                    }))],
                })),
                else_branch: Box::new(Some(Stmt::Block(Block {
                    statements: vec![Box::new(Stmt::Var(Var {
                        name: Token::Simple(TokenType::Identifier, "b".to_owned(), 1),
                        initializer: Box::new(None),
                    }))],
                }))),
            }
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn logical_or() {
        let mut parser = Parser::new(get_tokens("true or false;"));

        let expected = vec![Stmt::Expression(Expression {
            expression: Box::new(Expr::Logical(Logical {
                left: Box::new(Expr::Literal(Literal {
                    value: LiteralType::BoolLiteral(true),
                })),
                operator: Token::Simple(TokenType::Or, "or".to_owned(), 1),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralType::BoolLiteral(false),
                })),
            })),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn logical_and() {
        let mut parser = Parser::new(get_tokens("true and false;"));

        let expected = vec![Stmt::Expression(Expression {
            expression: Box::new(Expr::Logical(Logical {
                left: Box::new(Expr::Literal(Literal {
                    value: LiteralType::BoolLiteral(true),
                })),
                operator: Token::Simple(TokenType::And, "and".to_owned(), 1),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralType::BoolLiteral(false),
                })),
            })),
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn while_statement() {
        let mut parser = Parser::new(get_tokens("while(true){print 1;}"));

        let expected = vec![Stmt::While({
            While {
                condition: Box::new(Expr::Literal(Literal {
                    value: LiteralType::BoolLiteral(true),
                })),
                body: Box::new(Stmt::Block(Block {
                    statements: vec![Box::new(Stmt::Print(Print {
                        expression: Box::new(Expr::Literal(Literal {
                            value: LiteralType::FloatLiteral(1.),
                        })),
                    }))],
                })),
            }
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }

    #[test]
    fn fun_statement() {
        let mut parser = Parser::new(get_tokens("fun one(){print 1;}"));

        let expected = vec![Stmt::Function(stmt::Function {
            name: Token::Simple(TokenType::Identifier, "one".to_owned(), 1),
            params: vec![],
            body: vec![Box::new(Stmt::Print(Print {
                expression: Box::new(Expr::Literal(Literal {
                    value: LiteralType::FloatLiteral(1.),
                })),
            }))],
        })];

        assert_eq!(parser.parse().unwrap(), expected);
    }
}
