use thiserror::Error;
use uuid::Uuid;

use crate::{
    expr::{
        Assign, Binary, Call, Expr, Grouping, Literal, Logical, Set, SuperExpr, This, Unary,
        Variable,
    },
    lox_error::LoxError,
    stmt::{self, Block, Class, Expression, If, Print, Return, Stmt, Var, While},
    token::{LiteralType, Token, TokenType},
};

#[derive(Debug, Clone, Error)]
pub enum ParserError {
    #[error("'{lexeme}' at line {line} : Expect '{c}' {msg}.")]
    ExpectChar {
        lexeme: String,
        line: usize,
        c: char,
        msg: String,
    },
    #[error("\"{lexeme}\" at line {line} : Expect class name.")]
    ExpectClass { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect superclass name.")]
    ExpectSuperClass { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect superclass method name.")]
    ExpectSuperClassMethod { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect function name.")]
    ExpectFunction { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect parameter name.")]
    ExpectParameter { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect variable name.")]
    ExpectVariable { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect property identifier after '.'.")]
    ExpectProperty { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Expect expression")]
    ExpectExpression { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Can't have more than 255 arguments.")]
    ArgumentsLimit { lexeme: String, line: usize },
    #[error("\"{lexeme}\" at line {line} : Invalid assignment target")]
    InvalidAssignment { lexeme: String, line: usize },
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, LoxError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(d) => statements.push(d),
                Err(e) => {
                    self.synchronize();
                    return Err(LoxError::Parser(e));
                }
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.match_token_type(&[TokenType::Class]) {
            return self.class_declaration();
        }

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

        self.statement()
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.consume(
            TokenType::Identifier,
            ParserError::ExpectClass {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
            },
        )?;

        let super_class = if self.match_token_type(&[TokenType::Less]) {
            let _ = self.consume(
                TokenType::Identifier,
                ParserError::ExpectSuperClass {
                    lexeme: self.current_lexeme(),
                    line: self.current_line(),
                },
            )?;

            Some(Expr::Variable(Variable {
                uuid: Uuid::new_v4(),
                name: self.previous().clone(),
            }))
        } else {
            None
        };

        let _ = self.consume(
            TokenType::LeftBrace,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '{',
                msg: "before class body".to_owned(),
            },
        )?;

        let mut methods = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function()?);
        }

        let _ = self.consume(
            TokenType::RightBrace,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '}',
                msg: "after class body".to_owned(),
            },
        )?;

        Ok(Stmt::Class(Class {
            name,
            methods,
            super_class: Box::new(super_class),
        }))
    }

    fn function(&mut self) -> Result<Stmt, ParserError> {
        let name = self.consume(
            TokenType::Identifier,
            ParserError::ExpectFunction {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
            },
        )?;
        let _ = self.consume(
            TokenType::LeftParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '(',
                msg: "after function name".to_owned(),
            },
        )?;

        let mut params = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(ParserError::ArgumentsLimit {
                        lexeme: self.current_lexeme(),
                        line: self.current_line(),
                    });
                }

                params.push(self.consume(
                    TokenType::Identifier,
                    ParserError::ExpectParameter {
                        lexeme: self.current_lexeme(),
                        line: self.current_line(),
                    },
                )?);

                if !self.match_token_type(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let _ = self.consume(
            TokenType::RightParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ')',
                msg: "after parameter name".to_owned(),
            },
        )?;
        let _ = self.consume(
            TokenType::LeftBrace,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '{',
                msg: "before function body".to_owned(),
            },
        )?;

        let body = self.block()?;

        Ok(Stmt::Function(stmt::Function { name, params, body }))
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.consume(
            TokenType::Identifier,
            ParserError::ExpectVariable {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
            },
        )?;

        let initializer = if self.match_token_type(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        let _ = self.consume(
            TokenType::Semicolon,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ';',
                msg: "after variable declaration".to_owned(),
            },
        )?;

        Ok(Stmt::Var(Var {
            name,
            initializer: Box::new(initializer),
        }))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.match_token_type(&[TokenType::For]) {
            return self.for_statement();
        }

        if self.match_token_type(&[TokenType::If]) {
            return self.if_statement();
        }

        if self.match_token_type(&[TokenType::Print]) {
            return self.print_statement();
        }

        if self.match_token_type(&[TokenType::Return]) {
            return self.return_statement();
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

    fn return_statement(&mut self) -> Result<Stmt, ParserError> {
        let keyword = self.previous().clone();

        let value = if !self.check(&TokenType::Semicolon) {
            self.expression()?
        } else {
            Expr::Literal(Literal {
                uuid: Uuid::new_v4(),
                value: LiteralType::Nil,
            })
        };

        let _ = self.consume(
            TokenType::Semicolon,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ';',
                msg: "after return value".to_owned(),
            },
        )?;

        Ok(Stmt::Return(Return {
            keyword,
            value: Box::new(Some(value)),
        }))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(
            TokenType::LeftParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '(',
                msg: "after for".to_owned(),
            },
        )?;

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

        self.consume(
            TokenType::Semicolon,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ';',
                msg: "after for loop condition".to_owned(),
            },
        )?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::RightParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ')',
                msg: "after for clauses".to_owned(),
            },
        )?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(Block {
                statements: vec![
                    body,
                    Stmt::Expression(Expression {
                        expression: Box::new(inc),
                    }),
                ],
            })
        }

        if condition.is_none() {
            condition = Some(Expr::Literal(Literal {
                uuid: Uuid::new_v4(),
                value: LiteralType::Bool(true),
            }));
        }

        body = Stmt::While(While {
            condition: Box::new(condition.unwrap()),
            body: Box::new(body),
        });

        if let Some(init) = initializer {
            body = Stmt::Block(Block {
                statements: vec![init, body],
            })
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(
            TokenType::LeftParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '(',
                msg: "after while".to_owned(),
            },
        )?;
        let condition = self.expression()?;
        self.consume(
            TokenType::RightParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ')',
                msg: "after while condition".to_owned(),
            },
        )?;

        let body = self.statement()?;

        Ok(Stmt::While(While {
            condition: Box::new(condition),
            body: Box::new(body),
        }))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        let _ = self.consume(
            TokenType::LeftParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '(',
                msg: "after if".to_owned(),
            },
        )?;
        let condition = self.expression()?;
        let _ = self.consume(
            TokenType::RightParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ')',
                msg: "after if condition".to_owned(),
            },
        )?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_token_type(&[TokenType::Else]) {
            // note: in lox, else statement is bound to the nearest if that precedes it
            Some(self.statement()?)
        } else {
            None
        };

        Ok(Stmt::If(If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(
            TokenType::RightBrace,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: '}',
                msg: "after block".to_owned(),
            },
        )?;

        Ok(statements)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let val = self.expression()?;
        let _ = self.consume(
            TokenType::Semicolon,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ';',
                msg: "after value".to_owned(),
            },
        )?;

        Ok(Stmt::Print(Print {
            expression: Box::new(val),
        }))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        let _ = self.consume(
            TokenType::Semicolon,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ';',
                msg: "after expression".to_owned(),
            },
        )?;

        Ok(Stmt::Expression(Expression {
            expression: Box::new(expr),
        }))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assigment()
    }

    fn assigment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.or()?;

        if self.match_token_type(&[TokenType::Equal]) {
            let value = self.assigment()?;

            if let Expr::Variable(var) = expr {
                return Ok(Expr::Assign(Assign {
                    uuid: Uuid::new_v4(),
                    name: var.name,
                    value: Box::new(value),
                }));
            } else if let Expr::Get(get) = expr {
                return Ok(Expr::Set(Set {
                    uuid: Uuid::new_v4(),
                    object: get.object,
                    name: get.name,
                    value: Box::new(value),
                }));
            } else {
                return Err(ParserError::InvalidAssignment {
                    lexeme: self.current_lexeme(),
                    line: self.current_line(),
                });
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;

        while self.match_token_type(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;

            expr = Expr::Logical(Logical {
                uuid: Uuid::new_v4(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;

        while self.match_token_type(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;

            expr = Expr::Logical(Logical {
                uuid: Uuid::new_v4(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;

        while self.match_token_type(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;

            expr = Expr::Binary(Binary {
                uuid: Uuid::new_v4(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
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
                uuid: Uuid::new_v4(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while self.match_token_type(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;

            expr = Expr::Binary(Binary {
                uuid: Uuid::new_v4(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;

        while self.match_token_type(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            expr = Expr::Binary(Binary {
                uuid: Uuid::new_v4(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token_type(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            return Ok(Expr::Unary(Unary {
                uuid: Uuid::new_v4(),
                operator,
                right: Box::new(right),
            }));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token_type(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_token_type(&[TokenType::Dot]) {
                let name = self.consume(
                    TokenType::Identifier,
                    ParserError::ExpectProperty {
                        lexeme: self.current_lexeme(),
                        line: self.current_line(),
                    },
                )?;

                expr = Expr::Get(crate::expr::Get {
                    uuid: Uuid::new_v4(),
                    object: Box::new(expr),
                    name,
                });
            } else {
                break Ok(expr);
            }
        }
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(ParserError::ArgumentsLimit {
                        lexeme: self.current_lexeme(),
                        line: self.current_line(),
                    });
                }

                arguments.push(self.expression()?);

                if !self.match_token_type(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(
            TokenType::RightParen,
            ParserError::ExpectChar {
                lexeme: self.current_lexeme(),
                line: self.current_line(),
                c: ')',
                msg: "after arguments".to_owned(),
            },
        )?;

        Ok(Expr::Call(Call {
            uuid: Uuid::new_v4(),
            callee: Box::new(callee),
            paren,
            arguments,
        }))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token_type(&[TokenType::False]) {
            return Ok(Expr::Literal(Literal {
                uuid: Uuid::new_v4(),
                value: LiteralType::Bool(false),
            }));
        }

        if self.match_token_type(&[TokenType::True]) {
            return Ok(Expr::Literal(Literal {
                uuid: Uuid::new_v4(),
                value: LiteralType::Bool(true),
            }));
        }

        if self.match_token_type(&[TokenType::Nil]) {
            return Ok(Expr::Literal(Literal {
                uuid: Uuid::new_v4(),
                value: LiteralType::Nil,
            }));
        }

        if self.match_token_type(&[TokenType::Number, TokenType::String]) {
            if let Token::Literal(_, _, literal, _) = self.previous() {
                return Ok(Expr::Literal(Literal {
                    uuid: Uuid::new_v4(),
                    value: literal.clone(),
                }));
            }
        }

        if self.match_token_type(&[TokenType::Super]) {
            let keyword = self.previous().clone();
            let _ = self.consume(
                TokenType::Dot,
                ParserError::ExpectChar {
                    lexeme: self.current_lexeme(),
                    line: self.current_line(),
                    c: '.',
                    msg: "after super".to_owned(),
                },
            )?;
            let method = self.consume(
                TokenType::Identifier,
                ParserError::ExpectSuperClassMethod {
                    lexeme: self.current_lexeme(),
                    line: self.current_line(),
                },
            )?;

            return Ok(Expr::SuperExpr(SuperExpr {
                uuid: Uuid::new_v4(),
                keyword,
                method,
            }));
        }

        if self.match_token_type(&[TokenType::This]) {
            return Ok(Expr::This(This {
                uuid: Uuid::new_v4(),
                keyword: self.previous().clone(),
            }));
        }

        if self.match_token_type(&[TokenType::Identifier]) {
            return Ok(Expr::Variable(Variable {
                uuid: Uuid::new_v4(),
                name: self.previous().clone(),
            }));
        }

        if self.match_token_type(&[TokenType::LeftParen]) {
            let expr = self.expression()?;

            let _ = self.consume(
                TokenType::RightParen,
                ParserError::ExpectChar {
                    lexeme: self.current_lexeme(),
                    line: self.current_line(),
                    c: ')',
                    msg: "after expresson".to_owned(),
                },
            )?;

            return Ok(Expr::Grouping(Grouping {
                uuid: Uuid::new_v4(),
                expression: Box::new(expr),
            }));
        }

        Err(ParserError::ExpectExpression {
            lexeme: self.current_lexeme(),
            line: self.current_line(),
        })
    }

    fn consume(&mut self, token_type: TokenType, error: ParserError) -> Result<Token, ParserError> {
        if self.check(&token_type) {
            return Ok((*self.advance()).clone());
        }

        Err(error)
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

    fn current_lexeme(&self) -> String {
        self.peek().get_lexeme()
    }

    fn current_line(&self) -> usize {
        self.peek().get_line()
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
