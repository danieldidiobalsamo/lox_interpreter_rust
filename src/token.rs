use std::fmt::Display;

use crate::lox_callable::Callable;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // litterals
    Identifier,
    String,
    Number,

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "LeftParen"),
            TokenType::RightParen => write!(f, "RightParen"),
            TokenType::LeftBrace => write!(f, "LeftBrace"),
            TokenType::RightBrace => write!(f, "RightBrace"),
            TokenType::Comma => write!(f, "Comma"),
            TokenType::Dot => write!(f, "Dot"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Semicolon => write!(f, "Semicolon"),
            TokenType::Slash => write!(f, "Slash"),
            TokenType::Star => write!(f, "Star"),
            TokenType::Bang => write!(f, "Bang"),
            TokenType::BangEqual => write!(f, "BangEqual"),
            TokenType::Equal => write!(f, "Equal"),
            TokenType::EqualEqual => write!(f, "EqualEqual"),
            TokenType::Greater => write!(f, "Greater"),
            TokenType::GreaterEqual => write!(f, "GreaterEqual"),
            TokenType::Less => write!(f, "Less"),
            TokenType::LessEqual => write!(f, "LessEqual"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::String => write!(f, "String"),
            TokenType::Number => write!(f, "Number"),
            TokenType::And => write!(f, "And"),
            TokenType::Class => write!(f, "Class"),
            TokenType::Else => write!(f, "Else"),
            TokenType::False => write!(f, "False"),
            TokenType::Fun => write!(f, "Fun"),
            TokenType::For => write!(f, "For"),
            TokenType::If => write!(f, "If"),
            TokenType::Nil => write!(f, "Nil"),
            TokenType::Or => write!(f, "Or"),
            TokenType::Print => write!(f, "Print"),
            TokenType::Return => write!(f, "Return"),
            TokenType::Super => write!(f, "Super"),
            TokenType::This => write!(f, "This"),
            TokenType::True => write!(f, "True"),
            TokenType::Var => write!(f, "Var"),
            TokenType::While => write!(f, "While"),
            TokenType::Eof => write!(f, "Eof"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralType {
    StringLiteral(String),
    FloatLiteral(f64),
    BoolLiteral(bool), // in lox, "nil" and "false" are false, everything else is true
    NilLiteral,
    Callable(Callable),
}

impl LiteralType {
    pub fn get_float(&self) -> Result<f64, String> {
        if let LiteralType::FloatLiteral(v) = self {
            return Ok(*v);
        }

        Err("Not a float literal".to_owned())
    }

    pub fn get_string(&self) -> Result<String, String> {
        if let LiteralType::StringLiteral(s) = self {
            return Ok(s.to_string());
        }

        Err("Not a string literal".to_owned())
    }
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralType::StringLiteral(l) => String::from(l),
                LiteralType::FloatLiteral(l) => l.to_string(),
                LiteralType::BoolLiteral(l) => l.to_string(),
                LiteralType::NilLiteral => String::from("nil"),
                LiteralType::Callable(c) => match c {
                    Callable::Clock(_) => "<fn clock>".to_owned(),
                    Callable::Function(f) => format!("<fn {}>", f.declaration.name.get_lexeme()),
                    Callable::LoxClass(lox_class) => lox_class.name.clone(),
                    Callable::LoxInstance(lox_instance) =>
                        format!("{} instance", lox_instance.class.borrow().name),
                },
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // token_type, lexeme, line
    Simple(TokenType, String, usize),
    // token_type, lexeme, literal, line
    Literal(TokenType, String, LiteralType, usize),
}

impl Token {
    pub fn get_lexeme(&self) -> String {
        match self {
            Token::Simple(_, lexeme, _) => (*lexeme).to_string(),
            Token::Literal(_, lexeme, _, _) => (*lexeme).to_string(),
        }
    }

    pub fn get_type(&self) -> &TokenType {
        match self {
            Token::Simple(token_type, _, _) => token_type,
            Token::Literal(token_type, _, _, _) => token_type,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Simple(token_type, lexeme, line) => {
                write!(f, "{} {} {}", token_type, lexeme, line)
            }
            Token::Literal(token_type, lexeme, literal, line) => {
                write!(f, "{} {} {} {}", token_type, lexeme, literal, line)
            }
        }
    }
}
