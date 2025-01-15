#[derive(Debug, PartialEq, Eq, Clone)]
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

impl ToString for TokenType {
    fn to_string(&self) -> String {
        match *self {
            TokenType::LeftParen => String::from("LeftParen"),
            TokenType::RightParen => String::from("RightParen"),
            TokenType::LeftBrace => String::from("LeftBrace"),
            TokenType::RightBrace => String::from("RightBrace"),
            TokenType::Comma => String::from("Comma"),
            TokenType::Dot => String::from("Dot"),
            TokenType::Minus => String::from("Minus"),
            TokenType::Plus => String::from("Plus"),
            TokenType::Semicolon => String::from("Semicolon"),
            TokenType::Slash => String::from("Slash"),
            TokenType::Star => String::from("Star"),
            TokenType::Bang => String::from("Bang"),
            TokenType::BangEqual => String::from("BangEqual"),
            TokenType::Equal => String::from("Equal"),
            TokenType::EqualEqual => String::from("EqualEqual"),
            TokenType::Greater => String::from("Greater"),
            TokenType::GreaterEqual => String::from("GreaterEqual"),
            TokenType::Less => String::from("Less"),
            TokenType::LessEqual => String::from("LessEqual"),
            TokenType::Identifier => String::from("Identifier"),
            TokenType::String => String::from("String"),
            TokenType::Number => String::from("Number"),
            TokenType::And => String::from("And"),
            TokenType::Class => String::from("Class"),
            TokenType::Else => String::from("Else"),
            TokenType::False => String::from("False"),
            TokenType::Fun => String::from("Fun"),
            TokenType::For => String::from("For"),
            TokenType::If => String::from("If"),
            TokenType::Nil => String::from("Nil"),
            TokenType::Or => String::from("Or"),
            TokenType::Print => String::from("Print"),
            TokenType::Return => String::from("Return"),
            TokenType::Super => String::from("Super"),
            TokenType::This => String::from("This"),
            TokenType::True => String::from("True"),
            TokenType::Var => String::from("Var"),
            TokenType::While => String::from("While"),
            TokenType::Eof => String::from("Eof"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralType {
    StringLiteral(String),
    FloatLiteral(f64),
    BoolLiteral(bool), // in lox, "nil" and "false" are false, everything else is true
    NilLiteral,
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

impl ToString for LiteralType {
    fn to_string(&self) -> String {
        match self {
            LiteralType::StringLiteral(l) => String::from(l),
            LiteralType::FloatLiteral(l) => l.to_string(),
            LiteralType::BoolLiteral(l) => l.to_string(),
            LiteralType::NilLiteral => String::from("nil"),
        }
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

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::Simple(token_type, lexeme, line) => {
                format!("{} {} {}", token_type.to_string(), lexeme, line)
            }
            Token::Literal(token_type, lexeme, literal, line) => {
                format!(
                    "{} {} {} {}",
                    token_type.to_string(),
                    lexeme,
                    literal.to_string(),
                    line
                )
            }
        }
    }
}
