use std::collections::HashMap;

use crate::{
    build_error_message,
    token::{LiteralType, Token, TokenType},
};

pub struct Scanner<'a> {
    source: &'a str,
    src_chars: Vec<char>,
    tokens: Vec<Token>,
    line: usize,
    start_lexeme: usize,
    current: usize,
    error_log: String,
    last: usize,
    keywords: HashMap<&'static str, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        Self {
            source,
            src_chars: source.chars().collect::<Vec<char>>(),
            tokens: Vec::new(),
            line: 1,
            start_lexeme: 0,
            current: 0,
            error_log: String::new(),
            last: source.as_bytes().len(),
            keywords: HashMap::from([
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&[Token], &str> {
        // if scan_tokens() has already been called
        if !self.tokens.is_empty() || !self.error_log.is_empty() {
            return self.get_scan_results();
        }

        while !self.is_last_index(self.current) {
            self.start_lexeme = self.current;
            self.scan_token();
            self.current += 1;
        }

        self.tokens
            .push(Token::Simple(TokenType::Eof, String::new(), self.line));

        self.get_scan_results()
    }

    fn scan_token(&mut self) {
        let c = self.src_chars[self.current];
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '/' => {
                if self.next_is('/') {
                    while let Some(c) = self.peek(self.current + 1) {
                        if *c == '\n' {
                            self.line += 1;
                            self.current += 1;
                            break;
                        }
                        self.current += 1;
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            '!' => {
                if self.next_is('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.next_is('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.next_is('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '>' => {
                if self.next_is('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }
            ' ' | '\r' | '\t' => (),
            '\n' => {
                self.line += 1;
            }
            '\"' => {
                while let Some(c) = self.peek(self.current + 1) {
                    match c {
                        '\"' => break,
                        '\n' => self.line += 1,
                        _ => (),
                    };
                    self.current += 1;
                }

                if self.is_last_index(self.current + 1) {
                    self.error_log += &build_error_message(self.line, "", "Unterminated string");
                } else {
                    self.current += 1; // closing "
                    let literal = &self.source[self.start_lexeme + 1..self.current];

                    self.add_token_literal(
                        TokenType::String,
                        LiteralType::StringLiteral(literal.to_string()),
                    );
                }
            }
            x if x.is_ascii_digit() => {
                self.advance_until_not_digit();

                let (current_char, next_char) =
                    (self.peek(self.current + 1), self.peek(self.current + 2));

                if current_char.is_some() && *current_char.unwrap() == '.' {
                    if next_char.is_none() || !next_char.unwrap().is_ascii_digit() {
                        self.error_log += &build_error_message(self.line, "", "Unterminated float");
                    } else if next_char.unwrap().is_ascii_digit() {
                        self.current += 1; // consume '.'
                        self.advance_until_not_digit();
                    }
                }

                let literal = self.source[self.start_lexeme..=self.current]
                    .parse::<f64>()
                    .unwrap();

                self.add_token_literal(TokenType::Number, LiteralType::FloatLiteral(literal));
            }

            x if x.is_alphabetic() => {
                while let Some(c) = self.peek(self.current + 1) {
                    if !c.is_alphanumeric() {
                        break;
                    }

                    self.current += 1;
                }

                let text = &self.source[self.start_lexeme..=self.current];
                let token_type = match self.keywords.get(&text) {
                    None => TokenType::Identifier,
                    Some(t) => t.clone(),
                };

                self.add_token(token_type);
            }
            _ => {
                self.error_log +=
                    &build_error_message(self.line, "", &format!("Unexpected character: {c}"));
            }
        }
    }

    fn get_scan_results(&self) -> Result<&[Token], &str> {
        if self.error_log.is_empty() {
            Ok(&self.tokens)
        } else {
            Err(&self.error_log)
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start_lexeme..=self.current];

        self.tokens
            .push(Token::Simple(token_type, text.to_owned(), self.line));
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: LiteralType) {
        let text = &self.source[self.start_lexeme..=self.current];

        self.tokens.push(Token::Literal(
            token_type,
            text.to_owned(),
            literal,
            self.line,
        ));
    }

    fn next_is(&mut self, expected: char) -> bool {
        match self.src_chars.get(self.current + 1) {
            None => false,
            Some(c) => {
                if *c == expected {
                    self.current += 1;
                    true
                } else {
                    false
                }
            }
        }
    }

    fn is_last_index(&self, index: usize) -> bool {
        index >= self.last
    }

    fn peek(&self, n: usize) -> Option<&char> {
        self.src_chars.get(n)
    }

    fn advance_until_not_digit(&mut self) {
        while let Some(c) = self.peek(self.current + 1) {
            if !c.is_ascii_digit() {
                break;
            }
            self.current += 1;
        }

        if self.peek(self.current).is_none() {
            self.current -= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn one_char() {
        let source = "(";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::LeftParen, String::from("("), 1),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn single_char_only_lexemes() {
        let source = "(\n)\n{\n}\n,\n.\n-\n+\n;\n*";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::LeftParen, String::from("("), 1),
            Token::Simple(TokenType::RightParen, String::from(")"), 2),
            Token::Simple(TokenType::LeftBrace, String::from("{"), 3),
            Token::Simple(TokenType::RightBrace, String::from("}"), 4),
            Token::Simple(TokenType::Comma, String::from(","), 5),
            Token::Simple(TokenType::Dot, String::from("."), 6),
            Token::Simple(TokenType::Minus, String::from("-"), 7),
            Token::Simple(TokenType::Plus, String::from("+"), 8),
            Token::Simple(TokenType::Semicolon, String::from(";"), 9),
            Token::Simple(TokenType::Star, String::from("*"), 10),
            Token::Simple(TokenType::Eof, String::from(""), 10),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn invalid_character() {
        let source = "@";
        let mut scanner = Scanner::new(&source);

        assert!(scanner.scan_tokens().is_err());
    }

    #[test]
    fn bang_and_bang_equal() {
        let source = "!=\n!";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::BangEqual, String::from("!="), 1),
            Token::Simple(TokenType::Bang, String::from("!"), 2),
            Token::Simple(TokenType::Eof, String::from(""), 2),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn comparisons() {
        let source = ">>=\n<=<< == > =";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::Greater, String::from(">"), 1),
            Token::Simple(TokenType::GreaterEqual, String::from(">="), 1),
            Token::Simple(TokenType::LessEqual, String::from("<="), 2),
            Token::Simple(TokenType::Less, String::from("<"), 2),
            Token::Simple(TokenType::Less, String::from("<"), 2),
            Token::Simple(TokenType::EqualEqual, String::from("=="), 2),
            Token::Simple(TokenType::Greater, String::from(">"), 2),
            Token::Simple(TokenType::Equal, String::from("="), 2),
            Token::Simple(TokenType::Eof, String::from(""), 2),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn ignored_character() {
        let source = "\r \t \t \r\r";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![Token::Simple(TokenType::Eof, String::from(""), 1)];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn handle_slash() {
        let source = "/ // /<<=\n*";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::Slash, String::from("/"), 1),
            Token::Simple(TokenType::Star, String::from("*"), 2),
            Token::Simple(TokenType::Eof, String::from(""), 2),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn only_comment() {
        let source = "//";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![Token::Simple(TokenType::Eof, String::from(""), 1)];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn unterminated_string() {
        let source = "\"";
        let mut scanner = Scanner::new(&source);

        assert!(scanner.scan_tokens().is_err());
    }

    #[test]
    fn string_literal() {
        let source = "\"hello world\"";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Literal(
                TokenType::String,
                String::from("\"hello world\""),
                LiteralType::StringLiteral(String::from("hello world")),
                1,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn string_literal_on_same_line() {
        let source = "\"hello world\"\"test\"";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Literal(
                TokenType::String,
                String::from("\"hello world\""),
                LiteralType::StringLiteral(String::from("hello world")),
                1,
            ),
            Token::Literal(
                TokenType::String,
                String::from("\"test\""),
                LiteralType::StringLiteral(String::from("test")),
                1,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn multiline_string_literal() {
        let source = "\"hello\nworld\"";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Literal(
                TokenType::String,
                String::from("\"hello\nworld\""),
                LiteralType::StringLiteral(String::from("hello\nworld")),
                2,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 2),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn call_scan_tokens_many_times() {
        let source = "(\n)\n{\n}";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::LeftParen, String::from("("), 1),
            Token::Simple(TokenType::RightParen, String::from(")"), 2),
            Token::Simple(TokenType::LeftBrace, String::from("{"), 3),
            Token::Simple(TokenType::RightBrace, String::from("}"), 4),
            Token::Simple(TokenType::Eof, String::from(""), 4),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn integer() {
        let source = "5";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Literal(
                TokenType::Number,
                String::from("5"),
                LiteralType::FloatLiteral(5.0),
                1,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn float() {
        let source = "3.14";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Literal(
                TokenType::Number,
                String::from("3.14"),
                LiteralType::FloatLiteral(3.14),
                1,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn multiple_float() {
        let source = "2.2 + 5.1";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Literal(
                TokenType::Number,
                String::from("2.2"),
                LiteralType::FloatLiteral(2.2),
                1,
            ),
            Token::Simple(TokenType::Plus, String::from("+"), 1),
            Token::Literal(
                TokenType::Number,
                String::from("5.1"),
                LiteralType::FloatLiteral(5.1),
                1,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn leading_decimal_point_as_dot_token() {
        let source = ".5.3";
        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::Dot, String::from("."), 1),
            Token::Literal(
                TokenType::Number,
                String::from("5.3"),
                LiteralType::FloatLiteral(5.3),
                1,
            ),
            Token::Simple(TokenType::Eof, String::from(""), 1),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn trailing_decimal_point_forbidden() {
        let source = "5.";
        let mut scanner = Scanner::new(&source);

        assert!(scanner.scan_tokens().is_err());
    }

    #[test]
    fn trailing_decimal_point_forbidden_second() {
        let source = "5. +";
        let mut scanner = Scanner::new(&source);

        assert!(scanner.scan_tokens().is_err());
    }

    #[test]
    fn reserved_keywords() {
        let source = "and\nclass\nelse\nfalse\nfor\nfun\nif\nnil\nor\nprint\nreturn\nsuper\nthis\ntrue\nvar\nwhile";

        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::And, String::from("and"), 1),
            Token::Simple(TokenType::Class, String::from("class"), 2),
            Token::Simple(TokenType::Else, String::from("else"), 3),
            Token::Simple(TokenType::False, String::from("false"), 4),
            Token::Simple(TokenType::For, String::from("for"), 5),
            Token::Simple(TokenType::Fun, String::from("fun"), 6),
            Token::Simple(TokenType::If, String::from("if"), 7),
            Token::Simple(TokenType::Nil, String::from("nil"), 8),
            Token::Simple(TokenType::Or, String::from("or"), 9),
            Token::Simple(TokenType::Print, String::from("print"), 10),
            Token::Simple(TokenType::Return, String::from("return"), 11),
            Token::Simple(TokenType::Super, String::from("super"), 12),
            Token::Simple(TokenType::This, String::from("this"), 13),
            Token::Simple(TokenType::True, String::from("true"), 14),
            Token::Simple(TokenType::Var, String::from("var"), 15),
            Token::Simple(TokenType::While, String::from("while"), 16),
            Token::Simple(TokenType::Eof, String::from(""), 16),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }

    #[test]
    fn file_simple_lox() {
        let source = fs::read_to_string("./test_lox_scripts/simple.lox").unwrap();

        let mut scanner = Scanner::new(&source);

        let tokens = vec![
            Token::Simple(TokenType::Fun, String::from("fun"), 1),
            Token::Simple(TokenType::Identifier, String::from("addPair"), 1),
            Token::Simple(TokenType::LeftParen, String::from("("), 1),
            Token::Simple(TokenType::Identifier, String::from("a"), 1),
            Token::Simple(TokenType::Comma, String::from(","), 1),
            Token::Simple(TokenType::Identifier, String::from("b"), 1),
            Token::Simple(TokenType::RightParen, String::from(")"), 1),
            Token::Simple(TokenType::LeftBrace, String::from("{"), 1),
            Token::Simple(TokenType::Return, String::from("return"), 2),
            Token::Simple(TokenType::Identifier, String::from("a"), 2),
            Token::Simple(TokenType::Plus, String::from("+"), 2),
            Token::Simple(TokenType::Identifier, String::from("b"), 2),
            Token::Simple(TokenType::Semicolon, String::from(";"), 2),
            Token::Simple(TokenType::RightBrace, String::from("}"), 3),
            Token::Simple(TokenType::Print, String::from("print"), 5),
            Token::Simple(TokenType::Identifier, String::from("addPair"), 5),
            Token::Simple(TokenType::LeftParen, String::from("("), 5),
            Token::Literal(
                TokenType::Number,
                String::from("2"),
                LiteralType::FloatLiteral(2.),
                5,
            ),
            Token::Simple(TokenType::Comma, String::from(","), 5),
            Token::Literal(
                TokenType::Number,
                String::from("2"),
                LiteralType::FloatLiteral(2.),
                5,
            ),
            Token::Simple(TokenType::RightParen, String::from(")"), 5),
            Token::Simple(TokenType::Semicolon, String::from(";"), 5),
            Token::Simple(TokenType::Eof, String::from(""), 5),
        ];

        assert_eq!(tokens, *scanner.scan_tokens().unwrap());
    }
}
