use thiserror::Error;

use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum ScannerError {
    #[error("Unexpected character")]
    UnexpectedCharacter { c: char },
    #[error("Unterminated string")]
    UnterminatedString,
    #[error("Unterminated float")]
    UnterminatedFloat,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scanner {
    source: Vec<char>,
    start: usize,
    last: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            last: source.len(),
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, ScannerError> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof));
        }

        let token = match self.advance() {
            x if x.is_digit(10) => self.number()?,
            x if x.is_alphabetic() || x == '_' => self.identifier(),
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            '"' => self.string()?,
            c => return Err(ScannerError::UnexpectedCharacter { c }),
        };

        Ok(token)
    }
    fn identifier(&mut self) -> Token {
        while matches!(self.peek(), Some(x) if x.is_alphabetic() || *x == '_' || x.is_digit(10)) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        match self.source[self.start] {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => {
                // 'f' is a valid identifier name
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => self.check_keyword(2, 1, "r", TokenType::For),
                        'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                // 't' is a valid identifier name
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        'h' => self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => self.check_keyword(2, 2, "ue", TokenType::True),

                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }
    fn check_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &str,
        token_type: TokenType,
    ) -> TokenType {
        let start_index = self.start + start;
        let end_index = start_index + length;
        let sub = self.source[start_index..end_index]
            .into_iter()
            .collect::<String>();

        if self.current - self.start == start + length && sub == rest {
            token_type
        } else {
            TokenType::Identifier
        }
    }

    fn number(&mut self) -> Result<Token, ScannerError> {
        while matches!(self.peek(), Some(x) if x.is_digit(10)) {
            self.advance();
        }

        // search for the fractional part
        if self.peek() == Some(&'.') {
            if matches!(self.peek_next(), Some(x) if x.is_digit(10)) {
                self.advance();

                while matches!(self.peek(), Some(x) if x.is_digit(10)) {
                    self.advance();
                }
            } else {
                return Err(ScannerError::UnterminatedFloat);
            }
        }

        Ok(self.make_token(TokenType::Number))
    }

    fn string(&mut self) -> Result<Token, ScannerError> {
        while self.peek() != Some(&'"') && !self.is_at_end() {
            if self.peek() == Some(&'\n') {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(ScannerError::UnterminatedString);
        }

        self.advance();

        Ok(self.make_token(TokenType::String))
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(c) = self.peek() {
                match c {
                    ' ' | '\r' | '\t' => {
                        self.advance();
                    }
                    '\n' => {
                        self.line += 1;
                        self.advance();
                    }
                    '/' => {
                        if matches!(self.peek_next(), Some(c) if *c == '/') {
                            while self.peek() != Some(&'\n') && !self.is_at_end() {
                                self.advance();
                            }
                        } else {
                            break;
                        }
                    }
                    _ => break,
                };
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<&char> {
        self.source.get(self.current)
    }

    fn peek_next(&self) -> Option<&char> {
        self.source.get(self.current + 1)
    }

    fn match_char(&mut self, expected: char) -> bool {
        match self.source.get(self.current) {
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

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;

        c
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.last
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        let lexeme = match token_type {
            TokenType::String | TokenType::Number | TokenType::Identifier => {
                let (start, end) = if token_type == TokenType::String {
                    (self.start + 1, self.current - 1) // ignore quotes
                } else {
                    (self.start, self.current)
                };

                Some(self.source[start..end].iter().collect::<String>())
            }
            _ => None,
        };

        Token::new(token_type, self.start, self.line, lexeme)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_results(scanner: &mut Scanner, good: &[Token], eof: (usize, usize)) {
        for token in good {
            assert_eq!(scanner.scan_token().unwrap(), *token);
        }

        // at this point scanner should only return EOF
        assert_eq!(
            scanner.scan_token().unwrap(),
            Token::new(TokenType::Eof, eof.0, eof.1, None)
        );
    }

    #[test]
    fn unexpected_char() {
        let mut scanner = Scanner::new("~");
        assert!(scanner.scan_token().is_err());
    }

    #[test]
    fn eof() {
        let mut scanner = Scanner::new("");
        assert_eq!(
            scanner.scan_token().unwrap(),
            Token::new(TokenType::Eof, 0, 1, None)
        );
    }

    #[test]
    fn comparisons() {
        let mut scanner = Scanner::new(">>=\n<=<< == > =");

        let good = vec![
            Token::new(TokenType::Greater, 0, 1, None),
            Token::new(TokenType::GreaterEqual, 1, 1, None),
            Token::new(TokenType::LessEqual, 4, 2, None),
            Token::new(TokenType::Less, 6, 2, None),
            Token::new(TokenType::Less, 7, 2, None),
            Token::new(TokenType::EqualEqual, 9, 2, None),
            Token::new(TokenType::Greater, 12, 2, None),
            Token::new(TokenType::Equal, 14, 2, None),
            Token::new(TokenType::Eof, 15, 2, None),
        ];

        check_results(&mut scanner, &good, (15, 2));
    }

    #[test]
    fn comment() {
        let mut scanner = Scanner::new("<//test\n+");
        let good = vec![
            Token::new(TokenType::Less, 0, 1, None),
            Token::new(TokenType::Plus, 8, 2, None),
        ];

        check_results(&mut scanner, &good, (9, 2));
    }

    #[test]
    fn string() {
        let mut scanner = Scanner::new("+\"test\"-");
        let good = vec![
            Token::new(TokenType::Plus, 0, 1, None),
            Token::new(TokenType::String, 1, 1, Some("test".to_owned())),
            Token::new(TokenType::Minus, 7, 1, None),
            Token::new(TokenType::Eof, 8, 1, None),
        ];

        check_results(&mut scanner, &good, (8, 1));
    }

    #[test]
    fn multiline_string_literal() {
        let mut scanner = Scanner::new("\"hello\nworld\"");
        let good = vec![
            Token::new(TokenType::String, 0, 2, Some("hello\nworld".to_owned())),
            Token::new(TokenType::Eof, 13, 2, None),
        ];

        check_results(&mut scanner, &good, (13, 2));
    }

    #[test]
    fn string_literal_on_same_line() {
        let mut scanner = Scanner::new("\"hello world\"\"test\"");
        let good = vec![
            Token::new(TokenType::String, 0, 1, Some("hello world".to_owned())),
            Token::new(TokenType::String, 13, 1, Some("test".to_owned())),
            Token::new(TokenType::Eof, 19, 1, None),
        ];

        check_results(&mut scanner, &good, (19, 1));
    }

    #[test]
    fn unterminated_string() {
        let mut scanner = Scanner::new("\"");

        assert!(scanner.scan_token() == Err(ScannerError::UnterminatedString));
    }

    #[test]
    fn only_comment() {
        let mut scanner = Scanner::new("//");

        let good = vec![Token::new(TokenType::Eof, 2, 1, None)];
        check_results(&mut scanner, &good, (2, 1));
    }

    #[test]
    fn ignored_character() {
        let mut scanner = Scanner::new("\r \t \t \r\r");

        let good = vec![Token::new(TokenType::Eof, 8, 1, None)];
        check_results(&mut scanner, &good, (8, 1));
    }

    #[test]
    fn handle_slash() {
        let mut scanner = Scanner::new("/ // /<<=\n*");

        let good = vec![
            Token::new(TokenType::Slash, 0, 1, None),
            Token::new(TokenType::Star, 10, 2, None),
            Token::new(TokenType::Eof, 11, 2, None),
        ];

        check_results(&mut scanner, &good, (11, 2));
    }

    #[test]
    fn number() {
        let mut scanner = Scanner::new("2.5+2");

        let good = vec![
            Token::new(TokenType::Number, 0, 1, Some("2.5".to_owned())),
            Token::new(TokenType::Plus, 3, 1, None),
            Token::new(TokenType::Number, 4, 1, Some("2".to_owned())),
            Token::new(TokenType::Eof, 5, 1, None),
        ];

        check_results(&mut scanner, &good, (5, 1));
    }

    #[test]
    fn identifier() {
        let mut scanner = Scanner::new("test1");

        let good = vec![
            Token::new(TokenType::Identifier, 0, 1, Some("test1".to_owned())),
            Token::new(TokenType::Eof, 5, 1, None),
        ];

        check_results(&mut scanner, &good, (5, 1));
    }

    #[test]
    fn reserved_keywords() {
        let source = "and\nclass\nelse\nfalse\nfor\nfun\nif\nnil\nor\nprint\nreturn\nsuper\nthis\ntrue\nvar\nwhile";

        let mut scanner = Scanner::new(&source);

        let good = vec![
            Token::new(TokenType::And, 0, 1, None),
            Token::new(TokenType::Class, 4, 2, None),
            Token::new(TokenType::Else, 10, 3, None),
            Token::new(TokenType::False, 15, 4, None),
            Token::new(TokenType::For, 21, 5, None),
            Token::new(TokenType::Fun, 25, 6, None),
            Token::new(TokenType::If, 29, 7, None),
            Token::new(TokenType::Nil, 32, 8, None),
            Token::new(TokenType::Or, 36, 9, None),
            Token::new(TokenType::Print, 39, 10, None),
            Token::new(TokenType::Return, 45, 11, None),
            Token::new(TokenType::Super, 52, 12, None),
            Token::new(TokenType::This, 58, 13, None),
            Token::new(TokenType::True, 63, 14, None),
            Token::new(TokenType::Var, 68, 15, None),
            Token::new(TokenType::While, 72, 16, None),
            Token::new(TokenType::Eof, 77, 16, None),
        ];

        check_results(&mut scanner, &good, (77, 16));
    }

    #[test]
    fn trailing_decimal_point_forbidden() {
        let source = "5.";
        let mut scanner = Scanner::new(&source);

        assert!(scanner.scan_token() == Err(ScannerError::UnterminatedFloat));
    }

    #[test]
    fn trailing_decimal_point_forbidden_second() {
        let source = "5.a";
        let mut scanner = Scanner::new(&source);

        assert!(scanner.scan_token() == Err(ScannerError::UnterminatedFloat));
    }
}
