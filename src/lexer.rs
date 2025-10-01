#[derive(Debug)]
pub struct Lexer {
    pub src: String,
    pub line: usize,
    pub coln: usize, // TODO: populate this
    curr: usize,
    start: usize,
}

#[derive(Debug, PartialEq)]
pub struct TokenHeader {
    pub tokn: Token,
    pub coln: usize,
    pub line: usize,
    pub lexm: String,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Colon,
    SemiColon,

    Minus,
    Slash,
    Plus,
    Star,

    And,
    Or,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    While,
    Break,
    Continue,
    Print,

    Nil,
    Int(i64),
    Bool(bool),
    LiteralString,
    Identifer,

    TypeInteger,

    EOF,
}

#[derive(Debug)]
pub struct LexerError {
    pub line: usize,
    pub coln: usize,
    pub mssg: &'static str,
    pub lexm: String,
}

impl LexerError {
    pub fn new(mssg: &'static str, lexer: &Lexer) -> LexerError {
        LexerError {
            line: lexer.line,
            coln: lexer.coln,
            lexm: String::from(&lexer.src[lexer.start..lexer.curr]),
            mssg,
        }
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "at {}-{}: {}", self.line, self.coln, self.mssg)
    }
}

impl std::error::Error for LexerError {}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer {
            src,
            coln: 0,
            line: 0,
            curr: 0,
            start: 0,
        }
    }

    // TODO: u64 -> f64
    fn consume_integer(&mut self) -> i64 {
        self.curr = match self.src[self.curr..].find(|c: char| !c.is_ascii_digit()) {
            Some(end) => self.curr + end,
            None => self.src.len()
        };
        self.src[self.start..self.curr].parse::<i64>().unwrap()
    }

    fn consume_id(&mut self) -> String {
        self.curr = match self.src[self.curr..].find(|c: char| !c.is_ascii_alphanumeric()) {
            Some(end) => self.curr + end,
            None => self.src.len()
        };
        String::from(&self.src[self.start..self.curr])
    }

    fn consume_string(&mut self) -> Result<String, LexerError> {
        match self.src[self.curr+1..].find('"') {
            Some(end) => {
                self.curr += end + 2;
                Ok(String::from(&self.src[self.start..self.curr]))
            },
            None => Err(
                LexerError::new("expected trailing \"", &self)
            )
        }
    }

    fn consume_char(&mut self) -> Option<char>{
        loop {
            match self.src.chars().nth(self.curr) {
                Some(c) => {
                    // TODO: make this not ugly
                    self.curr += 1;
                    if c == '\n' {
                        self.line += 1;
                    } else if c != ' ' && c != '\r' && c != '\t' {
                        return Some(c);
                    }
                    self.start = self.curr;
                },
                None => return None,
            };
        }
    }

    fn expect_char(&mut self, e: char) -> bool {
        match self.src.chars().nth(self.curr) {
            Some(c) => {
                if c == e {
                    self.curr += 1;
                } 
                c == e
            },
            None => false
        }
    }

    pub fn next(&mut self) -> Result<TokenHeader, LexerError> {
        self.start = self.curr;
        let c = match self.consume_char() {
            Some(c) => c,
            None => return Ok(TokenHeader {
                tokn: Token::EOF,
                coln: self.coln,
                line: self.line,
                lexm: String::from(&self.src[self.start..self.curr])
            })
        };
        let tok = match c {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ';' => Token::SemiColon,
            '-' => Token::Minus,
            '/' => Token::Slash,
            '+' => Token::Plus,
            '*' => Token::Star,
            '=' => {
                if self.expect_char('=') {
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            },
            '!' => {
                if self.expect_char('=') {
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            },
            '<' => {
                if self.expect_char('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                }
            },
            '>' => {
                if self.expect_char('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            },
            '|' => {
                if self.expect_char('|') {
                    Token::Or
                } else {
                    return Err(LexerError::new("there is no '|' operator", self))
                }
            },
            '&' => {
                if self.expect_char('&') {
                    Token::And
                } else {
                    return Err(LexerError::new("there is no '|' operator", self))
                }
            },
            ':' => Token::Colon,
            c if c.is_ascii_digit() => {
                self.curr -= 1;
                Token::Int(self.consume_integer())
            },
            '"' => {
                self.curr -= 1;
                // self.consume_string()?
                Token::LiteralString
            },
            c if c.is_ascii_alphabetic() => {
                self.curr -= 1;
                let id = self.consume_id();
                // TODO: replace with hashmap
                match id.as_str() {
                    "while" => Token::While,
                    "break" => Token::Break,
                    "continue" => Token::Continue,
                    "print" => Token::Print,
                    "int" => Token::TypeInteger,
                    "false" => Token::Bool(false),
                    "true" => Token::Bool(true),
                    "nil" => Token::Nil,
                    _ => Token::Identifer
                }
            },
            _ => return Err(
                LexerError::new("invalid character", &self)
            )
        };
        Ok(TokenHeader {
            tokn: tok,
            coln: self.coln,
            line: self.line,
            lexm: String::from(&self.src[self.start..self.curr])
        })
    }

    pub fn peek(&mut self) -> Result<TokenHeader, LexerError> {
        let curr = self.curr;
        let result = self.next();
        self.curr = curr;
        result
    }
}
