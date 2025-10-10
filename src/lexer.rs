#[derive(Debug)]
pub struct Lexer {
    src: String,
    line: usize,
    line_start: usize,
    curr: usize,
    start: usize,
}

#[derive(Debug, PartialEq, Clone)]
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
    Comma,

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

    If,
    Else,
    While,
    Print,
    Let,
    Func,
    Break,
    Continue,
    Return,

    Nil,
    Int(i32),
    Float(f64),
    Bool(bool),
    String,
    Identifer,

    IntT,
    FloatT,
    StrT,
    BoolT,
    RightArrow,

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
            coln: lexer.start - lexer.line_start + 1,
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
            line: 1,
            line_start: 0,
            curr: 0,
            start: 0,
        }
    }

    fn consume_integer(&mut self) {
        self.curr = match self.src[self.curr..].find(|c: char| !c.is_ascii_digit()) {
            Some(end) => self.curr + end,
            None => self.src.len()
        };
    }

    fn consume_id(&mut self) {
        self.curr = match self.src[self.curr..].find(|c: char| !(c.is_ascii_alphanumeric())) {
            Some(end) => self.curr + end,
            None => self.src.len()
        };
    }

    fn consume_string(&mut self) -> Result<(), LexerError> {
        match self.src[self.curr+1..].find('"') {
            Some(end) => {
                self.curr += end+2;
                Ok(())
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
                    self.curr += 1;
                    if c == '\n' {
                        self.line_start = self.curr;
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

    fn match_char(&mut self, e: char) -> bool {
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
                coln: self.start - self.line_start + 1,
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
            '-' => {
                if self.match_char('>') {
                    Token::RightArrow
                } else {
                    Token::Minus
                }
            },
            '/' => {
                if self.match_char('/') {
                    self.curr += self.src[self.curr..].find('\n')
                        .unwrap_or_else(|| self.src.len() - (self.curr + 1));
                    return self.next();
                } else {
                    Token::Slash
                }
            },
            '+' => Token::Plus,
            '*' => Token::Star,
            '=' => {
                if self.match_char('=') {
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            },
            '!' => {
                if self.match_char('=') {
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            },
            '<' => {
                if self.match_char('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                }
            },
            '>' => {
                if self.match_char('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            },
            '|' => {
                if self.match_char('|') {
                    Token::Or
                } else {
                    return Err(LexerError::new("there is no '|' operator", self))
                }
            },
            '&' => {
                if self.match_char('&') {
                    Token::And
                } else {
                    return Err(LexerError::new("there is no '|' operator", self))
                }
            },
            ':' => Token::Colon,
            ',' => Token::Comma,
            c if c.is_ascii_digit() => {
                self.curr -= 1;
                self.consume_integer();
                if self.match_char('.') {
                    self.consume_integer();
                    Token::Float(self.src[self.start..self.curr].parse::<f64>().unwrap())
                } else {
                    Token::Int(self.src[self.start..self.curr].parse::<i32>().unwrap())
                }
            },
            '"' => {
                self.curr -= 1;
                self.consume_string()?;
                Token::String
            },
            c if c.is_ascii_alphabetic() => {
                self.curr -= 1;
                self.consume_id();
                match &self.src[self.start..self.curr] {
                    "let" => Token::Let,
                    "print" => Token::Print,
                    "false" => Token::Bool(false),
                    "true" => Token::Bool(true),
                    "nil" => Token::Nil,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "while" => Token::While,
                    "break" => Token::Break,
                    "continue" => Token::Continue,
                    "return" => Token::Return,
                    "func" => Token::Func,
                    "Int" => Token::IntT,
                    "Float" => Token::FloatT,
                    "Str" => Token::StrT,
                    "Bool" => Token::BoolT,
                    _ => Token::Identifer
                }
            },
            _ => return Err(
                LexerError::new("invalid character", &self)
            )
        };
        Ok(TokenHeader {
            tokn: tok,
            coln: self.start - self.line_start + 1,
            line: self.line,
            lexm: String::from(&self.src[self.start..self.curr])
        })
    }

    pub fn peek(&mut self) -> Result<TokenHeader, LexerError> {
        let curr = self.curr;
        let line = self.line;
        let result = self.next();
        self.curr = curr;
        self.line = line;
        result
    }
}
