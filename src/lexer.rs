#[derive(Debug)]
pub struct Lexer {
    src: String,
    curr: usize,
    start: usize,
    line: usize,
    filename: String,
}

#[derive(Debug)]
pub struct TokenHeader {
    pub val: Token,
    pub pos: usize,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
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

    Identifer(String),
    LiteralString(String),
    LiteralInt(i64),
    TypeInteger,
    TypeString,

    EOF,
}

#[derive(Debug)]
pub struct LexerError {
    filename: String,
    line: usize,
    pos: usize,
    message: String,
}

impl LexerError {
    pub fn new(message: String, lexer: &Lexer) -> LexerError {
        LexerError {
            filename: lexer.filename.clone(), // TODO: heap allocation going BRRR
            line: lexer.line,
            pos: lexer.curr,
            message,
        }
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} at {}-{}: {}", self.filename, self.line, self.pos, self.message)
    }
}

impl std::error::Error for LexerError {}

impl Lexer {
    pub fn new(filename: String, src: String) -> Lexer {
        Lexer {
            src,
            filename,
            curr: 0,
            start: 0,
            line: 0,
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
                LexerError::new(String::from("expected trailing \""), &self)
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
                val: Token::EOF,
                pos: self.start,
                line: self.line
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
            ':' => Token::Colon,
            c if c.is_ascii_digit() => {
                self.curr -= 1;
                Token::LiteralInt(self.consume_integer())
            },
            '"' => {
                self.curr -= 1;
                Token::LiteralString(self.consume_string()?)
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
                    "string" => Token::TypeString,
                    _ => Token::Identifer(id)
                }
            },
            c => return Err(
                LexerError::new(format!("invalid character {}", c), &self)
            )
        };
        Ok(TokenHeader {
            val: tok,
            pos: self.start,
            line: self.line
        })
    }

    pub fn peak(&mut self) -> Result<TokenHeader, LexerError> {
        let (start, curr) = (self.start, self.curr);
        let tok = self.next()?;
        self.start = start; self.curr = curr;
        Ok(tok)
    }
}