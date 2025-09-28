use crate::lexer::{
    Lexer,
    Token,
    TokenHeader
};

use crate::vm::{
    Op,
    Chunk
};

use std::error::Error;

pub struct Compiler {
    chunk: Chunk,
    lexer: Lexer,
}

enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary
}


#[derive(Debug)]
pub struct CompilerError {
    line: usize,
    coln: usize,
    text: String,
    message: &'static str,
}

impl CompilerError {
    pub fn new(message: &'static str, token: TokenHeader) -> CompilerError {
        CompilerError {
            line: token.line,
            coln: token.coln,
            text: token.lexeme,
            message,
        }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "at {}-{}: {}, got token {}", self.line, self.coln, self.message, self.text)
    }
}

impl std::error::Error for CompilerError {}

impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            chunk: Chunk::new(),
            lexer: Lexer::new(src),
        }
    }

    pub fn compile(&mut self) {
        self.expression();
    }
    
    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    /*
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,

    Unary,
>>>>>>> Stashed changes

    Call,

    Primary
    */

    fn integer(&mut self) {
    }

    fn group(&mut self) {
    }

    fn unary(&mut self) -> Result<(), CompilerError> {
        let op = self.lexer.next().unwrap_or_else(|e| {
            return Err(
                CompilerError::new("syntax error", e.)
            )
        });

        self.parse_precedence(Precedence::Unary);

        match self.lexer.next()?.val {
            Token::Minus => self.chunk.push_byte(Op::Neg as u8),
            t => return Err(
                CompilerError::new("invalid unary operator", t)
            )
        };
    }

    fn binary(&mut self) {
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
    }
}
