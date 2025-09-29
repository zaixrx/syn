use crate::lexer::{
    Lexer,
    Token,
    TokenHeader
};

use crate::vm::{
    Op,
    Value,
    Chunk
};

/*
Assignment:
    /

Binary:
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,

Unary:
    [Bang, Minus]

Call:
    [Dot annotation, Bracket annotation, Parenthensis calls]

Primary:
    [Literals, Identifiers, Group]

*/

#[derive(Debug)]
struct CompilerError {
    line: usize,
    coln: usize,
    lexm: String,
    mssg: &'static str,
}

impl CompilerError {
    pub fn from_tok(tok: TokenHeader, mssg: &'static str) -> Self {
        Self {
            line: tok.line,
            coln: tok.coln,
            lexm: tok.lexm,
            mssg,
        }
    }

    pub fn new(line: usize, coln: usize, lexm: String, mssg: &'static str) -> Self {
        Self { line, coln, lexm, mssg }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "at {}-{}: {}, at {}", self.line, self.coln, self.mssg, self.lexm)
    }
}

impl std::error::Error for CompilerError {}

pub struct Compiler {
    chunk: Chunk,
    lexer: Lexer,
    prev: Token,
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

impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            chunk: Chunk::new(),
            lexer: Lexer::new(src),
            prev: Token::EOF,
        }
    }

    pub fn compile(mut self) -> Result<Chunk, CompilerError> {
        self.expression()?;
        Ok(self.chunk)
    }

    fn get_rule(&self, tok: Token) -> Option<Precedence> {
        match tok {
            Token::Minus | Token::Plus => Some(Precedence::Term),
            Token::Slash | Token::Star => Some(Precedence::Factor),
            _ => None
        }
    }

    fn expect(&mut self, what: Token, msg: &'static str) -> Result<(), CompilerError> {
        match self.lexer.next() {
            Ok(tok) => {
                if tok.valu != what {
                    Err(CompilerError::from_tok(tok, msg))
                } else {
                    Ok(())
                }
            },
            Err(e) => Err(
                // TODO: ensure better error handiling
                CompilerError::new(e.line, e.coln, e.lexm, e.as_str())
            )
        }
    }

    fn expression(&mut self) -> Result<(), CompilerError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn integer(&mut self) -> Result<(), CompilerError> {
        match self.prev {
            Token::LiteralInt(val) => self.chunk.push_val(Value::Integer(val)),
            _ => panic!("Compiler::integer ~ expected integer")
        };
        Ok(())
    }

    fn group(&mut self) -> Result<(), CompilerError> {
        self.parse_precedence(Precedence::Primary);
        Ok(self.expect(Token::RightParen, "expected closing ')'")?)
    }

    fn unary(&mut self) -> Result<(), CompilerError> {
        let op_tok = self.prev;
        self.parse_precedence(Precedence::Unary);
        match op_tok {
            Token::Minus => self.chunk.push_byte(Op::Neg as u8),
            _ => panic!("Compiler::unary ~ invalid unary operator")
        };
        Ok(())
    }

    fn binary(&mut self) -> Result<(), CompilerError> {
        let op_tok = self.prev;
        let prec: Precedence = self.get_rule(op_tok).unwrap_or_else(|| {
            panic!("Compiler::binary ~ invalid binary operator")
        });
        self.parse_precedence(prec);
        match op_tok {
            Token::Minus => self.chunk.push_byte(Op::Sub as u8),
            Token::Slash => self.chunk.push_byte(Op::Div as u8),
            Token::Plus => self.chunk.push_byte(Op::Add as u8),
            Token::Star => self.chunk.push_byte(Op::Mul as u8),
            _ => panic!("Compiler::binary ~ invalid binary operator")
        };
        Ok(())
    }

    // expression: unary [binary]
    // binary: unary [- | / | + | *] unary
    fn parse_precedence(&mut self, prec: Precedence) -> Result<(), CompilerError> {
        Ok(())
    }
}
