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

#[derive(PartialEq, PartialOrd)]
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

type RuleFn = fn(&mut Compiler) -> Result<(), CompilerError>;

struct Rule {
    prefix: Option<RuleFn>,
    infix: Option<RuleFn>,
    prec: Precedence
}

pub struct Compiler {
    chunk: Chunk,
    lexer: Lexer,
    curr: Token,
}

impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            lexer: Lexer::new(src),
            chunk: Chunk::new(),
            curr: Token::EOF
        }
    }

    pub fn compile(mut self) -> Result<Chunk, CompilerError> {
        self.expression()?;
        Ok(self.chunk)
    }

    fn get_rule(&self, tok: Token) -> Rule {
        match tok {
            Token::LeftParen => Rule {
                prefix: Some(Compiler::group),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::LiteralInt(_) => Rule {
                prefix: Some(Compiler::integer),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Minus => Rule {
                prefix: Some(Compiler::unary),
                infix: Some(Compiler::binary),
                prec: Precedence::Term,
            },
            Token::Plus => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Term,
            },
            Token::Slash | Token::Star => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Factor,
            },
            _ => Rule {
                prefix: None,
                infix: None,
                prec: Precedence::None,
            }
        }
    }

    fn next(&mut self) -> Result<TokenHeader, CompilerError> {
        match self.lexer.next() {
            Ok(tok) => {
                self.curr = tok.valu;
                Ok(tok)
            },
            Err(e) => {
                Err(CompilerError::new(e.line, e.coln, e.mssg, e.lexm))
            }
        }
    }

    fn expect(&mut self, what: Token, msg: &'static str) -> Result<(), CompilerError> {
        let tok = self.next()?;
        if tok.valu != what {
            Err(CompilerError::from_tok(tok, msg))
        } else {
            Ok(())
        }
    }

    fn expression(&mut self) -> Result<(), CompilerError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn integer(&mut self) -> Result<(), CompilerError> {
        match self.curr {
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
        let op_tok = self.curr;
        self.parse_precedence(Precedence::Unary);
        match op_tok {
            Token::Minus => self.chunk.push_byte(Op::Neg as u8),
            _ => panic!("Compiler::unary ~ invalid unary operator")
        };
        Ok(())
    }

    fn binary(&mut self) -> Result<(), CompilerError> {
        let op_tok = self.curr;
        self.parse_precedence(self.get_rule(op_tok).prec);
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
    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), CompilerError> {
        let prefix_tok = self.next()?;
        match self.get_rule(prefix_tok.valu).prefix {
            Some(prefix) => {
                prefix(self)?;
                loop {
                    let infix_tok = self.next()?;
                    let infix_rule = self.get_rule(infix_tok.valu);
                    if precedence > infix_rule.prec { break; }
                    infix_rule.infix.unwrap()(self)?;
                }
            },
            None => return Err(
                CompilerError::from_tok(prefix_tok, "expected prefix rule")
            )
        }
        Ok(())
    }
}


#[derive(Debug)]
struct CompilerError {
    line: usize,
    coln: usize,
    mssg: &'static str,
    lexm: String,
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

    pub fn new(line: usize, coln: usize, mssg: &'static str, lexm: String) -> Self {
        Self { line, coln, mssg, lexm }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "at {}-{}: {}, at {}", self.line, self.coln, self.mssg, self.lexm)
    }
}

impl std::error::Error for CompilerError {}
