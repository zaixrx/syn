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
    prec: Precedence,
}

// TODO: add panic mode
pub struct Compiler {
    chunk: Chunk,
    lexer: Lexer,
    curr: TokenHeader,
}

impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            lexer: Lexer::new(src),
            chunk: Chunk::new(),
            curr: TokenHeader { tokn: Token::EOF, coln: 0, line: 0, lexm: String::new() }
        }
    }

    fn get_rule(&self, tok: Token) -> Rule {
        match tok {
            Token::LeftParen => Rule {
                prefix: Some(Compiler::group),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Identifer => Rule {
                prefix: Some(Compiler::variable),
                infix: None,
                prec: Precedence::Primary
            },
            Token::Int(_) => Rule {
                prefix: Some(Compiler::literal),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Float(_) => Rule {
                prefix: Some(Compiler::literal),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::String => Rule {
                prefix: Some(Compiler::literal),
                infix: None,
                prec: Precedence::Primary
            },
            Token::Bool(_) => Rule {
                prefix: Some(Compiler::literal),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Nil => Rule {
                prefix: Some(Compiler::literal),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Bang => Rule {
                prefix: Some(Compiler::unary),
                infix: None,
                prec: Precedence::Unary,
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
            Token::Or => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Or,
            },
            Token::And => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::And,
            },
            Token::EqualEqual | Token::BangEqual => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Equality,
            },
            Token::Greater | Token::Less |
            Token::GreaterEqual | Token::LessEqual => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Comparison,
            },
            _ => Rule {
                prefix: None,
                infix: None,
                prec: Precedence::None,
            }
        }
    }

    fn next(&mut self) -> Result<(), CompilerError> {
        match self.lexer.next() {
            Ok(tok) => {
                self.curr = tok;
                Ok(())
            },
            Err(e) => {
                Err(CompilerError::new(e.line, e.coln, e.mssg, e.lexm))
            }
        }
    }

    fn peek(&mut self) -> Result<TokenHeader, CompilerError> {
        match self.lexer.peek() {
            Ok(tok) => Ok(tok),
            Err(e) => Err(CompilerError::new(e.line, e.coln, e.mssg, e.lexm))
        }
    }

    fn expect(&mut self, what: Token, msg: &'static str) -> Result<(), CompilerError> {
        self.next()?;
        if self.curr.tokn != what {
            Err(self.error(msg))
        } else {
            Ok(())
        }
    }

    fn check(&mut self, what: Token) -> Result<bool, CompilerError> {
        if self.peek()?.tokn == what {
            self.next()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn compile(mut self) -> Result<Chunk, CompilerError> {
        loop {
            self.statement()?;
            if self.curr.tokn == Token::EOF { break; }
        }
        Ok(self.chunk)
    }

    fn statement(&mut self) -> Result<(), CompilerError> {
        match self.peek()?.tokn {
            Token::Print => {
                self.next()?;
                self.expression()?;
                self.push_byte(Op::Print);
            },
            Token::Let => {
                self.next()?;
                self.expect(Token::Identifer, "expected 'identifer' after 'let'")?;
                // TODO: this needs to be handled by GC
                let idx = self.push_value(Value::String(self.curr.lexm.clone()))?;
                if self.check(Token::Equal)? {
                    self.expression()?;
                } else {
                    self.push_value(Value::Nil)?;
                }
                self.push_byte(Op::GDef(idx));
            },
            Token::EOF => {
                self.next()?;
                return Ok(()); // TODO: push RET byte
            },
            _ => {
                self.expression()?;
                self.push_byte(Op::Pop)
            }
        };
        self.expect(Token::SemiColon, "expected ';' after statement")
    }

    fn expression(&mut self) -> Result<(), CompilerError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn literal(&mut self) -> Result<(), CompilerError> {
        // TODO: handle result
        match self.curr.tokn {
            Token::Int(val) => self.push_value(Value::Integer(val))?,
            Token::Float(val) => self.push_value(Value::Float(val))?,
            Token::Bool(val) => self.push_value(Value::Bool(val))?,
            Token::String => {
                let s = &self.curr.lexm[1..self.curr.lexm.len()-1];
                self.push_value(Value::String(s.into()))?
            },
            Token::Nil => self.push_value(Value::Nil)?,
            _ => panic!("Compiler::literal ~ unhandled literal {:?}", self.curr)
        };
        Ok(())
    }

    fn variable(&mut self) -> Result<(), CompilerError> {
        let idx = self.push_value(Value::String(self.curr.lexm.clone()))?;
        if self.check(Token::Equal)? {
            self.expression()?;
            self.push_byte(Op::GSet(idx));
        } else {
            self.push_byte(Op::GGet(idx));
        }
        Ok(())
    }

    fn group(&mut self) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightParen, "expected closing ')'")?;
        Ok(())
    }

    fn unary(&mut self) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(Precedence::Unary)?;
        self.push_byte(match op_tok {
            Token::Minus => Op::Neg,
            Token::Bang => Op::Not,
            _ => panic!("Compiler::unary ~ invalid unary operator")
        });
        Ok(())
    }

    fn binary(&mut self) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(self.get_rule(op_tok).prec)?;
        match op_tok {
            Token::Minus => self.push_byte(Op::Sub),
            Token::Slash => self.push_byte(Op::Div),
            Token::Plus => self.push_byte(Op::Add),
            Token::Star => self.push_byte(Op::Mul),
            Token::Or => self.push_byte(Op::Or),
            Token::And => self.push_byte(Op::And),
            Token::EqualEqual => self.push_byte(Op::Equal),
            Token::BangEqual => self.push_bytes(Op::Equal, Op::Not),
            Token::Greater => self.push_byte(Op::Greater),
            Token::Less => self.push_byte(Op::Less),
            Token::GreaterEqual => self.push_bytes(Op::Less, Op::Not),
            Token::LessEqual => self.push_bytes(Op::Greater, Op::Not),
            _ => panic!("Compiler::binary ~ invalid binary operator")
        };
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), CompilerError> {
        self.next()?;
        match self.get_rule(self.curr.tokn).prefix {
            Some(prefix) => {
                prefix(self)?;
                loop {
                    let infix_tok = self.peek()?;
                    let infix_rule = self.get_rule(infix_tok.tokn);
                    if precedence > infix_rule.prec { break; }
                    self.next()?;
                    infix_rule.infix.unwrap()(self)?;
                }
            },
            None => return Err(self.error("expected prefix rule"))
        }
        Ok(())
    }

    fn push_byte(&mut self, b: Op) {
        self.chunk.push_byte(b, (self.curr.line, self.curr.coln))
    }

    fn push_bytes(&mut self, b1: Op, b2: Op) {
        self.chunk.push_bytes(b1, (self.curr.line, self.curr.coln), b2, (self.curr.line, self.curr.coln))
    }

    fn push_value(&mut self, v: Value) -> Result<u8, CompilerError> {
        match self.chunk.push_value(v, (self.curr.line, self.curr.coln)) {
            Ok(idx) => Ok(idx),
            Err(e) => Err(self.error(e))
        }
    }

    fn error(&self, msg: &'static str) -> CompilerError {
        CompilerError::from_tok(&self.curr, msg)
    }
}


#[derive(Debug)]
pub struct CompilerError {
    line: usize,
    coln: usize,
    mssg: &'static str,
    lexm: String,
}

impl CompilerError {
    pub fn from_tok(tok: &TokenHeader, mssg: &'static str) -> Self {
        Self {
            line: tok.line,
            coln: tok.coln,
            lexm: match tok.tokn {
                Token::EOF => String::from("end"),
                _ => tok.lexm.clone()
            },
            mssg,
        }
    }

    pub fn new(line: usize, coln: usize, mssg: &'static str, lexm: String) -> Self {
        Self { line, coln, mssg, lexm }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "at {}-{}: {} at {}", self.line, self.coln, self.mssg, self.lexm)
    }
}

impl std::error::Error for CompilerError {}
