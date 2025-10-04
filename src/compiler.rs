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

type RuleFn = fn(&mut Compiler, cab_assign: bool) -> Result<(), CompilerError>;

struct Rule {
    prefix: Option<RuleFn>,
    infix: Option<RuleFn>,
    prec: Precedence,
}

struct Local {
    token: TokenHeader,
    scope_depth: usize,
}

// TODO: add panic mode
pub struct Compiler {
    chunk: Chunk,
    lexer: Lexer,
    curr: TokenHeader,

    locals: Vec<Local>,
    scope_depth: usize,
}

impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            lexer: Lexer::new(src),
            chunk: Chunk::new(),
            curr: TokenHeader { tokn: Token::EOF, coln: 0, line: 0, lexm: String::new() },
            locals: Vec::with_capacity(u8::MAX as usize),
            scope_depth: 0
        }
    }

    fn start_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        while self.locals.len() > 0 {
            if self.locals.last().unwrap().scope_depth >= self.scope_depth {
                self.locals.pop();
            } else {
                break;
            }
        }
        self.scope_depth -= 1;
    }

    pub fn compile(mut self) -> Result<Chunk, CompilerError> {
        loop {
            self.statement()?;
            if self.curr.tokn == Token::EOF { break; }
        }
        Ok(self.chunk)
    }

    // REFACTOR
    fn define_local(&mut self) -> Result<(), CompilerError> {
        if self.locals.len() > u8::MAX as usize {
            return Err(self.error("exceeded maximum number of local variable definitions."));
        }
        for local in self.locals.iter().rev() {
            if local.scope_depth < self.scope_depth {
                break;
            }
            if local.token.lexm == self.curr.lexm {
                return Err(self.error("variable identifiers must be unique within the current scope."));
            }
        }
        self.locals.push(Local {
            token: self.curr.clone(),
            scope_depth: self.scope_depth
        });
        Ok(())
    }

    // REFATOR
    fn resolve_local(&self, name: &str) -> i16 { // i16 to cover all [0-255]
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.token.lexm == name {
                return i as i16;
            }
        }
        -1
    }

    fn statement(&mut self) -> Result<(), CompilerError> {
        match self.peek()?.tokn {
            Token::Print => {
                self.next()?;
                self.expression()?;
                self.push_byte(Op::Print);
                self.expect(Token::SemiColon, "expected ';' after statement")?;
            },
            Token::LeftBrace => {
                self.next()?;
                self.start_scope();
                loop {
                    let next = self.peek()?;
                    if next.tokn == Token::RightBrace || next.tokn == Token::EOF {
                        break;
                    }
                    self.statement()?;
                }
                self.expect(Token::RightBrace, "expected trailing '}'")?;
                self.end_scope();
            },
            // REFACTOR
            Token::Let => {
                self.next()?;
                self.expect(Token::Identifer, "expected 'identifer' after 'let'")?;
                if self.scope_depth > 0 {
                    self.define_local()?;
                    if self.check(Token::Equal)? {
                        self.expression()?;
                    } else {
                        self.push_value(Value::Nil)?;
                    }
                    self.push_byte(Op::LDef);
                } else {
                    // TODO: fix -- allocating string on each variable declaration
                    self.push_value(Value::String(self.curr.lexm.clone()))?;
                    if self.check(Token::Equal)? {
                        self.expression()?;
                    } else {
                        self.push_value(Value::Nil)?;
                    }
                    self.push_byte(Op::GDef);
                }
                self.expect(Token::SemiColon, "expected ';' after statement")?;
            },
            Token::EOF => {
                self.next()?;
            },
            _ => {
                self.expression()?;
                self.push_byte(Op::Pop);
                self.expect(Token::SemiColon, "expected ';' after statement")?;
            }
        };
        Ok(())
    }

    fn expression(&mut self) -> Result<(), CompilerError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn literal(&mut self, _: bool) -> Result<(), CompilerError> {
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

    // REFACTOR
    fn variable(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        let offset = self.resolve_local(self.curr.lexm.as_str());
        if offset == -1 {
            self.push_value(Value::String(self.curr.lexm.clone()))?;
            if can_assign && self.check(Token::Equal)? {
                self.expression()?;
                self.push_byte(Op::GSet);
            } else {
                self.push_byte(Op::GGet);
            }
        } else {
            if can_assign && self.check(Token::Equal)? {
                self.expression()?;
                self.push_byte(Op::LSet(offset as u8));
            } else {
                self.push_byte(Op::LGet(offset as u8));
            }
        } 
        Ok(())
    }

    fn group(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightParen, "expected closing ')'")?;
        Ok(())
    }

    fn unary(&mut self, _: bool) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(Precedence::Unary)?;
        self.push_byte(match op_tok {
            Token::Minus => Op::Neg,
            Token::Bang => Op::Not,
            _ => panic!("Compiler::unary ~ invalid unary operator")
        });
        Ok(())
    }

    fn binary(&mut self, _: bool) -> Result<(), CompilerError> {
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
            _ => return Err(self.error("invalid binary operator"))
        };
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), CompilerError> {
        self.next()?;
        match self.get_rule(self.curr.tokn).prefix {
            Some(prefix) => {
                let can_assign = precedence <= Precedence::Assignment;
                prefix(self, can_assign)?;
                loop {
                    let infix_tok = self.peek()?;
                    let infix_rule = self.get_rule(infix_tok.tokn);
                    if precedence > infix_rule.prec { break; }
                    self.next()?;
                    infix_rule.infix.unwrap()(self, can_assign)?;
                }
                if can_assign && self.peek()?.tokn == Token::Equal {
                    return Err(self.error("invalid assignment target"));
                }
            },
            None => return Err(self.error("expected expression"))
        }
        Ok(())
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
