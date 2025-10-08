use crate::lexer::{
    Lexer,
    Token,
    TokenHeader,
};

use crate::vm::{
    ByteCode,
    Constant,
    Chunk,
    VarType,
};

const Max_LocalBindings: usize = u8::MAX as usize;

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

pub struct LoopState {
    in_loop: bool,
    start: usize,
    break_jumps: Vec<usize>,
}

impl LoopState {
    fn new() -> Self {
        Self {
            start: 0,
            in_loop: false,
            break_jumps: Vec::new(),
        }
    }

    fn start_loop(&mut self, start: usize) {
        self.in_loop = true;
        self.start = start;
        unsafe {
            self.break_jumps.set_len(0);
        }
    }

    fn end_loop(&mut self) {
        self.in_loop = false;
        self.start = 0;
    }
}

#[allow(dead_code)]
pub struct Func {
    arity: usize,
    chunk: Chunk,
    name: String,
    retype: VarType,
}

impl Func {
    fn new(name: String) -> Self {
        Self {
            name,
            arity: 0,
            chunk: Chunk::new(),
            retype: VarType::None,
        }
    }
}

// TODO: add panic mode
pub struct Compiler {
    lexer: Lexer,
    curr: TokenHeader,
    had_error: bool,
    loop_state: LoopState,
    locals: Vec<Local>,
    scope_depth: usize,

    func: Func,
}

// public methods
impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            lexer: Lexer::new(src),
            curr: TokenHeader { tokn: Token::EOF, coln: 0, line: 0, lexm: String::new() },
            scope_depth: 0,

            loop_state: LoopState::new(),
            had_error: false,

            func: Func {
                params: Vec::new(),
                chunk: Chunk::new(),
                name: String::from("main"),
                retype: VarType::None,
            }, 
            locals: Vec::with_capacity(u8::MAX as usize),
        }
    }

    pub fn compile(mut self) -> Result<Chunk, Vec<CompilerError>> {
        let mut errs = Vec::new();
        loop {
            match self.declaration() {
                Ok(_) => (),
                Err(err) => {
                    errs.push(err);
                    self.had_error = true;
                }
            };
            if self.curr.tokn == Token::EOF { break; }
        }
        if self.had_error {
            Err(errs)
        } else {
            Ok(self.func.chunk)
        }
    }
}

// grammar agnostic methods
impl Compiler {
    fn declaration(&mut self) -> Result<(), CompilerError> {
        match self.peek()?.tokn {
            Token::Let => {
                self.next()?;
                self.compile_let()?;
            },
            Token::Func => {
                self.next()?;
                self.compile_func()?;
            },
            _ => {
                return self.statement();
            }
        };
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        Ok(())
    }

    fn statement(&mut self) -> Result<(), CompilerError> {
        match self.peek()?.tokn {
            Token::If => {
                self.next()?;
                self.compile_if()?;
            },
            Token::While => {
                self.next()?;
                self.compile_while()?;
            },
            Token::Print => {
                self.next()?;
                self.compile_print()?;
            },
            Token::LeftBrace => {
                self.next()?;
                self.compile_block()?;
            },
            Token::Break => {
                self.next()?;
                self.compile_break()?;
            },
            Token::Continue => {
                self.next()?;
                self.compile_continue()?;
            },
            Token::EOF => {
                self.next()?;
            },
            _ => {
                self.expression()?;
                self.expect(Token::SemiColon, "expected ';' after statement")?;
                self.func.chunk.push_byte(ByteCode::Pop);
            }
        };
        Ok(())
    }

    fn expression(&mut self) -> Result<(), CompilerError> {
        self.parse_precedence(Precedence::Assignment)
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
                    if let Some(infix) = infix_rule.infix {
                        self.next()?;
                        infix(self, can_assign)?;
                    } else {
                        return Err(self.error("invalid expression"));
                    }
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
                prefix: Some(Compiler::identifer),
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
}

// non-expression statement grammar 
impl Compiler {
    fn compile_let(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "expected identifer")?;
        let byte = if self.scope_depth > 0 {
            self.push_local()?;
            ByteCode::LDef
        } else {
            self.load_const(Constant::String(self.curr.lexm.clone()))?;
            ByteCode::GDef
        };
        if self.check(Token::Equal)? {
            self.expression()?;
        } else {
            self.load_const(Constant::Nil)?;
        }
        self.func.chunk.push_byte(byte);
        Ok(())
    }

    fn compile_func(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "expected function name")?;
        self.func = Func::new(self.curr.lexm.clone());
        self.func.arity = self.compile_params()?;
        if self.check(Token::RightArrow)? {
            self.func.retype = self.compile_type()?;
        }
        self.compile_forced_block()?;
        Ok(())
    }

    fn compile_params(&mut self) -> Result<usize, CompilerError> {
        let mut count = 0;
        self.expect(Token::RightParen, "expected disclosing '('")?;
        loop {
            let curr = self.peek()?.tokn;
            if curr == Token::RightParen || curr == Token::EOF {
                break;
            }
            // TODO: push as local variable
            self.expect(Token::Identifer, "expected parameter name")?;
            self.expect(Token::Colon, "expected ':'")?;
            self.compile_type()?;
            count += 1;
        }
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(count)
    }

    fn compile_type(&mut self) -> Result<VarType, CompilerError> {
        self.expect(Token::Identifer, "expected type")?;
        Ok(match self.curr.tokn {
            Token::Int_T   => VarType::Integer,
            Token::Float_T => VarType::Float,
            Token::Bool_T  => VarType::Bool,
            Token::Str_T   => VarType::String,
            _ => return Err(self.error("got unexpected type")),
        })
    }

    fn compile_while(&mut self) -> Result<(), CompilerError> {
        self.loop_state.start_loop(self.func.chunk.get_count());
        self.expression()?;
        let loop_cond = self.func.chunk.push_byte(ByteCode::JumpIfFalse(0));
        self.compile_forced_block()?;
        self.func.chunk.push_byte(ByteCode::Jump(self.loop_state.start));
        self.patch_fjump(loop_cond);
        for jump in self.loop_state.break_jumps.iter() {
            self.func.chunk.set_byte(*jump, ByteCode::Jump(self.func.chunk.get_count()));
        }
        self.loop_state.end_loop();
        Ok(())
    }

    fn compile_if(&mut self) -> Result<(), CompilerError> {
        self.expression()?;
        let if_start = self.func.chunk.push_byte(ByteCode::JumpIfFalse(0));
        self.compile_forced_block()?;
        let if_end = self.func.chunk.push_byte(ByteCode::Jump(0));
        self.patch_fjump(if_start);
        if self.check(Token::Else)? {
            if self.check(Token::If)? {
                self.compile_if()?;
            } else {
                self.compile_forced_block()?;
            }
        }
        self.patch_jump(if_end);
        Ok(())
    }

    fn compile_forced_block(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::LeftBrace, "expected enclosing '{'")?;
        self.compile_block()
    }

    fn compile_block(&mut self) -> Result<(), CompilerError> {
        self.start_scope();
        loop {
            let next = self.peek()?.tokn;
            if next == Token::RightBrace || next == Token::EOF {
                break;
            }
            self.declaration()?;
        }
        self.end_scope();
        self.expect(Token::RightBrace, "expected trailing '}'")?;
        Ok(())
    }

    fn compile_break(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        if !self.loop_state.in_loop {
            return Err(self.error("'break' can only be used inside a loop"));
        }
        let jump = self.func.chunk.push_byte(ByteCode::Jump(0));
        self.loop_state.break_jumps.push(jump);
        Ok(())
    }

    fn compile_continue(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        if !self.loop_state.in_loop {
            return Err(self.error("'continue' can only be used inside a loop"));
        }
        self.func.chunk.push_byte(ByteCode::Jump(self.loop_state.start));
        Ok(())
    }

    fn compile_print(&mut self) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        self.func.chunk.push_byte(ByteCode::Print);
        Ok(())
    }
}

// expression statement grammar
impl Compiler {
    fn literal(&mut self, _: bool) -> Result<(), CompilerError> {
        // TODO: handle result
        match self.curr.tokn {
            Token::Int(val) => self.load_const(Constant::Integer(val))?,
            Token::Float(val) => self.load_const(Constant::Float(val))?,
            Token::Bool(val) => self.load_const(Constant::Bool(val))?,
            Token::String => {
                let s = &self.curr.lexm[1..self.curr.lexm.len()-1];
                self.load_const(Constant::String(s.into()))?
            },
            Token::Nil => self.load_const(Constant::Nil)?,
            _ => panic!("Compiler::literal ~ unhandled literal {:?}", self.curr)
        };
        Ok(())
    }

    // REFACTOR
    fn identifer(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        let offset = self.resolve_local(self.curr.lexm.as_str());
        if offset == -1 {
            self.load_const(Constant::String(self.curr.lexm.clone()))?;
            if can_assign && self.check(Token::Equal)? {
                self.expression()?;
                self.func.chunk.push_byte(ByteCode::GSet);
            } else {
                self.func.chunk.push_byte(ByteCode::GGet);
            }
        } else {
            if can_assign && self.check(Token::Equal)? {
                self.expression()?;
                self.func.chunk.push_byte(ByteCode::LSet(offset as u8));
            } else {
                self.func.chunk.push_byte(ByteCode::LGet(offset as u8));
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
        self.func.chunk.push_byte(match op_tok {
            Token::Minus => ByteCode::Neg,
            Token::Bang => ByteCode::Not,
            _ => panic!("Compiler::unary ~ invalid unary operator")
        });
        Ok(())
    }

    fn binary(&mut self, _: bool) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(self.get_rule(op_tok).prec)?;
        match op_tok {
            Token::Minus => self.func.chunk.push_byte(ByteCode::Sub),
            Token::Slash => self.func.chunk.push_byte(ByteCode::Div),
            Token::Plus => self.func.chunk.push_byte(ByteCode::Add),
            Token::Star => self.func.chunk.push_byte(ByteCode::Mul),
            Token::Or => self.func.chunk.push_byte(ByteCode::Or),
            Token::And => self.func.chunk.push_byte(ByteCode::And),
            Token::EqualEqual => self.func.chunk.push_byte(ByteCode::Equal),
            Token::BangEqual => self.func.chunk.push_bytes(ByteCode::Equal, ByteCode::Not),
            Token::Greater => self.func.chunk.push_byte(ByteCode::Greater),
            Token::Less => self.func.chunk.push_byte(ByteCode::Less),
            Token::GreaterEqual => self.func.chunk.push_bytes(ByteCode::Less, ByteCode::Not),
            Token::LessEqual => self.func.chunk.push_bytes(ByteCode::Greater, ByteCode::Not),
            _ => return Err(self.error("invalid binary operator"))
        };
        Ok(())
    }
}

// helpers
impl Compiler {
    fn start_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        while self.locals.len() > 0 {
            if self.locals.last().unwrap().scope_depth >= self.scope_depth {
                self.locals.pop();
                self.func.chunk.push_byte(ByteCode::Pop);
            } else {
                break;
            }
        }
        self.scope_depth -= 1;
    }

    // requires identifier to be parsed
    fn push_local(&mut self) -> Result<(), CompilerError> {
        if self.locals.len() > Max_LocalBindings {
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

    fn patch_jump(&mut self, idx: usize) {
        self.func.chunk.set_byte(idx, ByteCode::Jump(self.func.chunk.get_count()));
    }

    fn patch_fjump(&mut self, idx: usize) {
        self.func.chunk.set_byte(idx, ByteCode::JumpIfFalse(self.func.chunk.get_count()));
    }

    fn load_const(&mut self, v: Constant) -> Result<u8, CompilerError> {
        match self.func.chunk.load_const(v) {
            Ok(idx) => Ok(idx),
            Err(e) => Err(self.error(e))
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
        if self.peek()?.tokn == what {
            self.next()
        } else {
            Err(self.error(msg))
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
