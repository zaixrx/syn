// TODO: make global variables work
use crate::lexer::{
    Lexer,
    Token,
    TokenHeader,
};

use crate::vm::{
    Func,
    Chunk,
    ByteCode,
    Constant,
    VarType,
};

pub struct Compiler {
    lexer: Lexer,
    curr: TokenHeader,
    loop_state: LoopState,
    locals: Vec<Local>,
    scope_depth: usize,
    curr_chunk: Option<Chunk>,
    had_error: bool,
    panic_mode: bool,
    prog: Chunk
}

struct Local {
    token: TokenHeader,
    scope_depth: usize,
}

struct LoopState {
    in_loop: bool,
    start: usize,
    break_jumps: Vec<usize>,
}

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

pub type ArgsCount = u8;
pub type LocalsCount = u8;

// public methods
impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            lexer: Lexer::new(src),
            curr: TokenHeader { tokn: Token::EOF, coln: 0, line: 0, lexm: String::new() },
            scope_depth: 0,
            loop_state: LoopState::new(),
            locals: Vec::with_capacity(LocalsCount::MAX as usize),
            curr_chunk: None,
            prog: Chunk::new(),
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(mut self) -> Result<Chunk, Vec<CompilerError>> {
        let mut errs = Vec::new();
        loop {
            match self.declaration() {
                Ok(_) => (),
                Err(err) => {
                    if !self.panic_mode {
                        errs.push(err);
                        self.had_error = true;
                        self.panic_mode = true;
                    }
                }
            };
            if self.curr.tokn == Token::EOF { break; }
        }
        if self.had_error {
            Err(errs)
        } else {
            if let Err(msg) = self.prog.load_const(Constant::String(String::from("main"))) {
                return Err(vec![self.error(msg)]);
            }
            self.prog.push(ByteCode::Call(0));
            Ok(self.prog)
        }
    }

    fn synchronize(&mut self) -> bool {
        self.panic_mode = false;
        let mut prev;
        loop {
            if self.curr.tokn == Token::EOF {
                return true;
            }
            prev = match self.peek() {
                Ok(t) => {
                    t.tokn
                },
                Err(_) => {
                    return false
                }
            };
            match prev {
                Token::Func   |
                Token::Let    |
                Token::If     |
                Token::While  |
                Token::Print  |
                Token::Return => return true,
                _ => ()
            };
            if self.next().is_err() {
                return false;
            };
            if prev == Token::SemiColon {
                return true;
            }
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
        if self.panic_mode {
            self.synchronize();
        }
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
                self.push_bytecode(ByteCode::Pop)?;
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
                prec: Precedence::Call,
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
        self.push_bytecode(byte)?;
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        Ok(())
    }

    fn compile_func(&mut self) -> Result<(), CompilerError> {
        if self.curr_chunk.is_some() {
            return Err(self.error("can't have nested functions"))
        }
        self.curr_chunk = Some(Chunk::new());
        self.expect(Token::Identifer, "expected the function's name")?;
        let name = self.curr.lexm.clone();
        let arity = self.compile_params()?;
        let retype = if self.check(Token::RightArrow)? {
            self.compile_type()?
        } else {
            VarType::None
        };
        self.compile_forced_block()?;
        self.push_bytecode(ByteCode::Ret)?;
        let chunk = self.curr_chunk.take().unwrap();
        match self.prog.load_const(Constant::String(name.clone())) {
            Ok(idx) => idx,
            Err(msg) => return Err(self.error(msg))
        };
        match self.prog.load_const(Constant::Function(Func::new(name, arity, retype, chunk))) {
            Ok(idx) => idx,
            Err(msg) => return Err(self.error(msg))
        };
        self.prog.push(ByteCode::GDef);
        Ok(())
    }

    fn compile_params(&mut self) -> Result<usize, CompilerError> {
        let mut count = 0;
        self.expect(Token::LeftParen, "expected disclosing '('")?;
        loop {
            let curr = self.peek()?.tokn;
            if curr == Token::RightParen || curr == Token::EOF {
                break;
            }
            if count > 0 {
                self.expect(Token::Comma, "expected ',' param seperator")?;
            }
            // TODO: push as local variable
            self.expect(Token::Identifer, "expected param name")?;
            self.push_local()?; // the next step in defining a local variable is at runtime
                                // (loading it's value in the VM's stack)
            self.expect(Token::Colon, "expected ':' param type seperator")?;
            self.compile_type()?;
            count += 1;
        }
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(count)
    }

    fn compile_type(&mut self) -> Result<VarType, CompilerError> {
        let typ = match self.peek()?.tokn {
            Token::IntT   => VarType::Integer,
            Token::FloatT => VarType::Float,
            Token::BoolT  => VarType::Bool,
            Token::StrT   => VarType::String,
            _ => return Err(self.error("expected valid type")),
        };
        self.next()?;
        Ok(typ)
    }

    fn compile_while(&mut self) -> Result<(), CompilerError> {
        self.loop_state.start_loop(self.bytecode_count()?);
        self.expression()?;
        let loop_cond = self.push_bytecode(ByteCode::JumpIfFalse(0))?;
        self.compile_forced_block()?;
        self.push_bytecode(ByteCode::Jump(self.loop_state.start))?;
        self.patch_fjump(loop_cond)?;
        for i in 0..self.loop_state.break_jumps.len() {
            self.set_bytecode(self.loop_state.break_jumps[i], ByteCode::Jump(self.bytecode_count()?))?;
        }
        self.loop_state.end_loop();
        Ok(())
    }

    fn compile_if(&mut self) -> Result<(), CompilerError> {
        self.expression()?;
        let if_start = self.push_bytecode(ByteCode::JumpIfFalse(0))?;
        self.compile_forced_block()?;
        let if_end = self.push_bytecode(ByteCode::Jump(0))?;
        self.patch_fjump(if_start)?;
        if self.check(Token::Else)? {
            if self.check(Token::If)? {
                self.compile_if()?;
            } else {
                self.compile_forced_block()?;
            }
        }
        self.patch_jump(if_end)?;
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
        self.end_scope()?;
        self.expect(Token::RightBrace, "expected trailing '}'")?;
        Ok(())
    }

    fn compile_break(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        if !self.loop_state.in_loop {
            return Err(self.error("'break' can only be used inside a loop"));
        }
        let jump = self.push_bytecode(ByteCode::Jump(0))?;
        self.loop_state.break_jumps.push(jump);
        Ok(())
    }

    fn compile_continue(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        if !self.loop_state.in_loop {
            return Err(self.error("'continue' can only be used inside a loop"));
        }
        self.push_bytecode(ByteCode::Jump(self.loop_state.start))?;
        Ok(())
    }

    fn compile_print(&mut self) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        self.push_bytecode(ByteCode::Print)?;
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
                self.push_bytecode(ByteCode::GSet)?;
            } else if self.check(Token::LeftParen)? {
                self.call(can_assign)?;
            } else {
                self.push_bytecode(ByteCode::GGet)?;
            }
        } else {
            if can_assign && self.check(Token::Equal)? {
                self.expression()?;
                self.push_bytecode(ByteCode::LSet(offset as u8))?;
            } else {
                self.push_bytecode(ByteCode::LGet(offset as u8))?;
            }
        } 
        Ok(())
    }

    fn group(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightParen, "expected closing ')'")?;
        Ok(())
    }

    fn call(&mut self, _: bool) -> Result<(), CompilerError> {
        let args_count = self.args()?;
        self.push_bytecode(ByteCode::Call(args_count))?;
        Ok(())
    }

    fn args(&mut self) -> Result<ArgsCount, CompilerError> {
        let mut count = 0;
        loop {
            let curr = self.peek()?.tokn;
            if curr == Token::RightParen || curr == Token::EOF {
                break;
            }
            if count > 0 {
                self.expect(Token::Comma, "expected ',' arg seperator")?;
            }
            self.expression()?;
            count += if count > ArgsCount::MAX {
                return Err(self.error("exceeded max args limit"))
            } else {
                1
            };
        }
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(count)
    }

    fn unary(&mut self, _: bool) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(Precedence::Unary)?;
        self.push_bytecode(match op_tok {
            Token::Minus => ByteCode::Neg,
            Token::Bang => ByteCode::Not,
            _ => panic!("Compiler::unary ~ invalid unary operator")
        })?;
        Ok(())
    }

    fn binary(&mut self, _: bool) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(self.get_rule(op_tok).prec)?;
        match op_tok {
            Token::Minus => self.push_bytecode(ByteCode::Sub)?,
            Token::Slash => self.push_bytecode(ByteCode::Div)?,
            Token::Plus => self.push_bytecode(ByteCode::Add)?,
            Token::Star => self.push_bytecode(ByteCode::Mul)?,
            Token::Or => self.push_bytecode(ByteCode::Or)?,
            Token::And => self.push_bytecode(ByteCode::And)?,
            Token::EqualEqual => self.push_bytecode(ByteCode::Equal)?,
            Token::BangEqual => self.push_bytecodes(ByteCode::Equal, ByteCode::Not)?,
            Token::Greater => self.push_bytecode(ByteCode::Greater)?,
            Token::Less => self.push_bytecode(ByteCode::Less)?,
            Token::GreaterEqual => self.push_bytecodes(ByteCode::Less, ByteCode::Not)?,
            Token::LessEqual => self.push_bytecodes(ByteCode::Greater, ByteCode::Not)?,
            _ => return Err(self.error("invalid binary operator"))
        };
        Ok(())
    }
}

// VM::Chunk abstractions
impl Compiler {
    #[inline]
    fn get_chunk(&mut self) -> Result<&mut Chunk, CompilerError> {
        match self.curr_chunk {
            Some(ref mut chunk) => Ok(chunk),
            None => Err(self.error("non-declarative statements must be wrapped within functions"))
        }
    }

    #[inline]
    fn push_bytecode(&mut self, b: ByteCode) -> Result<usize, CompilerError> {
        Ok(self.get_chunk()?.push(b))
    }

    #[inline]
    fn push_bytecodes(&mut self, b1: ByteCode, b2: ByteCode) -> Result<usize, CompilerError> {
        Ok(self.get_chunk()?.pushs(b1, b2))
    }

    #[inline]
    fn set_bytecode(&mut self, idx: usize, b: ByteCode) -> Result<(), CompilerError> {
        Ok(self.get_chunk()?.set(idx, b))
    }

    #[inline]
    fn bytecode_count(&self) -> Result<usize, CompilerError> {
        Ok(self.curr_chunk.as_ref().unwrap().count())
    }

    #[inline]
    fn load_const(&mut self, c: Constant) -> Result<u8, CompilerError> {
        match self.get_chunk()?.load_const(c) {
            Ok(idx) => Ok(idx),
            Err(msg) => Err(self.error(msg))
        }
    }
}

// helpers
impl Compiler {
    fn start_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) -> Result<(), CompilerError> {
        while self.locals.len() > 0 {
            if self.locals.last().unwrap().scope_depth >= self.scope_depth {
                self.locals.pop();
                self.push_bytecode(ByteCode::Pop)?;
            } else {
                break;
            }
        }
        self.scope_depth -= 1;
        Ok(())
    }

    // requires identifier to be parsed
    fn push_local(&mut self) -> Result<(), CompilerError> {
        if self.locals.len() > LocalsCount::MAX as usize {
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

    // REFACTOR
    fn resolve_local(&self, name: &str) -> i16 { // i16 to cover all [-1; 255]
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.token.lexm == name {
                return i as i16;
            }
        }
        -1
    }

    fn patch_jump(&mut self, idx: usize) -> Result<(), CompilerError> {
        self.set_bytecode(idx, ByteCode::Jump(self.bytecode_count()?))?;
        Ok(())
    }

    fn patch_fjump(&mut self, idx: usize) -> Result<(), CompilerError> {
        self.set_bytecode(idx, ByteCode::JumpIfFalse(self.bytecode_count()?))?;
        Ok(())
    }
}

// parser methods
impl Compiler {
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
        match self.peek()? {
            t if t.tokn == what => self.next(),
            t => Err(CompilerError::new(t.line, t.coln, msg, t.lexm))
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
