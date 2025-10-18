use std::collections::HashSet;

use crate::lexer::{
    Lexer,
    Token,
    TokenHeader
};

use crate::vm::{
    ArgsCounter, ByteCode, Chunk, Classifer, Func, FuncCounter, GlobalCounter, InstPtr, LocalCounter, ObjPointer, Object, Program, Struct, StructMember, Type
};

pub struct Compiler {
    lexer: Lexer,
    curr: TokenHeader,

    prog: Program,
    chunk: Option<FuncCounter>,

    globals: Vec<Global>,
    locals: Vec<Local>,
    scope_depth: usize,
    loop_state: LoopState,

    had_error: bool,
    panic_mode: bool,
    declarative_mode: bool, // used to prevent non-declarative top level code -- for structure
}

struct Global {
    token: TokenHeader,
    type_info: Option<TypeInfo>,
}

struct Local {
    token: TokenHeader,
    scope_depth: usize,
    type_info: Option<TypeInfo>,
}

#[derive(Debug)]
enum TypeInfo {
    Struct(Struct),
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
    Primary,
}

type RuleFn = fn(&mut Compiler, can_assign: bool) -> Result<(), CompilerError>;
struct Rule {
    prefix: Option<RuleFn>,
    infix: Option<RuleFn>,
    prec: Precedence,
}

// public methods
impl Compiler {
    pub fn new(src: String) -> Self {
        Self {
            lexer: Lexer::new(src),
            curr: TokenHeader {
                tokn: Token::EOF,
                coln: 0,
                line: 0,
                lexm: String::new(),
            },

            prog: Program::new(),
            chunk: None,

            globals: Vec::new(),
            locals: Vec::new(),
            scope_depth: 0,
            loop_state: LoopState::new(),

            had_error: false,
            panic_mode: false,
            declarative_mode: false,
        }
    }

    pub fn compile(mut self) -> Result<Program, Vec<CompilerError>> {
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
            if self.curr.tokn == Token::EOF {
                break;
            }
        }
        if self.had_error {
            Err(errs)
        } else {
            if let Some(idx) = self.resolve_global("main") {
                self.prog.chunks[0].pushs(ByteCode::GGet(idx), ByteCode::Call(0), self.curr.clone());
            } else {
                return Err(vec![
                    self.error("consider adding `func main()` to your program"),
                ]);
            }
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
                Ok(t) => t.tokn,
                Err(_) => return false,
            };
            match prev {
                Token::Func
                | Token::Let
                | Token::If
                | Token::While
                | Token::Print
                | Token::Return => return true,
                _ => (),
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
                self.declarative_mode = true;
                self.next()?;
                self.compile_let()?;
                self.declarative_mode = false;
            }
            Token::Func => {
                self.declarative_mode = true;
                self.next()?;
                self.compile_func()?;
                self.declarative_mode = false;
            }
            Token::Struct => {
                self.declarative_mode = true;
                self.next()?;
                self.compile_struct()?;
                self.declarative_mode = false;
            }
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
            }
            Token::While => {
                self.next()?;
                self.compile_while()?;
            }
            Token::Print => {
                self.next()?;
                self.compile_print()?;
            }
            Token::LeftBrace => {
                self.next()?;
                self.compile_block()?;
            }
            Token::Break => {
                self.next()?;
                self.compile_break()?;
            }
            Token::Continue => {
                self.next()?;
                self.compile_continue()?;
            }
            Token::Return => {
                self.next()?;
                self.compile_return()?;
            }
            Token::EOF => {
                self.next()?;
            }
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
                    if precedence > infix_rule.prec {
                        break;
                    }
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
            }
            None => return Err(self.error("expected expression")),
        }
        Ok(())
    }

    fn get_rule(&self, tok: Token) -> Rule {
        match tok {
            Token::LeftBracket => Rule {
                prefix: Some(Compiler::array),
                infix: Some(Compiler::index),
                prec: Precedence::Call
            },
            Token::LeftParen => Rule {
                prefix: Some(Compiler::group),
                infix: Some(Compiler::call),
                prec: Precedence::Call,
            },
            Token::Identifer => Rule {
                prefix: Some(Compiler::identifer),
                infix: None,
                prec: Precedence::Primary,
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
                prec: Precedence::Primary,
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
            Token::Greater | Token::Less | Token::GreaterEqual | Token::LessEqual => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Comparison,
            },
            _ => Rule {
                prefix: None,
                infix: None,
                prec: Precedence::None,
            },
        }
    }
}

// non-expression statement grammar
impl Compiler {
    fn compile_let(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "expected identifer")?;
        let byte = if self.scope_depth > 0 {
            self.push_local(None)?;
            ByteCode::LDef
        } else {
            self.push_global(None)?;
            ByteCode::GDef
        };
        if self.check(Token::Equal)? {
            self.expression()?;
        } else {
            self.load_obj(Object::Nil)?;
        }
        self.push_bytecode(byte)?;
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        Ok(())
    }

    fn compile_func(&mut self) -> Result<(), CompilerError> {
        if self.chunk.is_some() {
            return Err(self.error("can't have nested functions"));
        }
        self.expect(Token::Identifer, "expected the function's name")?;
        self.push_global(None)?;
        let func = self.compile_func_body(self.curr.lexm.clone())?;
        match self
            .prog
            .chunk_load_obj(0, Object::Function(func), self.curr.clone())
        {
            Ok(idx) => idx,
            Err(err) => return Err(self.error(err)),
        };
        self.prog.chunks[0].push(ByteCode::GDef, self.curr.clone());
        Ok(())
    }

    fn compile_func_body(&mut self, name: String) -> Result<Func, CompilerError> {
        self.start_scope();
        self.chunk = match self.prog.push(Chunk::new()) {
            Ok(idx) => Some(idx),
            Err(msg) => return Err(self.error(msg))
        };
        let arity = self.compile_params()?;
        let retype = if self.check(Token::RightArrow)? {
            self.compile_type()?
        } else {
            Type::None
        };
        self.expect(Token::LeftBrace, "expected disclosing '{'")?;
        loop {
            let token = self.peek()?.tokn;
            if token == Token::EOF || token == Token::RightBrace {
                break;
            }
            self.declaration()?;
        }
        self.expect(Token::RightBrace, "expected enclosing '}'")?;
        self.load_obj(Object::Nil)?;
        self.push_bytecode(ByteCode::Ret)?;
        self.end_scope()?;
        Ok(Func { name, arity, retype, chunk: self.chunk.take().unwrap() })
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
            self.expect(Token::Identifer, "expected param name")?;
            self.push_local(None)?; // TODO: typecheck
            self.expect(Token::Colon, "expected ':' param type seperator")?;
            self.compile_type()?;
            count += 1;
        }
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(count)
    }

    fn compile_struct(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "expected struct name")?;
        let tok = self.curr.clone();
        self.expect(Token::LeftBrace, "expected '{'")?;
        let mut fields = std::collections::HashMap::new();
        while !self.check_with_eof(Token::RightBrace)? {
            self.expect(Token::Identifer, "expected struct member name")?;
            let member_name = self.curr.lexm.clone();
            self.expect(Token::Colon, "expected ':' as a type seperator")?;
            let member_type = StructMember(self.compile_type()?);
            fields.insert(member_name, member_type);
            self.check(Token::Comma)?;
        }
        if self.curr.tokn != Token::RightBrace {
            return Err(self.error("expected '}'"));
        }
        let temp = self.curr.clone();
        {
            let le_struct = Struct{ name: tok.lexm.clone(), fields };
            self.curr = tok;
            let byte = if self.scope_depth > 0 {
                self.push_local(Some(TypeInfo::Struct(le_struct.clone())))?;
                ByteCode::LDef
            } else {
                self.push_global(Some(TypeInfo::Struct(le_struct.clone())))?;
                ByteCode::GDef
            };
            self.load_readonly_obj(Object::Struct(le_struct))?;
            self.push_bytecode(byte)?;
        }
        self.curr = temp;
        Ok(())
    }

    fn compile_type(&mut self) -> Result<Type, CompilerError> {
        let typ = match self.peek()?.tokn {
            Token::IntT => Type::Integer,
            Token::FloatT => Type::Float,
            Token::BoolT => Type::Bool,
            Token::StrT => Type::String,
            Token::FuncT => Type::Function,
            Token::Identifer => Type::Struct, // TODO: for now
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
        while let Some(jump) = self.loop_state.break_jumps.pop() {
            self.patch_jump(jump)?;
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

    fn compile_return(&mut self) -> Result<(), CompilerError> {
        if !self.check(Token::SemiColon)? {
            self.expression()?;
        } else {
            self.load_obj(Object::Nil)?;
        }
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        if self.chunk.is_none() {
            return Err(self.error("'return' can only be used inside a function"));
        }
        self.push_bytecode(ByteCode::Ret)?;
        Ok(())
    }

    fn args_format(&mut self) -> Result<ArgsCounter, CompilerError> {
        self.expect(Token::String, "expected format string")?; 
        let format = self.curr.lexm.clone();
        let mut offset = 0;
        let mut next_sub_str = |buf: &mut String| -> Result<bool, &'static str> {
            let mut start = None;
            let s = &format[offset..];
            for (i, c) in s.bytes().enumerate() {
                match c as char {
                    '{' => {
                        if start.is_some() {
                            return Err("can't have nested templates");
                        }
                        start = Some(i+1);
                    },
                    '}' => {
                        match start {
                            Some(j) => {
                                if j != i {
                                    return Err("can't(yet) have something inside {}");
                                }
                                offset += i+1;
                                *buf = String::from(&s[..i-1]);
                                return Ok(false);
                            },
                            None => {
                                return Err("no corresponding '{'");
                            }
                        };
                    }
                    _ => ()
                };
            }
            if start.is_some() {
                Err("no corresponding '}'")
            } else {
                *buf = String::from(s);
                Ok(true)
            }
        };
        let mut count = 0;
        let mut params_count = 1;
        loop {
            if params_count == ArgsCounter::MAX {
                return Err(self.error("exceeded max args limit"));
            }
            let mut buf = String::new();
            let is_end = match next_sub_str(&mut buf) {
                Ok(is_end) => is_end,
                Err(msg) => return Err(self.error(msg))
            };
            if buf.len() > 0 {
                self.load_obj(Object::String(buf))?;
                count += 1;
            }
            if is_end {
                break;
            }
            self.expect(Token::Comma, "expected ',' arg seperator")?;
            self.expression()?;
            params_count += 1;
            count += 1;
        }
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(count)
    }

    fn compile_print(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::LeftParen, "expected disclosing '('")?;
        let count = self.args_format()?;
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        self.push_bytecode(ByteCode::Print(count))?;
        Ok(())
    }
}

// expression statement grammar
impl Compiler {
    fn literal(&mut self, _: bool) -> Result<(), CompilerError> {
        match self.curr.tokn {
            Token::Int(val) => {
                self.load_obj(Object::Integer(val))?
            },
            Token::Float(val) => {
                self.load_obj(Object::Float(val))?
            },
            Token::Bool(val) => {
                self.load_obj(Object::Bool(val))?
            },
            Token::String => {
                self.load_obj(Object::String(self.curr.lexm.clone()))?
            }
            Token::Nil => self.load_obj(Object::Nil)?,
            _ => panic!("Compiler::literal ~ unhandled literal {:?}", self.curr),
        };
        Ok(())
    }

    fn identifer(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        let name = self.curr.lexm.clone();
        match self.resolve_local(name.as_str()) {
            Some(idx) => {
                if can_assign && self.check(Token::Equal)? {
                    self.expression()?;
                    self.push_bytecode(ByteCode::LSet(idx))?;
                } else if self.check(Token::LeftBrace)? {
                    let base = match &self.locals[idx].type_info {
                        Some(TypeInfo::Struct(base)) => base.clone(),
                        None => return Err(self.error("expected struct declaration"))
                    };
                    let mut set = HashSet::new();
                    for _ in 0..base.fields.len() {
                        self.expect(Token::Identifer, "expected member name")?;
                        if base.fields.get(&self.curr.lexm).is_none() {
                            return Err(self.error("undefined member in struct"));
                        }
                        if set.contains(&self.curr.lexm) {
                            return Err(self.error("already defined field"));
                        } 
                        let name = self.curr.lexm.clone();
                        self.expect(Token::Colon, "expected ':'")?;
                        self.expression()?;
                        self.check(Token::Comma)?;
                        self.load_obj(Object::String(name.clone()))?;
                        set.insert(name);
                    }
                    self.expect(Token::RightBrace, "expected '}'")?;
                    self.push_bytecode(ByteCode::LStruct(idx))?;
                } else {
                    self.push_bytecode(ByteCode::LGet(idx))?;
                }
            }
            None => {
                if let Some(idx) = self.resolve_global(self.curr.lexm.as_str()) {
                    if can_assign && self.check(Token::Equal)? {
                        self.expression()?;
                        self.push_bytecode(ByteCode::GSet(idx))?;
                    } else if self.check(Token::LeftBrace)? {
                        let base = match &self.globals[idx].type_info {
                            Some(TypeInfo::Struct(base)) => base.clone(),
                            None => return Err(self.error("expected struct declaration"))
                        };
                        let mut set = HashSet::new();
                        for _ in 0..base.fields.len() {
                            self.expect(Token::Identifer, "expected member name")?;
                            if base.fields.get(&self.curr.lexm).is_none() {
                                return Err(self.error("undefined member in struct"));
                            }
                            if set.contains(&self.curr.lexm) {
                                return Err(self.error("already defined field"));
                            } 
                            let name = self.curr.lexm.clone();
                            self.expect(Token::Colon, "expected ':'")?;
                            self.expression()?;
                            self.check(Token::Comma)?;
                            self.load_obj(Object::String(name.clone()))?;
                            set.insert(name);
                        }
                        self.expect(Token::RightBrace, "expected '}'")?;
                        self.push_bytecode(ByteCode::GStruct(idx))?;
                    } else {
                        self.push_bytecode(ByteCode::GGet(idx))?;
                    }
                } else {
                    return Err(self.error("undefined binding"));
                }
            }
        };
        Ok(())
    }

    fn group(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(())
    }

    fn array(&mut self, _: bool) -> Result<(), CompilerError> {
        let mut count = 0;
        while !self.check_with_eof(Token::RightBracket)? {
            loop {
                count += 1;
                self.expression()?;
                if !self.check(Token::Comma)? {
                    break;
                }
            }
        }
        if self.curr.tokn != Token::RightBracket {
            return Err(self.error("expected disclosing ']'"));
        }
        self.push_bytecode(ByteCode::Array(count))?;
        Ok(())
    }

    fn index(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightBracket, "expected enclosing ']'")?;
        if can_assign && self.check(Token::Equal)? {
            self.expression()?;
            self.push_bytecode(ByteCode::ArraySet)?;
        } else {
            self.push_bytecode(ByteCode::ArrayGet)?;
        }
        Ok(())
    }

    fn call(&mut self, _: bool) -> Result<(), CompilerError> {
        let args_count = self.args()?;
        self.push_bytecode(ByteCode::Call(args_count))?;
        Ok(())
    }

    fn args(&mut self) -> Result<ArgsCounter, CompilerError> {
        let mut count = 0;
        if !self.check(Token::RightParen)? {
            loop {
                if count == ArgsCounter::MAX {
                    return Err(self.error("exceeded max args limit"));
                }
                self.expression()?; count += 1;
                if !self.check(Token::Comma)? {
                    break;
                }
            }
            self.expect(Token::RightParen, "expected enclosing ')'")?;
        }
        Ok(count)
    }

    fn unary(&mut self, _: bool) -> Result<(), CompilerError> {
        let op_tok = self.curr.tokn;
        self.parse_precedence(Precedence::Unary)?;
        self.push_bytecode(match op_tok {
            Token::Minus => ByteCode::Neg,
            Token::Bang => ByteCode::Not,
            _ => panic!("Compiler::unary ~ invalid unary operator"),
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
            _ => return Err(self.error("invalid binary operator")),
        };
        Ok(())
    }
}

// VM::Chunk abstractions
impl Compiler {
    fn get_chunk_at(&mut self, fc: FuncCounter) -> &mut Chunk {
        &mut self.prog.chunks[fc as usize]
    }

    #[inline]
    fn get_chunk(&mut self) -> Result<&mut Chunk, CompilerError> {
        match self.chunk {
            Some(fc) => {
                Ok(self.get_chunk_at(fc))
            },
            None if self.declarative_mode => {
                Ok(self.get_chunk_at(0))
            }
            _ => {
                Err(self.error("non-declarative statements must be wrapped within functions"))
            }
        }
    }

    #[inline]
    fn push_bytecode(&mut self, b: ByteCode) -> Result<usize, CompilerError> {
        let tok = self.curr.clone();
        Ok(self.get_chunk()?.push(b, tok))
    }

    #[inline]
    fn push_bytecodes(&mut self, b1: ByteCode, b2: ByteCode) -> Result<usize, CompilerError> {
        let tok = self.curr.clone();
        Ok(self.get_chunk()?.pushs(b1, b2, tok))
    }

    #[inline]
    fn set_bytecode(&mut self, idx: usize, b: ByteCode) -> Result<(), CompilerError> {
        Ok(self.get_chunk()?.set(idx, b))
    }

    #[inline]
    fn bytecode_count(&self) -> Result<usize, CompilerError> {
        Ok(self.prog.chunks[self.chunk.unwrap() as usize].count())
    }


    fn load_readonly_obj(&mut self, o: Object) -> Result<ObjPointer, CompilerError> {
        // NOTE: or_default is a stupid hack 
        self.prog.chunk_load_readonly_obj(self.chunk.unwrap_or_default(), o, self.curr.clone()).map_err(|err| {
            self.error(err)
        })
    }

    #[inline]
    fn load_obj(&mut self, o: Object) -> Result<ObjPointer, CompilerError> {
        // NOTE: or_default is a stupid hack 
        self.prog.chunk_load_obj(self.chunk.unwrap_or_default(), o, self.curr.clone()).map_err(|err| {
            self.error(err)
        })
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

    fn push_global(&mut self, type_info: Option<TypeInfo>) -> Result<(), CompilerError> {
        if self.globals.len() > GlobalCounter::MAX as usize {
            return Err(self.error("exceeded maximum number of global bindings."));
        }
        for global in self.globals.iter() {
            if global.token.lexm == self.curr.lexm {
                return Err(
                    self.error("variable identifiers must be unique within the current scope.")
                );
            }
        }
        self.globals.push(Global {
            token: self.curr.clone(),
            type_info,
        });
        Ok(())
    }

    fn resolve_global(&self, name: &str) -> Option<GlobalCounter> {
        for (i, global) in self.globals.iter().enumerate() {
            if global.token.lexm == name {
                return Some(i as GlobalCounter);
            }
        }
        None
    }

    // requires identifier to be parsed
    fn push_local(&mut self, type_info: Option<TypeInfo>) -> Result<(), CompilerError> {
        if self.locals.len() > LocalCounter::MAX as usize {
            return Err(self.error("exceeded maximum number of local bindings."));
        }
        for local in self.locals.iter().rev() {
            if local.scope_depth < self.scope_depth {
                break;
            }
            if local.token.lexm == self.curr.lexm {
                return Err(
                    self.error("variable identifiers must be unique within the current scope.")
                );
            }
        }
        self.locals.push(Local {
            token: self.curr.clone(),
            scope_depth: self.scope_depth,
            type_info
        });
        Ok(())
    }

    fn resolve_local(&self, name: &str) -> Option<LocalCounter> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.token.lexm == name {
                return Some(i as LocalCounter);
            }
        }
        None
    }

    fn patch_jump(&mut self, idx: InstPtr) -> Result<(), CompilerError> {
        self.set_bytecode(idx, ByteCode::Jump(self.bytecode_count()?))?;
        Ok(())
    }

    fn patch_fjump(&mut self, idx: InstPtr) -> Result<(), CompilerError> {
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
            }
            Err(e) => Err(CompilerError::new(e.line, e.coln, e.mssg, e.lexm)),
        }
    }

    fn peek(&mut self) -> Result<TokenHeader, CompilerError> {
        match self.lexer.peek() {
            Ok(tok) => Ok(tok),
            Err(e) => Err(CompilerError::new(e.line, e.coln, e.mssg, e.lexm)),
        }
    }

    fn expect(&mut self, what: Token, msg: &'static str) -> Result<(), CompilerError> {
        self.next()?;
        if self.curr.tokn != what {
            return Err(
                CompilerError::new(
                    self.curr.line, self.curr.coln, msg, self.curr.lexm.clone()
                )
            );
        }
        Ok(())
    }

    fn check_with_eof(&mut self, what: Token) -> Result<bool, CompilerError> {
        let tokn = self.peek()?.tokn;
        Ok(if tokn == Token::EOF || tokn == what {
            self.next()?;
            true
        } else {
            false
        })
    }

    fn check(&mut self, what: Token) -> Result<bool, CompilerError> {
        let tokn = self.peek()?.tokn;
        Ok(if tokn == what {
            self.next()?;
            true
        } else {
            false
        })
    }

    fn error(&self, msg: &'static str) -> CompilerError {
        CompilerError::from_tok(&self.curr, msg)
    }
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
                _ => tok.lexm.clone(),
            },
            mssg,
        }
    }

    pub fn new(line: usize, coln: usize, mssg: &'static str, lexm: String) -> Self {
        Self {
            line,
            coln,
            mssg,
            lexm,
        }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "at {}-{}: {} at {}",
            self.line, self.coln, self.mssg, self.lexm
        )
    }
}

impl std::error::Error for CompilerError {}
