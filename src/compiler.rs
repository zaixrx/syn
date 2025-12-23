// TODO: for structs build up a symbol table so that you don't need to interact with strings
// and use offsets in le struct to access things like fields and methods with additional
// type-safety
//
// TODO: replace the error returing with error reporting because each compiler method can produce
// more than one error for readability

use std::collections::HashSet;

use crate::lexer::{
    Lexer,
    Token,
    TokenHeader
};

use crate::vm::{
    ArgsCounter, ByteCode, Chunk, Classifier, Func, FuncCounter, GlobalCounter, LocalCounter, MethodCounter, Object, Program, Struct, StructMember, Type
};

use crate::arena::{Arena, Pointer};

pub struct Compiler<'a> {
    lexer: Lexer,
    curr: TokenHeader,

    prog: Program,
    chunk: Option<FuncCounter>,

    globals: Vec<Global>,
    locals: Vec<Local>,
    scope_depth: usize,

    loop_state: LoopState,
    impl_state: bool,

    had_error: bool,
    had_sync_err: bool, // TODO: had an error in the synchronization process... to prevent infinite loops

    panic_mode: bool,
    declarative_mode: bool, // used to prevent non-declarative top level code -- for structure

    arena: &'a mut Arena,
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
    LogicalOr,
    XOR,
    LogicalAnd,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

type RuleFn<'a> = fn(&mut Compiler<'a>, bool) -> Result<(), CompilerError>;

struct Rule<'a> {
    prefix: Option<RuleFn<'a>>,
    infix: Option<RuleFn<'a>>,
    prec: Precedence,
}

// public methods
impl<'a> Compiler<'a> {
    pub fn new(arena: &'a mut Arena, src: String) -> Self {
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

            loop_state: LoopState {
                start: 0,
                in_loop: false,
                break_jumps: Vec::new()
            },
            impl_state: false,

            had_error: false,
            panic_mode: false,
            declarative_mode: false,
            had_sync_err: false,

            arena,
        }
    }

    pub fn compile<F>(mut self, mut report: F) -> Option<Program> 
    where
        F: FnMut(CompilerError),
    {
        while !self.had_sync_err {
            self.declaration().unwrap_or_else(|err| {
                if !self.panic_mode {
                    report(err);
                    self.had_error = true;
                    self.panic_mode = true;
                }
            });
            if self.curr.tokn == Token::EOF {
                break;
            }
        }
        if self.had_error {
            None
        } else if let Some(idx) = self.resolve_global("main") {
            self.prog.chunks[0].push_instructions(ByteCode::GGet(idx), ByteCode::Call(0), self.curr.clone());
            Some(self.prog)
        } else {
            report(self.error("consider adding `func main()` to your program"));
            None
        }
    }

    // synchronize until:
    // end of file
    // the start of a new statement (func, impl, struct, while, if, ...)
    // end of a statement (;)
    fn synchronize(&mut self) {
        self.panic_mode = false;
        loop {
            if self.curr.tokn == Token::EOF {
                return;
            }
            let next = match self.peek() {
                Ok(t) => t.tokn,
                Err(_) => return { self.had_sync_err = true; },
            };
            match next {
                Token::Impl |
                Token::Struct |
                Token::Func |
                Token::Let |
                Token::If |
                Token::While |
                Token::Print |
                Token::Return => return,
                _ => (),
            };
            if self.next().is_err() {
                return { self.had_sync_err = true; };
            };
            if next == Token::SemiColon {
                return;
            }
        }
    }
}

// grammar agnostic methods
impl<'a> Compiler<'a> {
    fn declaration(&mut self) -> Result<(), CompilerError> {
        if self.panic_mode {
            self.synchronize();
        }
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
            Token::Impl => {
                self.declarative_mode = true;
                self.impl_state = true;
                self.next()?;
                self.expect(Token::Identifer, "expected struct name")?;
                let base_ptr = match self.resolve_global(&self.curr.lexm) {
                    Some(base_ptr) => base_ptr,
                    None => return Err(self.error("undefined struct"))
                };
                self.expect(Token::LeftBrace, "expected '{'")?;
                let mut methods_count = 0;
                while self.check(Token::Func)? {
                    if methods_count == MethodCounter::MAX {
                        return Err(self.error("reached max methods count in a struct"));
                    }
                    self.compile_method()?;
                    methods_count += 1;
                }
                self.expect(Token::RightBrace, "expected '}'")?;
                self.push_bytecode(ByteCode::GGet(base_ptr))?;
                self.push_bytecode(ByteCode::StructImpl(methods_count))?;
                self.impl_state = false;
                self.declarative_mode = false;
            }
            _ => {
                return self.statement();
            }
        };
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

    fn parse_precedence(&mut self, prec : Precedence) -> Result<(), CompilerError> {
        self.next()?;
        let can_assign = prec <= Precedence::Assignment;
        let prefix = {
            let rule = self.get_rule(self.curr.tokn);
            if let Some(prefix) = rule.prefix {
                prefix
            } else {
                return Err(self.error("expected expression"))
            }
        };
        prefix(self, can_assign)?;
        loop {
            let infix_tok = self.peek()?;
            let infix_rule = self.get_rule(infix_tok.tokn);
            if prec > infix_rule.prec {
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
        Ok(())
    }

    fn get_rule<'b>(&self, tok: Token) -> Rule<'b> {
        match tok {
            Token::ScopeRes => Rule {
                prefix: None,
                infix: Some(Compiler::method),
                prec: Precedence::Call
            },
            Token::Dot => Rule {
                prefix: None,
                infix: Some(Compiler::field),
                prec: Precedence::Call
            },
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
            Token::LeSelf | Token::Identifer => Rule {
                prefix: Some(Compiler::identifer),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Panic => Rule {
                prefix: Some(Compiler::panic),
                infix: None,
                prec: Precedence::Primary,
            },
            Token::Assert => Rule {
                prefix: Some(Compiler::assert),
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
            Token::Percent | Token::Slash | Token::Star => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Factor,
            },
            Token::Or => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::Or,
            },
            Token::LogicalOr => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::LogicalOr,
            },
            Token::LogicalAnd => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::LogicalAnd,
            },
            Token::Caret => Rule {
                prefix: None,
                infix: Some(Compiler::binary),
                prec: Precedence::XOR,
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
impl<'a> Compiler<'a> {
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
            self.load_runtime_obj(Object::Nil);
        }
        self.push_bytecode(byte)?;
        self.expect(Token::SemiColon, "expected ';' after statement")?;
        Ok(())
    }

    fn compile_method(&mut self) -> Result<Pointer, CompilerError> {
        self.expect(Token::Identifer, "expected the method's name")?;
        let src = self.curr.lexm.clone();
        let synstr = self.arena.str_to_synstr(src.clone());
        self.load_runtime_obj(Object::SynString(synstr));
        let func = self.compile_func_body(src)?;
        Ok(self.load_runtime_obj(Object::Function(func)))
    }

    fn compile_func(&mut self) -> Result<(), CompilerError> {
        if self.chunk.is_some() {
            return Err(self.error("can't have nested functions"));
        }
        self.expect(Token::Identifer, "expected the function's name")?;
        let token = self.curr.clone();
        self.push_global(None)?;
        let func = self.compile_func_body(token.lexm.clone())?;
        self.load_runtime_obj(Object::Function(func));
        self.prog.chunks[0].push_instruction(ByteCode::GDef, self.curr.clone());
        Ok(())
    }

    fn compile_func_body(&mut self, name: String) -> Result<Func, CompilerError> {
        self.start_scope();
        self.chunk = match self.prog.push(Chunk::new()) {
            Ok(idx) => Some(idx),
            Err(msg) => return Err(self.error(msg))
        };
        let (arity, is_method) = self.compile_params()?;
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
        self.load_runtime_obj(Object::Nil);
        self.push_bytecode(ByteCode::Ret)?;
        self.end_scope()?;
        let chunk = self.chunk.take().unwrap();
        Ok(Func { name, arity, retype, chunk, is_method })
    }

    fn compile_params(&mut self) -> Result<(usize, bool), CompilerError> {
        let mut count = 0;
        let mut is_method = false;
        self.expect(Token::LeftParen, "expected disclosing '('")?;
        if self.check(Token::LeSelf)? {
            count += 1;
            if !self.impl_state {
                return Err(self.error("can't declare a method outside `impl` block"));
            }
            self.push_local(None)?; // TODO: typecheck
            is_method = true;
        }
        loop {
            let curr = self.peek()?.tokn;
            if curr == Token::RightParen || curr == Token::EOF {
                break;
            }
            if count > 0 {
                self.expect(Token::Comma, "expected ',' param seperator")?;
            }
            if self.check(Token::LeSelf)? {
                // TODO: report both if possible
                if is_method {
                    return Err(self.error("can't have more than one 'self' for a method"));
                }
                return Err(self.error("self should be the first parameter in the method"));
            }
            self.expect(Token::Identifer, "expected param name")?;
            self.push_local(None)?; // TODO: typecheck
            self.expect(Token::Colon, "expected ':' param type seperator")?;
            self.compile_type()?;
            count += 1;
        }
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok((count, is_method))
    }

    fn compile_struct(&mut self) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "expected struct name")?;
        let mut le_struct = Struct::new(self.curr.lexm.clone());
        let rollback = self.curr.clone();
        {
            self.expect(Token::LeftBrace, "expected '{'")?;
            while !self.check_with_eof(Token::RightBrace)? {
                self.expect(Token::Identifer, "expected struct member name")?;
                let field_name = self.curr.lexm.clone();
                let synstr = self.arena.str_to_synstr(field_name);
                self.expect(Token::Colon, "expected ':' as a type seperator")?;
                le_struct.add_member(synstr.base, StructMember::Field{typ: self.compile_type()?});
                self.check(Token::Comma)?;
            }
            if self.curr.tokn != Token::RightBrace {
                return Err(self.error("expected '}'"));
            }
        }
        let temp = self.curr.clone();
        self.curr = rollback;
        {
            let byte = if self.scope_depth > 0 {
                self.push_local(Some(TypeInfo::Struct(le_struct.clone())))?;
                ByteCode::LDef
            } else {
                self.push_global(Some(TypeInfo::Struct(le_struct.clone())))?;
                ByteCode::GDef
            };
            // self.load_readonly_obj(Object::Struct(le_struct));
            self.load_runtime_obj(Object::Struct(le_struct));
            self.push_bytecode(byte)?;
        }
        self.curr = temp;
        Ok(())
    }

    fn compile_type(&mut self) -> Result<Type, CompilerError> {
        let typ = match self.peek()?.tokn {
            Token::ByteT => Type::Byte,
            Token::IntT => Type::Integer,
            Token::FloatT => Type::Float,
            Token::BoolT => Type::Bool,
            Token::StrT => Type::SynString,
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
            self.load_runtime_obj(Object::Nil);
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
                let synstr = self.arena.str_to_synstr(buf);
                self.load_runtime_obj(Object::SynString(synstr));
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
impl<'a> Compiler<'a> {
    fn literal(&mut self, _: bool) -> Result<(), CompilerError> {
        match self.curr.tokn {
            Token::Int(val) => {
                self.load_runtime_obj(Object::Integer(val))
            },
            Token::Float(val) => {
                self.load_runtime_obj(Object::Float(val))
            },
            Token::Bool(val) => {
                self.load_runtime_obj(Object::Bool(val))
            },
            Token::String => {
                let src = self.curr.lexm.clone();
                let synstr = self.arena.str_to_synstr(src);
                self.load_runtime_obj(Object::SynString(synstr))
            }
            Token::Nil => self.load_runtime_obj(Object::Nil),
            _ => panic!("Compiler::literal ~ unhandled literal {:?}", self.curr),
        };
        Ok(())
    }

    fn check_for_assign(&mut self) -> Result<bool, CompilerError> {
        let token = self.peek()?;
        match token.tokn {
            Token::Equal => {
                self.next()?;
                self.expression()?;
                Ok(true)
            },
            Token::PlusEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::Add)?;
                Ok(true)
            },
            Token::MinusEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::Sub)?;
                Ok(true)
            },
            Token::StarEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::Mul)?;
                Ok(true)
            },
            Token::SlashEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::Div)?;
                Ok(true)
            },
            Token::LogicalOrEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::LogicalOr)?;
                Ok(true)
            },
            Token::LogicalAndEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::LogicalAnd)?;
                Ok(true)
            },
            Token::CaretEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::XOR)?;
                Ok(true)
            },
            Token::PercentEqual => {
                self.identifer(false)?;
                self.next()?; // skip opreator
                self.expression()?;
                self.push_bytecode(ByteCode::Modulo)?;
                Ok(true)
            },
            _ => Ok(false)
        }
    }

    // does a lookup for the current identifer from the ineer most scope to the global scope
    fn identifer(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        let name = self.curr.lexm.clone();
        match self.resolve_local(name.as_str()) {
            Some(idx) => {
                if can_assign && self.check_for_assign()? {
                    self.push_bytecode(ByteCode::LSet(idx))?;
                } else if self.check(Token::LeftBrace)? {
                    let base = match &self.locals[idx].type_info {
                        Some(TypeInfo::Struct(base)) => base.clone(),
                        None => return Err(self.error("expected struct declaration"))
                    };
                    let mut set = HashSet::new();
                    for _ in 0..base.members.len() {
                        self.expect(Token::Identifer, "expected member name")?;
                        let name = self.curr.lexm.clone();
                        let synstr = self.arena.str_to_synstr(name);
                        if base.members.get(&synstr.base).is_none() {
                            return Err(self.error("undefined member in struct"));
                        }
                        if set.contains(&synstr.base) {
                            return Err(self.error("already defined field"));
                        } 
                        self.expect(Token::Colon, "expected ':'")?;
                        self.expression()?;
                        self.check(Token::Comma)?;
                        set.insert(synstr.base);
                        self.load_runtime_obj(Object::SynString(synstr));
                    }
                    self.expect(Token::RightBrace, "expected '}'")?;
                    self.push_bytecode(ByteCode::LStruct(idx))?;
                } else {
                    self.push_bytecode(ByteCode::LGet(idx))?;
                }
            }
            None => {
                if let Some(idx) = self.resolve_global(self.curr.lexm.as_str()) {
                    if can_assign && self.check_for_assign()? { 
                        self.push_bytecode(ByteCode::GSet(idx))?;
                    } else if self.check(Token::LeftBrace)? {
                        let base = match &self.globals[idx].type_info {
                            Some(TypeInfo::Struct(base)) => base.clone(),
                            None => return Err(self.error("expected struct declaration"))
                        };
                        let mut set = HashSet::new();
                        for _ in 0..base.members.len() {
                            self.expect(Token::Identifer, "expected member name")?;
                            let name = self.curr.lexm.clone();
                            let synstr = self.arena.str_to_synstr(name.clone());
                            if base.members.get(&synstr.base).is_none() {
                                return Err(self.error("undefined member in struct"));
                            }
                            if set.contains(&name) {
                                return Err(self.error("already defined field"));
                            } 
                            self.expect(Token::Colon, "expected ':'")?;
                            self.expression()?;
                            self.check(Token::Comma)?;
                            set.insert(name.clone());
                            self.load_runtime_obj(Object::SynString(synstr));
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

    fn panic(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.push_bytecode(ByteCode::Panic)?;
        Ok(())
    }

    fn assert(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.push_bytecode(ByteCode::Assert)?;
        Ok(())
    }

    fn group(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightParen, "expected enclosing ')'")?;
        Ok(())
    }

    // NOTE: for now it's only specific to `static` methods
    fn method(&mut self, _: bool) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "exepeted method name")?;
        let src = self.curr.lexm.clone();
        let synstr = self.arena.str_to_synstr(src);
        self.load_runtime_obj(Object::SynString(synstr));
        self.push_bytecode(ByteCode::MethodGet)?;
        Ok(())
    }

    fn field(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        self.expect(Token::Identifer, "exepeted field name")?;
        let src = self.curr.lexm.clone();
        let synstr = self.arena.str_to_synstr(src);
        self.load_runtime_obj(Object::SynString(synstr));
        if can_assign && self.check_for_assign()? {
            self.push_bytecode(ByteCode::FieldSet)?;
        } else {
            self.push_bytecode(ByteCode::FieldGet)?;
        }
        Ok(())
    }

    fn array(&mut self, _: bool) -> Result<(), CompilerError> {
        let mut len = 0;
        while !self.check_with_eof(Token::RightBracket)? {
            loop {
                len += 1;
                self.expression()?;
                if !self.check(Token::Comma)? {
                    break;
                }
            }
        }
        if self.curr.tokn != Token::RightBracket {
            return Err(self.error("expected disclosing ']'"));
        }
        self.push_bytecode(ByteCode::Array(len))?;
        Ok(())
    }

    fn index(&mut self, can_assign: bool) -> Result<(), CompilerError> {
        self.expression()?;
        self.expect(Token::RightBracket, "expected enclosing ']'")?;
        if can_assign && self.check_for_assign()? {
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
            Token::LogicalOr => self.push_bytecode(ByteCode::LogicalOr)?,
            Token::Caret => self.push_bytecode(ByteCode::XOR)?,
            Token::LogicalAnd => self.push_bytecode(ByteCode::LogicalAnd)?,
            Token::Percent => self.push_bytecode(ByteCode::Modulo)?,
            _ => return Err(self.error("invalid binary operator")),
        };
        Ok(())
    }
}

// VM::Chunk abstractions
impl<'a> Compiler<'a> {
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
        Ok(self.get_chunk()?.push_instruction(b, tok))
    }

    #[inline]
    fn push_bytecodes(&mut self, b1: ByteCode, b2: ByteCode) -> Result<usize, CompilerError> {
        let tok = self.curr.clone();
        Ok(self.get_chunk()?.push_instructions(b1, b2, tok))
    }

    #[inline]
    fn set_bytecode(&mut self, idx: usize, b: ByteCode) -> Result<(), CompilerError> {
        Ok(self.get_chunk()?.set(idx, b))
    }

    #[inline]
    fn bytecode_count(&self) -> Result<usize, CompilerError> {
        Ok(self.prog.chunks[self.chunk.unwrap() as usize].count())
    }

    #[allow(dead_code)]
    fn load_readonly_obj(&mut self, obj: Object) -> Pointer {
        let chunk = &mut self.prog.chunks[self.chunk.unwrap_or(0)];
        let ptr = self.arena.push_obj(Classifier::Readonly(obj));
        chunk.push_instruction(ByteCode::Push(ptr), self.curr.clone());
        return ptr;
    }

    #[inline]
    fn load_runtime_obj(&mut self, obj: Object) -> Pointer {
        let chunk = &mut self.prog.chunks[self.chunk.unwrap_or(0)];
        let ptr = self.arena.push_obj(Classifier::Runtime(obj));
        chunk.push_instruction(ByteCode::Push(ptr), self.curr.clone());
        return ptr;
    }
}

// helpers
impl<'a> Compiler<'a> {
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
impl<'a> Compiler<'a> {
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
            "ERROR at {}-{}: {} at `{}`",
            self.line, self.coln, self.mssg, self.lexm
        )
    }
}

impl std::error::Error for CompilerError {}
