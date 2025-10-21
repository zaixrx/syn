use std::{collections::HashMap, fmt::Debug};

use crate::lexer::TokenHeader;

pub struct VM {
    prog: Program,
    frame: CallFrame,
    stack: Vec<ObjPointer>,
    globals: Vec<ObjPointer>,
    call_stack: Vec<CallFrame>
}

pub struct Program {
    pub chunks: Vec<Chunk>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            chunks: vec![Chunk::new()],
        }
    }

    pub fn push(&mut self, c: Chunk) -> Result<FuncCounter, &'static str> {
        let idx = self.chunks.len() as FuncCounter;
        if idx < FuncCounter::MAX {
            self.chunks.push(c);
            Ok(idx)
        } else {
            Err("exceeded the number of possible functions in a program")
        }
    }

    fn load_obj(&mut self, fc: FuncCounter, obj: Classifer<Object>, tok: TokenHeader) -> Result<ObjPointer, &'static str> {
        let chunk = &mut self.chunks[fc as usize];
        let ptr = (fc, chunk.objs.len() as ObjCounter);
        if ptr.1 < ObjCounter::MAX {
            chunk.objs.push(obj);
            chunk.push(ByteCode::Push(ptr), tok);
            Ok(ptr)
        } else {
            Err("can't define more constants in chunk")
        }
    }

    pub fn chunk_load_obj(&mut self, fc: FuncCounter, obj: Object, tok: TokenHeader) -> Result<ObjPointer, &'static str> {
        self.load_obj(fc, Classifer::Raw(obj), tok)
    }

    pub fn chunk_load_readonly_obj(&mut self, fc: FuncCounter, obj: Object, tok: TokenHeader) -> Result<ObjPointer, &'static str> {
        self.load_obj(fc, Classifer::Readonly(obj), tok)
    }

    pub fn get_obj(&self, ptr: ObjPointer) -> &Object {
        self.chunks[ptr.0 as usize].get_obj(ptr.1)
    }

    pub fn get_obj_mut(&mut self, ptr: ObjPointer) -> Result<&mut Object, &'static str> {
        self.chunks[ptr.0 as usize].get_obj_mut(ptr.1)
    }

    #[allow(unused)]
    pub fn chunk_disassemble_one(&self, fc: FuncCounter, ip: InstPtr) {
        print!("{:0>4}", ip);
        match self.chunks[fc as usize].code[ip] {
            ByteCode::Push(i) => {
                println!("  Push {} --> {:?}", i.1, self.chunks[i.0 as usize].objs[i.1 as usize]);
            }
            byte => println!("  {:?}", byte),
        };
    }

    #[allow(unused)]
    pub fn chunk_disassemble(&self, fc: FuncCounter) {
        for ip in 0..self.chunks[fc as usize].code.len() {
            self.chunk_disassemble_one(fc, ip);
        }
    }

    pub fn disassemble(&self) {
        println!("== PROG_START ==");
        for fc in 0..self.chunks.len() {
            self.chunk_disassemble(fc as FuncCounter);
            println!();
        }
        println!("== PROG_END ==");
    }
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub objs: Vec<Classifer<Object>>,
    code: Vec<ByteCode>,
    info: Vec<TokenHeader>, // info for code(info.len() = code.len())
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            objs: Vec::new(),
            info: Vec::new(),
            code: Vec::new(),
        }
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn push(&mut self, b: ByteCode, t: TokenHeader) -> usize {
        self.info.push(t);
        self.code.push(b);
        self.code.len() - 1
    }

    pub fn pushs(&mut self, b1: ByteCode, b2: ByteCode, t: TokenHeader) -> usize {
        self.push(b1, t.clone());
        self.push(b2, t)
    }

    pub fn set(&mut self, idx: usize, b: ByteCode) {
        self.code[idx] = b;
    }

    pub fn get_obj(&self, idx: ObjCounter) -> &Object {
        match &self.objs[idx as usize] {
            Classifer::Raw(obj) => obj,
            Classifer::Readonly(obj) => obj
        }
    }

    pub fn get_obj_mut(&mut self, idx: ObjCounter) -> Result<&mut Object, &'static str> {
        match &mut self.objs[idx as usize] {
            Classifer::Raw(obj) => Ok(obj),
            Classifer::Readonly(obj) => Ok(obj),
            // Classifer::Readonly(_) => Err("failed to mutate")
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum ByteCode {
    Push(ObjPointer),
    Pop,

    Add,
    Sub,
    Mul,
    Div,
    Neg,

    Not,
    Equal,
    Less,
    Greater,
    Or,
    And,

    Print(ArgsCounter),

    GDef,
    GGet(GlobalCounter),
    GSet(GlobalCounter),
    LDef,
    LGet(LocalCounter),
    LSet(LocalCounter),

    Jump(InstPtr),
    JumpIfFalse(InstPtr),
    Call(ArgsCounter),

    Array(usize),
    ArrayGet,
    ArraySet,

    LStruct(LocalCounter),
    GStruct(GlobalCounter),
    MethodGet,
    FieldGet,
    FieldSet,
    StructImpl(MethodCounter),

    Ret,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Classifer<T> {
    Raw(T),
    Readonly(T),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Array),
    Function(Func),
    Method(Method),
    Struct(Struct),
    StructAlive(StructAlive),
    Nil,
}

impl Object {
    fn to_bool(self) -> bool {
        match self {
            Object::Bool(false) | Object::Nil => false,
            _ => true,
        }
    }

    fn to_string(&self, prog: &Program, tab_lvl: usize) -> String {
        match self {
            Object::Integer(val) => {
                format!("{}", val)
            }
            Object::Float(val) => {
                format!("{}", val)
            }
            Object::Bool(val) => {
                format!("{}", val)
            }
            Object::String(val) => {
                format!("{}", val)
            }
            Object::Function(val) => {
                format!("Func<{}>", val.name)
            }
            Object::Method(Method { func, owner }) => {
                let s1 = prog.get_obj(*func).to_string(prog, tab_lvl);
                let s2 = prog.get_obj(*owner).to_string(prog, tab_lvl);
                format!("Method<{}, {}>", s1, s2)
            }
            Object::Array(val) => {
                let mut s = String::from("[");
                for i in 0..val.items.len() {
                    let ptr = val.items[i];
                    let obj_str = prog.get_obj(ptr).to_string(prog, tab_lvl + 1);
                    if i+1 < val.items.len() {
                        s = format!("{}{}, ", s, obj_str);
                    } else {
                        s = format!("{}{}]", s, obj_str);
                    }
                }
                s
            },
            Object::Struct(typ) => {
                format!("Struct<{}>", typ.name)
            }
            Object::StructAlive(val) => {
                if let Object::Struct(base) = prog.get_obj(val.base) {
                    let mut s = format!("{} {{\n", base.name);
                    for (key, val) in &val.data {
                        let obj = prog.get_obj(val.clone());
                        s = format!("{s}{key: >tab$}: {}\n", obj.to_string(prog, tab_lvl+1), tab=(tab_lvl+1)*4);
                    }
                    format!("{s}}}")
                } else {
                    unreachable!()
                }
            }
            Object::Nil => {
                format!("nil")
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub name: String,
    pub members: HashMap<String, StructMember>,
    fields_count: usize,
    methods_count: usize,
}

impl Struct {
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields_count: 0,
            methods_count: 0,
            members: HashMap::new(),
        }
    }

    pub fn add_member(&mut self, name: String, member: StructMember) {
        match member {
            StructMember::Field { .. } => {
                self.fields_count += 1;
                self.members.insert(name, member)
            },
            StructMember::Method { .. } => {
                self.methods_count += 1;
                self.members.insert(name, member)
            }
        };
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum StructMember {
    Field { typ: Type },
    Method { ptr: ObjPointer }, // points to the function
}

#[derive(Clone, PartialEq, Debug)]
pub struct StructAlive {
    pub base: ObjPointer,
    pub data: HashMap<String, ObjPointer> // TODO: have a list of pointer instead
}

#[derive(Debug, Clone)]
pub struct Func {
    pub arity: usize,
    pub name: String,
    pub retype: Type,
    pub chunk: FuncCounter,
    pub is_method: bool,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity && self.retype == other.retype
    }
}

impl Func {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: 0,
            retype: Type::None,
            name: String::new(),
            is_method: false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
    func: ObjPointer,
    owner: ObjPointer, // points to `self`
}

#[derive(Clone)]
pub struct CallFrame {
    ip: InstPtr,
    func: Func,
    stack_offset: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Array {
    pub typ: Type,
    pub items: Vec<ObjPointer>,
}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        for i in 0..other.items.len() {
            if self.items[i] != other.items[i] {
                return false;
            }
        }
        return true;
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Float,
    Bool,
    String,
    Function,
    Array,
    Struct,
    None,
}

impl VM {
    pub fn new(prog: Program) -> Self {
        let frame = CallFrame {
            ip: 0,
            stack_offset: 0,
            func: Func::new(),
        };
        Self {
            prog,
            frame,
            stack: Vec::new(),
            call_stack: Vec::new(),
            globals: Vec::new(),
        }
    }

    #[inline]
    fn push(&mut self, ptr: ObjPointer) {
        self.stack.push(ptr);
    }

    #[inline]
    fn peek(&self, lvl: usize) -> ObjPointer {
        self.stack[self.stack.len() - (lvl + 1)]
    }

    #[inline]
    fn pop(&mut self) -> ObjPointer {
        self.stack.pop().unwrap()
    }

    #[inline]
    fn get_obj(&self, ptr: ObjPointer) -> &Object {
        self.prog.get_obj(ptr)
    }

    #[inline]
    fn get_obj_mut(&mut self, ptr: ObjPointer) -> Result<&mut Object, VMError> {
        let err = self.s_error("can't referece this mutably");
        if let Ok(obj) = self.prog.get_obj_mut(ptr) {
            Ok(obj)
        } else {
            Err(err)
        }
    }

    #[inline]
    fn push_obj(&mut self, obj: Object) {
        let fc = self.frame.func.chunk;
        let chunk = &mut self.prog.chunks[fc as usize];
        let oc = chunk.objs.len() as ObjCounter;
        chunk.objs.push(Classifer::Raw(obj));
        self.push((fc, oc))
    }

    #[allow(dead_code)]
    #[inline]
    fn peek_obj(&self, lvl: usize) -> &Object {
        let ptr = self.peek(lvl);
        self.get_obj(ptr)
    }

    #[allow(dead_code)]
    #[inline]
    fn peek_obj_mut(&mut self, lvl: usize) -> Result<&mut Object, VMError> {
        let ptr = self.peek(lvl);
        self.get_obj_mut(ptr)
    }

    #[inline]
    // BEWARE: this clones the value but doesn't get it's reference
    // delete the const out of the chunk
    fn pop_obj(&mut self) -> Object {
        let ptr = self.pop();
        self.get_obj(ptr).clone()
    }

    fn must_op<T>(&self, r: Option<T>, msg: &'static str) -> Result<T, VMError> {
        match r {
            Some(v) => Ok(v),
            None => Err(self.s_error(msg)),
        }
    }

    pub fn exec(mut self) -> Result<(), VMError> {
        while self.frame.ip < self.prog.chunks[self.frame.func.chunk as usize].count() {
            self.prog.chunk_disassemble_one(self.frame.func.chunk, self.frame.ip);
            let byte = self.prog.chunks[self.frame.func.chunk as usize].code[self.frame.ip];
            match byte {
                ByteCode::Push(i) => {
                    self.push(i);
                }
                ByteCode::Add
                | ByteCode::Sub
                | ByteCode::Mul
                | ByteCode::Div
                | ByteCode::Less
                | ByteCode::Greater => {
                    let y = self.pop_obj();
                    let x = self.pop_obj();
                    if let Object::Integer(x) = x
                        && let Object::Integer(y) = y
                    {
                        let obj = match byte {
                            ByteCode::Add => Object::Integer(
                                self.must_op(x.checked_add(y), "addition overflow")?,
                            ),
                            ByteCode::Sub => Object::Integer(
                                self.must_op(x.checked_sub(y), "subtraction overflow")?,
                            ),
                            ByteCode::Mul => Object::Integer(
                                self.must_op(x.checked_mul(y), "multiplication overflow")?,
                            ),
                            ByteCode::Div => Object::Integer(
                                self.must_op(x.checked_div(y), "division overflow")?,
                            ),
                            ByteCode::Less => {
                                Object::Bool(x < y)
                            },
                            ByteCode::Greater => {
                                Object::Bool(x > y)
                            },
                            _ => unreachable!(),
                        };
                        self.push_obj(obj);
                    } else if let Object::Float(x) = x
                        && let Object::Float(y) = y
                    {
                        let obj = match byte {
                            ByteCode::Add => Object::Float({
                                let r = x + y;
                                self.must_op(r.is_finite().then_some(r), "addition overflow")?
                            }),
                            ByteCode::Sub => Object::Float({
                                let r = x - y;
                                self.must_op(r.is_finite().then_some(r), "subtraction overflow")?
                            }),
                            ByteCode::Mul => Object::Float({
                                let r = x * y;
                                self.must_op(r.is_finite().then_some(r), "multiplication overflow")?
                            }),
                            ByteCode::Div => Object::Float({
                                let r = x / y;
                                self.must_op(r.is_finite().then_some(r), "division overflow")?
                            }),
                            ByteCode::Less => {
                                Object::Bool(x < y)
                            },
                            ByteCode::Greater => {
                                Object::Bool(x > y)
                            },
                            _ => unreachable!(),
                        };
                        self.push_obj(obj);
                    } else if let Object::String(x) = x
                        && let Object::String(y) = y
                    {
                        let obj = match byte {
                            ByteCode::Add => {
                                Object::String(String::from(x + y.as_str()))
                            }
                            _ => return Err(self.s_error("you can only concatenate strings with '+'"))
                        };
                        self.push_obj(obj);
                    } else {
                        return Err(
                            self.error(
                                format!("invalid operands for {:?}", byte)
                            )
                        )
                    };
                }
                ByteCode::Print(count) => {
                    // TODO: utterly slow
                    let mut buffer = String::new();
                    for _ in 0..count {
                        let ptr = self.pop();
                        let obj = self.get_obj(ptr);
                        buffer = format!("{}{}", obj.to_string(&self.prog, 1), buffer);
                    }
                    println!("{}", buffer);
                }
                ByteCode::Neg => {
                    let x = self.pop_int("operand is required to be an integer")?;
                    let obj = Object::Integer(-x);
                    self.push_obj(obj);
                }
                ByteCode::Or | ByteCode::And => {
                    let y = self.pop_bool("left operand is required to be a boolean")?;
                    let x = self.pop_bool("right operand is require to be a boolean")?;
                    let obj = match byte {
                        ByteCode::Or => {
                            Object::Bool(x || y)
                        },
                        ByteCode::And => {
                            Object::Bool(x && y)
                        },
                        _ => {
                            unreachable!()
                        },
                    };
                    self.push_obj(obj);
                }
                ByteCode::Equal => {
                    let y = self.pop_obj();
                    let x = self.pop_obj();
                    let obj = Object::Bool(x == y);
                    self.push_obj(obj);
                }
                ByteCode::Not => {
                    let x = self.pop_bool("operand expected to be a boolean")?;
                    let obj = Object::Bool(!x);
                    self.push_obj(obj);
                }
                ByteCode::Pop => {
                    self.pop();
                }
                ByteCode::GDef => {
                    let ptr = self.pop();
                    self.globals.push(ptr);
                }
                ByteCode::GGet(idx) => {
                    self.push(self.globals[idx as usize]);
                }
                // TODO: move typechecking to compile time
                ByteCode::GSet(idx) => {
                    let new_ptr = self.pop();
                    self.globals[idx as usize] = new_ptr;
                }
                ByteCode::LDef => (), // yeah
                ByteCode::LGet(idx) => {
                    self.push(self.stack[self.frame.stack_offset + idx])
                }
                // TODO: move typechecking to compile time
                ByteCode::LSet(idx) => {
                    let new_ptr = self.peek(0);
                    self.stack[self.frame.stack_offset + idx] = new_ptr;
                }
                ByteCode::Jump(dest) => {
                    self.frame.ip = dest - 1; // to make room for iterating
                }
                ByteCode::JumpIfFalse(dest) => {
                    if !self.pop_obj().to_bool() {
                        self.frame.ip = dest - 1;
                    }
                }
                ByteCode::Call(args_c) => {
                    let mut args = (0..args_c).map(|_| self.pop()).collect::<Vec<ObjPointer>>();
                    args.reverse();
                    // TODO: move call validation to compile-time so that you only need to store
                    // the function's pointer in the CallFrame and get rid of the chunk pointers
                    let obj = self.pop_obj();
                    match obj {
                        Object::Function(func) => {
                            if func.arity != args_c as usize {
                                return Err(
                                    self.error(
                                        format!("expected {} args got {}", func.arity, args_c)
                                    )
                                );
                            }
                            self.push_func(self.frame.clone());
                            self.frame.ip = 0;
                            self.frame.stack_offset = self.stack.len();
                            for arg in args {
                                self.push(arg);
                            }
                            self.frame.func = func;
                            continue;
                        }
                        Object::Method(method) => {
                            let func = match self.get_obj(method.func) {
                                Object::Function(func) => func.clone(),
                                _ => unreachable!()
                            };
                            if func.arity != (args_c as usize) + 1 {
                                return Err(
                                    self.error(
                                        format!("expected {} args got {}", func.arity - 1, args_c)
                                    )
                                );
                            }
                            self.push_func(self.frame.clone());
                            self.frame.ip = 0;
                            self.frame.stack_offset = self.stack.len();
                            self.push(method.owner); // push `self`
                            for arg in args {
                                self.push(arg);
                            }
                            self.frame.func = func;
                        }
                        _ => {
                            return Err(self.s_error("invalid call target"));
                        }
                    }
                }
                ByteCode::Ret => { // don't `continue;`
                    let ret_val_ptr = self.pop();
                    while self.stack.len() > self.frame.stack_offset { // pop all garbage
                                                                  // NOTE: this could have been
                                                                  // done at compile-time similar
                                                                  // to how blocks manage locals
                                                                  // but because of args I'll just
                                                                  // let it here as it is the same
                        self.pop();
                    }
                    self.frame = self.pop_func();
                    self.push(ret_val_ptr);
                }
                // NOTE: I'm not gonna typecheck arrays here as it is uselessly painful
                ByteCode::Array(count) => {
                    let typ = Type::None;
                    let mut items: Vec<ObjPointer> = (0..count).map(|_| self.pop()).collect();
                    items.reverse();
                    let obj = Object::Array(Array { typ, items });
                    self.push_obj(obj);
                }
                ByteCode::ArrayGet => {
                    let idx = self.pop_int("invalid index")?;
                    let ptr = self.pop();
                    if let Object::Array(arr) = self.get_obj(ptr) {
                        if !(0 <= idx && (idx as usize) < arr.items.len()) {
                            return Err(self.s_error("index out of bounds"));
                        }
                        self.push(arr.items[idx as usize]);
                    } else {
                        return Err(self.s_error("invalid index target"));
                    }
                }
                ByteCode::ArraySet => {
                    let val = self.pop();
                    let idx = self.pop_int("invalid index")?;
                    let ptr = self.pop();
                    if let Object::Array(arr) = self.get_obj_mut(ptr)? {
                        if !(0 <= idx && (idx as usize) < arr.items.len()) {
                            return Err(self.s_error("index out of bounds"));
                        }
                        arr.items[idx as usize] = val;
                    } else {
                        return Err(self.s_error("invalid index target"));
                    }
                    self.push(val);
                }
                ByteCode::LStruct(idx) => {
                    let base_ptr = self.stack[self.frame.stack_offset + idx];
                    self.push_struct(base_ptr);
                }
                ByteCode::GStruct(idx) => {
                    let base_ptr = self.globals[idx];
                    self.push_struct(base_ptr);
                }
                ByteCode::MethodGet => {
                    let method_name = match self.pop_obj() {
                        Object::String(name) => name,
                        _ => unreachable!()
                    };
                    if let Object::Struct(base) = self.pop_obj() {
                        if let Some(StructMember::Method { ptr }) = base.members.get(&method_name) { 
                            // println!("{:?} -> {:?}", ptr, self.get_obj(*ptr));
                            self.push(ptr.clone());
                        } else {
                            return Err(self.error(
                                    format!("invalid method reference in `struct {}`", base.name)
                            ));
                        }
                    } else {
                        unreachable!()
                    }
                }
                ByteCode::FieldGet => {
                    let field_name = match self.pop_obj() {
                        Object::String(name) => name,
                        _ => unreachable!()
                    };
                    let le_struct_ptr = self.pop();
                    match self.get_obj(le_struct_ptr) {
                        Object::StructAlive(le_struct) => {
                            // println!("{:?}", match self.get_obj(le_struct.base) {
                            //     Object::Struct(le_base) => le_base,
                            //     _ => unreachable!()
                            // });
                            println!("{:?}", le_struct);
                            // if it's a field
                            if let Some(ptr) = le_struct.data.get(&field_name) {
                                self.push(*ptr);
                            } 
                            // else if it's a method
                            else if let Some(StructMember::Method { ptr }) = self.must_get_struct_mut(le_struct.base)?.members.get(&field_name) {
                                let method = Method {
                                    owner: le_struct_ptr,
                                    func: *ptr,
                                };
                                self.push_obj(Object::Method(method));
                            } else {
                                return Err(self.error(
                                        // TODO: log struct name too
                                        format!("field {} doesn't exist", field_name)
                                ))
                            }
                        },
                        _ => unreachable!("expected struct instance")
                    }
                }
                ByteCode::FieldSet => {
                    let val = self.pop();
                    let field_name = match self.pop_obj() {
                        Object::String(name) => name,
                        _ => unreachable!()
                    };
                    let le_struct_ptr = self.pop();
                    if let Object::StructAlive(le_struct) = self.get_obj_mut(le_struct_ptr)? {
                        if let Some(field) = le_struct.data.get_mut(&field_name) {
                            *field = val;
                        } else {
                            return Err(self.error(
                                    // TODO: log struct name too
                                    format!("field {} doesn't exist", field_name)
                            ));
                        }
                    }
                    self.push(val);
                }
                ByteCode::StructImpl(methods_count) => {
                    let base_ptr = self.pop();
                    for _ in 0..methods_count {
                        let method_ptr = self.pop();
                        let method_name = match self.get_obj(method_ptr) {
                            Object::Function(le_method) => le_method.name.clone(),
                            _ => unreachable!()
                        };
                        // TODO: this is awful
                        if let Object::Struct(base) = self.get_obj_mut(base_ptr)? {
                            base.add_member(method_name, StructMember::Method { ptr: method_ptr });
                        } else {
                            unreachable!();
                        }
                    }
                }
            }
            self.frame.ip += 1;
            // println!("stack: {:?}", self.stack);
        }
        return Ok(());
    }

    fn must_get_struct_mut(&mut self, ptr: ObjPointer) -> Result<&mut Struct, VMError> {
        match self.get_obj_mut(ptr)? {
            Object::Struct(le_base) => Ok(le_base),
            _ => unreachable!()
        }
    }

    fn push_struct(&mut self, base_ptr: ObjPointer) {
        let base = match self.get_obj(base_ptr) {
            Object::Struct(base) => base,
            _ => unreachable!()
        };
        let mut data = HashMap::new();
        for _ in 0..base.fields_count {
            if let Object::String(k) = self.pop_obj() {
                let v = self.pop();
                data.insert(k, v);
            } else {
                unreachable!();
            }
        }
        let le_struct = StructAlive{ base: base_ptr, data };
        self.push_obj(Object::StructAlive(le_struct));
    }

    #[allow(unused_variables)]
    fn push_func(&mut self, frame: CallFrame) {
        self.call_stack.push(frame);
    }

    fn pop_func(&mut self) -> CallFrame {
        self.call_stack.pop().unwrap()
    }

    fn pop_bool(&mut self, msg: &'static str) -> Result<bool, VMError> {
        match self.pop_obj() {
            Object::Bool(x) => Ok(x),
            _ => Err(self.s_error(msg))
        }
    }

    fn pop_int(&mut self, msg: &'static str) -> Result<i32, VMError> {
        match self.pop_obj() {
            Object::Integer(x) => Ok(x),
            _ => Err(self.s_error(msg))
        }
    }

    fn s_error(&self, msg: &'static str) -> VMError {
        self.error(msg.into())
    }

    fn error(&self, msg: String) -> VMError {
        VMError {
            msg,
            tok: 
                self.prog.chunks[
                    self.frame.func.chunk as usize
                ].info[
                    self.frame.ip as usize
                ].clone()
        }
    }
}

pub type ObjCounter = u32;
pub type ObjPointer = (FuncCounter, ObjCounter);
pub type FuncCounter = u32;
pub type ArgsCounter = u8;
pub type LocalCounter = usize;
pub type GlobalCounter = usize;
pub type MethodCounter = u8;
pub type InstPtr = usize;

#[derive(Debug)]
pub struct VMError {
    msg: String,
    tok: TokenHeader,
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Error at {}-{}: {} at {}",
            self.tok.line, self.tok.coln, self.msg, self.tok.lexm
        )
    }
}

impl std::error::Error for VMError {}
