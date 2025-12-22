use std::io;
use std::{collections::HashMap, fmt::Debug};
use serde::{Serialize, Deserialize};

use crate::arena::{Arena, Pointer};
use crate::lexer::TokenHeader;

pub struct VM<'a> {
    // VM state
    prog: Program,
    stack: Vec<Pointer>,
    frame: CallFrame,
    call_stack: Vec<CallFrame>,

    // data
    arena: &'a mut Arena,
    globals: Vec<Pointer>,
}

#[derive(Serialize, Deserialize)]
pub struct Program {
    pub chunks: Vec<Chunk>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Chunk {
    code: Vec<ByteCode>,
    info: Vec<TokenHeader>, // info for code(info.len() = code.len())
}

#[derive(Clone)]
pub struct CallFrame {
    ip: IP,
    func: Func,
    stack_offset: usize,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub enum Classifier<T> {
    Runtime(T),
    Readonly(T),
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
pub enum Object {
    Byte(u8),
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(SynString),
    Array(Array),
    Function(Func),
    Method(Method),
    Struct(Struct),
    StructAlive(StructAlive),
    Nil,
}

#[repr(u8)]
#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub enum ByteCode {
    Push(Pointer),
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
    XOR,
    Modulo,
    LogicalAnd,
    LogicalOr,

    Print(ArgsCounter),

    GDef,
    GGet(GlobalCounter),
    GSet(GlobalCounter),
    LDef,
    LGet(LocalCounter),
    LSet(LocalCounter),

    Jump(IP),
    JumpIfFalse(IP),
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

    Assert,
    Panic,

    Ret,
}

#[allow(dead_code)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Type {
    Byte,
    Integer,
    Float,
    Bool,
    SynString,
    Function,
    Array,
    Struct,
    None,
}

type SynString = String;

#[allow(dead_code)]
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Array {
    pub len: usize,
    pub base: Pointer,
}

impl PartialEq for Array {
    fn eq(&self, other: &Array) -> bool {
        return self.len == other.len; // TODO: fix this
    }
}

#[allow(dead_code)]
#[derive(Serialize, Deserialize, Debug, Clone)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Method {
    func: Pointer,
    owner: Pointer, // points to `self`
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub struct Struct {
    pub name: String,
    pub members: HashMap<String, StructMember>,
    fields_count: usize,
    methods_count: usize,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub enum StructMember {
    Field { typ: Type },
    Method { ptr: Pointer }, // points to the function
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub struct StructAlive {
    pub base: Pointer,
    pub data: HashMap<String, Pointer> // TODO: have a list of pointer instead
}

pub type FuncCounter = usize;
pub type LocalCounter = usize;
pub type GlobalCounter = usize;
pub type ArgsCounter = u8;
pub type MethodCounter = u8;
pub type IP = usize;

impl<'a> VM<'a> {
    pub fn new(arena: &'a mut Arena, prog: Program) -> Self {
        Self {
            prog,
            arena,
            frame: CallFrame {
                ip: 0,
                stack_offset: 0,
                func: Func {
                    arity: 0,
                    name: String::from("__global__"),
                    retype: Type::None,
                    chunk: 0,
                    is_method: false,
                },
            },
            stack: Vec::new(),
            call_stack: Vec::new(),
            globals: Vec::new(),
        }
    }

    #[inline]
    fn get_obj(&self, ptr: Pointer) -> &Object {
        self.arena.get_obj(ptr).
            expect(
                format!("{} @ {:04X}: failed to access address {:04X}", self.frame.func.name, self.frame.ip, ptr).as_str()
            )
    }

    #[inline]
    fn get_obj_mut(&mut self, ptr: Pointer) -> &mut Object {
        self.arena.get_obj_mut(ptr).
            expect(
                format!("{} @ {:04X}: failed to mutuably access address {:04X}", self.frame.func.name, self.frame.ip, ptr).as_str()
            )
    }

    #[inline]
    fn stack_push(&mut self, ptr: Pointer) {
        self.stack.push(ptr);
    }

    #[inline]
    fn stack_peek(&self, lvl: usize) -> Pointer {
        self.stack[self.stack.len() - (lvl + 1)]
    }

    #[inline]
    fn stack_pop(&mut self) -> Pointer {
        self.stack.pop().expect(format!("{} @ {:04X} ~ stack smashing detected", self.frame.func.name, self.frame.ip).as_str())
    }

    #[inline]
    fn stack_push_runtime_obj(&mut self, obj: Object) {
        let ptr = self.arena.push_obj(Classifier::Runtime(obj));
        self.stack_push(ptr);
    }

    #[inline]
    fn stack_pop_obj(&mut self) -> Object {
        let ptr = self.stack_pop();
        self.get_obj(ptr).clone()
    }

    fn stack_peek_obj(&self, lvl: usize) -> &Object {
        let ptr = self.stack_peek(lvl);
        self.get_obj(ptr)
    }

    fn raw_error(&self, msg: String) -> VMError {
        VMError {
            msg,
            tok: self.prog.chunks[
                    self.frame.func.chunk as usize
                ].info[
                    self.frame.ip as usize
                ].clone()
        }
    }

    fn error<T>(&self, msg: String) -> Result<T, VMError> {
        Err(self.raw_error(msg))
    }

    // TODO: replace with by macro
    fn must_op<T>(&self, a: Option<T>, msg: &'static str) -> Result<T, VMError> {
        match a {
            Some(val) => Ok(val),
            None => self.error(msg.into()),
        }
    }

    pub fn exec(mut self) -> Result<(), VMError> {
        while self.frame.ip < self.prog.chunks[self.frame.func.chunk as usize].count() {
            // self.prog.chunk_disassemble_one(self.frame.func.chunk, self.frame.ip);
            let bytecode = self.prog.chunks[self.frame.func.chunk as usize].code[self.frame.ip];
            match bytecode {
                ByteCode::Push(ptr) => {
                    self.stack_push(ptr);
                }
                ByteCode::Pop => {
                    self.stack_pop();
                }
                ByteCode::Add
                | ByteCode::Sub
                | ByteCode::Mul
                | ByteCode::Div
                | ByteCode::Less
                | ByteCode::Greater
                | ByteCode::LogicalOr
                | ByteCode::XOR 
                | ByteCode::LogicalAnd 
                | ByteCode::Modulo => {
                    let y = self.stack_pop_obj();
                    let x = self.stack_pop_obj();
                    if let Object::Integer(x) = x && let Object::Integer(y) = y {
                        let obj = match bytecode {
                            ByteCode::Add => Object::Integer(
                                self.must_op(x.checked_add(y), "invalid addition")?,
                            ),
                            ByteCode::Sub => Object::Integer(
                                self.must_op(x.checked_sub(y), "invalid subtraction")?,
                            ),
                            ByteCode::Mul => Object::Integer(
                                self.must_op(x.checked_mul(y), "invalid multiplication")?,
                            ),
                            ByteCode::Div => Object::Integer(
                                self.must_op(x.checked_div(y), "invalid division")?,
                            ),
                            ByteCode::Modulo => Object::Integer(
                                self.must_op(x.checked_rem_euclid(y), "invalid modulo")?
                            ),
                            ByteCode::Less => {
                                Object::Bool(x < y)
                            },
                            ByteCode::Greater => {
                                Object::Bool(x > y)
                            },
                            ByteCode::LogicalOr => {
                                Object::Integer(x | y)
                            },
                            ByteCode::XOR => {
                                Object::Integer(x ^ y)
                            },
                            ByteCode::LogicalAnd => {
                                Object::Integer(x & y)
                            },
                            _ => return self.error("invalid opreator on integer".into())
                        };
                        self.stack_push_runtime_obj(obj);
                    } else if let Object::Float(x) = x && let Object::Float(y) = y {
                        let obj = match bytecode {
                            ByteCode::Add => Object::Float({
                                let z = x + y;
                                self.must_op(z.is_finite().then_some(z), "addition overflow")?
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
                            ByteCode::Modulo => Object::Float({
                                let r = x.rem_euclid(y);
                                self.must_op(r.is_finite().then_some(r), "invalid modulo")?
                            }),
                            ByteCode::Less => {
                                Object::Bool(x < y)
                            },
                            ByteCode::Greater => {
                                Object::Bool(x > y)
                            },
                            _ => return self.error("invalid operator on float".into())
                        };
                        self.stack_push_runtime_obj(obj);
                    } else if let Object::Byte(x) = x && let Object::Byte(y) = y {
                        let obj = match bytecode {
                            ByteCode::Add => Object::Byte(
                                self.must_op(x.checked_add(y), "invalid addition")?,
                            ),
                            ByteCode::Sub => Object::Byte(
                                self.must_op(x.checked_sub(y), "invalid subtraction")?,
                            ),
                            ByteCode::Mul => Object::Byte(
                                self.must_op(x.checked_mul(y), "invalid multiplication")?,
                            ),
                            ByteCode::Div => Object::Byte(
                                self.must_op(x.checked_div(y), "invalid division")?,
                            ),
                            ByteCode::Modulo => Object::Byte(
                                self.must_op(x.checked_rem_euclid(y), "invalid modulo")?
                            ),
                            ByteCode::Less => {
                                Object::Bool(x < y)
                            },
                            ByteCode::Greater => {
                                Object::Bool(x > y)
                            },
                            ByteCode::LogicalOr => {
                                Object::Byte(x | y)
                            },
                            ByteCode::XOR => {
                                Object::Byte(x ^ y)
                            },
                            ByteCode::LogicalAnd => {
                                Object::Byte(x & y)
                            },
                            _ => return self.error("invalid opreator on byte".into())
                        };
                        self.stack_push_runtime_obj(obj);
                    } else if let Object::String(x) = x && let Object::String(y) = y {
                        let obj = match bytecode {
                            ByteCode::Add => {
                                Object::String(String::from(x + y.as_str()))
                            }
                            _ => return self.error("invalid operator on strings".into())
                        };
                        self.stack_push_runtime_obj(obj);
                    } else {
                        return self.error(format!("invalid operands for {bytecode:?}"));
                    };
                }
                ByteCode::Print(count) => {
                    let mut vec = (0..count).map(|_| {
                        let obj = self.stack_pop_obj();
                        self.obj_to_str(&obj)
                    }).collect::<Vec<String>>(); vec.reverse();
                    println!("{}", vec.join(""));
                }
                ByteCode::Neg => {
                    let obj = match self.stack_pop_obj() {
                        Object::Float(val) => Object::Float(-val),
                        Object::Integer(val) => Object::Integer(-val),
                        _ => return self.error("operand is required to be an integer".into())
                    };
                    self.stack_push_runtime_obj(obj);
                }
                ByteCode::Or | ByteCode::And => {
                    let (y, x) = (self.stack_pop_obj(), self.stack_pop_obj());
                    let obj = match bytecode {
                        ByteCode::Or => Object::Bool(x.to_bool() || y.to_bool()),
                        ByteCode::And => Object::Bool(x.to_bool() && y.to_bool()),
                        _ => unreachable!(),
                    };
                    self.stack_push_runtime_obj(obj);
                }
                ByteCode::Equal => {
                    let (y, x) = (self.stack_pop_obj(), self.stack_pop_obj());
                    self.stack_push_runtime_obj(Object::Bool(x == y));
                }
                ByteCode::Not => {
                    let old = self.stack_pop_obj();
                    let new = match old {
                        Object::Bool(val) => Object::Bool(!val),
                        Object::Byte(val) => Object::Byte(!val),
                        Object::Integer(val) => Object::Integer(!val), // applies 2's complement...
                                                                       // TODO: add '~' operator
                        _ => return self.error(format!("cannot apply unary operator '!' to {}", self.obj_to_str(&old)))
                    };
                    self.stack_push_runtime_obj(new);
                }
                ByteCode::GDef => {
                    let ptr = self.stack_pop();
                    self.globals.push(ptr);
                }
                ByteCode::GGet(idx) => {
                    self.stack_push(self.globals[idx]);
                }
                ByteCode::GSet(idx) => {
                    self.globals[idx] = self.stack_peek(0);
                }
                ByteCode::LDef => (), // yeah
                ByteCode::LGet(idx) => {
                    self.stack_push(self.stack[self.frame.stack_offset + idx])
                }
                ByteCode::LSet(idx) => {
                    self.stack[self.frame.stack_offset + idx] = self.stack_peek(0);
                }
                ByteCode::Jump(dest) => {
                    self.frame.ip = dest - 1; // to make room for next iteration
                }
                ByteCode::JumpIfFalse(dest) => {
                    if !self.stack_pop_obj().to_bool() {
                        self.frame.ip = dest - 1;
                    }
                }
                ByteCode::Call(args_c) => {
                    let mut args = (0..args_c).map(|_| self.stack_pop()).collect::<Vec<_>>(); args.reverse();
                    match self.stack_pop_obj() {
                        Object::Function(func) => {
                            if func.arity != args_c as usize {
                                return self.error(format!("expected {} args got {}", func.arity, args_c));
                            }
                            self.call_stack.push(self.frame.clone());
                            self.frame.ip = 0;
                            self.frame.stack_offset = self.stack.len();
                            for arg in args {
                                self.stack_push(arg);
                            }
                            self.frame.func = func;
                            continue;
                        }
                        Object::Method(method) => {
                            let func = match self.get_obj(method.func) {
                                Object::Function(func) => func.clone(),
                                _ => panic!("vm::VM::exec @ match arm ByteCode::Call @ Object::Method: invalid func")
                            };
                            if func.arity != 1 + args_c as usize { // because of `self`
                                return self.error(format!("expected {} args got {}", func.arity - 1, args_c));
                            }
                            self.call_stack.push(self.frame.clone());
                            self.frame.ip = 0;
                            self.frame.stack_offset = self.stack.len();
                            self.stack_push(method.owner); // push `self`
                            for arg in args {
                                self.stack_push(arg);
                            }
                            self.frame.func = func;
                            continue;
                        }
                        _ => {
                            return self.error("invalid call target".into());
                        }
                    }
                }
                ByteCode::Ret => {
                    let ret_val_ptr = self.stack_pop();
                    while self.stack.len() > self.frame.stack_offset {
                        self.stack_pop();
                    }
                    self.frame = self.call_stack.pop().expect("vm::VM::exec @ match arm ByteCode::Ret: expected to return");
                    self.stack_push(ret_val_ptr);
                }
                ByteCode::Array(len) => {
                    let mut objs = Vec::new();
                    for _ in 0..len {
                        objs.push(Classifier::Runtime(self.stack_pop_obj()));
                    }
                    objs.reverse();
                    let base = self.arena.push_objs(objs);
                    let obj = Object::Array(Array { len, base });
                    self.stack_push_runtime_obj(obj);
                }
                // TODO: push array at compile-time
                ByteCode::ArrayGet => {
                    let idx_obj = self.stack_pop_obj();
                    match self.stack_pop_obj() {
                        Object::Array(arr) => {
                            let idx = match idx_obj {
                                Object::Byte(idx) => idx as isize,
                                Object::Integer(idx) => idx as isize,
                                _ => return self.error("invalid index".into())
                            };
                            if idx < 0 || arr.len <= idx as usize {
                                return self.error("index out of bounds".into());
                            }
                            self.stack_push(arr.base + idx as usize);
                        },
                        _ => return self.error("invalid indexing target".into())
                    }
                }
                ByteCode::ArraySet => {
                    let val = self.stack_pop();
                    let idx_obj = self.stack_pop_obj();
                    match self.stack_pop_obj() {
                        Object::Array(arr) => {
                            let idx = match idx_obj {
                                Object::Byte(idx) => idx as isize,
                                Object::Integer(idx) => idx as isize,
                                _ => return self.error("invalid index".into())
                            };
                            if idx < 0 || arr.len <= idx as usize {
                                return self.error("index out of bounds".into());
                            }
                            *self.get_obj_mut(arr.base + idx as usize) = self.get_obj(val).clone();
                        },
                        _ => return self.error("invalid indexing target".into())
                    }
                    self.stack_push(val);
                }
                ByteCode::LStruct(idx) => {
                    let base_ptr = self.stack[self.frame.stack_offset + idx];
                    let base = match self.get_obj(base_ptr) {
                        Object::Struct(base) => base,
                        _ => panic!("vm::VM::exec @ match arm ByteCode::LStruct: invalid base struct")
                    };
                    let mut data = HashMap::new();
                    for _ in 0..base.fields_count {
                        if let Object::String(k) = self.stack_pop_obj() {
                            data.insert(k, self.stack_pop());
                        } else {
                            panic!("vm::VM::exec @ match arm ByteCode::LStruct: invalid value kind")
                        }
                    }
                    let le_struct = StructAlive{ base: base_ptr, data };
                    self.stack_push_runtime_obj(Object::StructAlive(le_struct));
                }
                ByteCode::GStruct(idx) => {
                    let base_ptr = self.globals[idx];
                    let base = match self.get_obj(base_ptr) {
                        Object::Struct(base) => base,
                        _ => panic!("vm::VM::exec @ match arm ByteCode::GStruct: invalid base struct")
                    };
                    let mut data = HashMap::new();
                    for _ in 0..base.fields_count {
                        if let Object::String(k) = self.stack_pop_obj() {
                            data.insert(k, self.stack_pop());
                        } else {
                            panic!("vm::VM::exec @ match arm ByteCode::GStruct: invalid value kind")
                        }
                    }
                    let le_struct = StructAlive{ base: base_ptr, data };
                    self.stack_push_runtime_obj(Object::StructAlive(le_struct));
                }
                ByteCode::MethodGet => {
                    let method_name = match self.stack_pop_obj() {
                        Object::String(name) => name,
                        _ => panic!("vm::VM::exec @ match arm ByteCode::MethodGet: invalid method name")
                    };
                    if let Object::Struct(base) = self.stack_pop_obj() {
                        if let Some(StructMember::Method { ptr }) = base.members.get(&method_name) { 
                            self.stack_push(*ptr);
                        } else {
                            return self.error(format!("invalid method reference in `struct {}`", base.name));
                        }
                    } else {
                        panic!("vm::VM::exec @ match arm ByteCode::MethodGet: invalid struct")
                    }
                }
                ByteCode::FieldGet => {
                    let field_name = match self.stack_pop_obj() {
                        Object::String(name) => name,
                        _ => panic!("vm::VM::exec: match arm ByteCode::FieldGet ~ invalid field name")
                    };
                    let struct_ptr = self.stack_pop();
                    if let Object::StructAlive(le_struct) = self.get_obj(struct_ptr) {
                        // if it's a field
                        if let Some(field_ptr) = le_struct.data.get(&field_name) {
                            self.stack_push(*field_ptr);
                        } else {
                            // else if it's a method
                            let le_base = match self.get_obj_mut(le_struct.base) {
                                Object::Struct(le_base) => le_base,
                                _ => panic!("vm::VM::exec @ match arm ByteCode::FieldGet: invalid struct base")
                            };
                            if let Some(StructMember::Method { ptr }) = le_base.members.get(&field_name) {
                                let method = Method {
                                    owner: struct_ptr,
                                    func: *ptr,
                                };
                                self.stack_push_runtime_obj(Object::Method(method));
                            } else {
                                return self.error(format!("field {field_name} doesn't exist"));
                            }
                        }
                    }
                }
                ByteCode::FieldSet => {
                    let val_set = self.stack_pop();
                    let field_name = match self.stack_pop_obj() {
                        Object::String(name) => name,
                        _ => panic!("vm::VM::exec: match arm ByteCode::FieldSet")
                    };
                    let struct_ptr = self.stack_pop();
                    if let Object::StructAlive(instance) = self.get_obj_mut(struct_ptr) {
                        if let Some(field) = instance.data.get_mut(&field_name) {
                            *field = val_set;
                        } else {
                            return self.error(format!("no field {field_name} on struct instance"));
                        }
                    } else {
                        panic!("vm::VM::exec: match arm ByteCode::FieldSet.");
                    }
                    self.stack_push(val_set);
                }
                ByteCode::StructImpl(methods_count) => {
                    let base_ptr = self.stack_pop();
                    for _ in 0..methods_count {
                        let method_ptr = self.stack_pop();
                        let method_name = match self.get_obj(method_ptr) {
                            Object::Function(le_method) => le_method.name.clone(),
                            _ => unreachable!()
                        };
                        if let Object::Struct(base) = self.get_obj_mut(base_ptr) {
                            base.add_member(method_name, StructMember::Method { ptr: method_ptr });
                        } else {
                            panic!("vm::VM::exec: match arm ByteCode::StructImpl.");
                        }
                    }
                }
                ByteCode::Assert => {
                    if !self.stack_peek_obj(0).to_bool() {
                        let chunk = &self.prog.chunks[self.frame.func.chunk as usize];
                        let msg = chunk.get_string_at_line(chunk.info[self.frame.ip].line);
                        println!("RUNTIME PANIC: {}", msg);
                        std::process::exit(69);
                    }
                }
                ByteCode::Panic => {
                    let obj = self.stack_pop_obj();
                    println!("RUNTIME PANIC: {}", self.obj_to_str(&obj));
                    std::process::exit(69);
                }
            }
            self.frame.ip += 1;
            // println!("stack: {:?}", self.stack);
        }
        return Ok(());
    }
}

impl<'a> VM<'a> {
    #[allow(unused)]
    pub fn disassemble_chunk(&self, chunk: FuncCounter) {
        println!("===== Chunk {chunk:04X} =====");
        let chunk = &self.prog.chunks[chunk];
        for (ip, bytecode) in chunk.code.iter().enumerate() {
            match bytecode {
                ByteCode::Push(ptr) => println!("{ip:04X}: Push({ptr}) <--- {}", self.obj_to_str(self.get_obj(*ptr))),
                _ => println!("{ip:04X}: {bytecode:?}"),
            };
        }
    }

    pub fn disassemble_program(&self) {
        for chunk in 0..self.prog.chunks.len() {
            self.disassemble_chunk(chunk);
            println!();
        }
    }

    fn obj_to_str(&self, obj: &Object) -> String {
        match obj {
            Object::Byte(val) => {
                val.to_string()
            }
            Object::Integer(val) => {
                val.to_string()
            }
            Object::Float(val) => {
                val.to_string()
            }
            Object::Bool(val) => {
                val.to_string()
            }
            Object::String(val) => {
                val.to_string()
            }
            Object::Function(val) => {
                format!("Func<{}>", val.name)
            }
            Object::Method(Method { func, owner }) => {
                let func = self.get_obj(*func);
                let owner = self.get_obj(*owner);
                format!("Method<{}, {}>", self.obj_to_str(func), self.obj_to_str(owner))
            }
            Object::Array(arr) => {
                let mut s = String::from("[");
                for i in 0..arr.len {
                    let obj = self.get_obj(arr.base + i);
                    if i + 1 < arr.len {
                        s = format!("{}{}, ", s, self.obj_to_str(obj));
                    } else {
                        s = format!("{}{}]", s, self.obj_to_str(obj));
                    }
                }
                s
            },
            Object::Struct(typ) => {
                format!("Struct<{}>", typ.name)
            }
            Object::StructAlive(instance) => {
                let base_obj = self.get_obj(instance.base);
                if let Object::Struct(base) = base_obj {
                    let mut s = format!("{} {{ ", base.name);
                    for (key, ptr) in &instance.data {
                        let val = self.get_obj(*ptr);
                        let field_str = format!("{}: {}, ", key, self.obj_to_str(val));
                        s.push_str(field_str.as_str())
                    }
                    format!("{s}}}\n")
                } else {
                    unreachable!("vm::VM::obj_to_str: match arm Object::StructAlive.")
                }
            }
            Object::Nil => {
                "nil".to_string()
            }
        }
    }
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

    pub fn write_to_file(&self, filepath: &str) -> std::io::Result<()> { 
        let buf = postcard::to_stdvec(&self).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        std::fs::write(filepath, buf)
    }

    pub fn read_from_file(filepath: &str) -> std::io::Result<Program> {
        let bytes = std::fs::read(filepath)?;
        postcard::from_bytes(&bytes).map_err(|err| io::Error::new(io::ErrorKind::Other, err))
    }
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            info: Vec::new(),
            code: Vec::new(),
        }
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn push_instruction(&mut self, b: ByteCode, t: TokenHeader) -> usize {
        self.info.push(t);
        self.code.push(b);
        self.code.len() - 1
    }

    pub fn push_instructions(&mut self, b1: ByteCode, b2: ByteCode, t: TokenHeader) -> usize {
        self.push_instruction(b1, t.clone());
        self.push_instruction(b2, t)
    }

    pub fn set(&mut self, idx: usize, b: ByteCode) {
        self.code[idx] = b;
    }

    pub fn get_string_at_line(&self, _: usize) -> &str {
        "TODO: get_string_at_line"
    }
}

impl Object {
    fn to_bool(&self) -> bool {
        match self {
            Object::Bool(false) | Object::Nil => false,
            _ => true,
        }
    }
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
