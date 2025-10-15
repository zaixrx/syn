pub struct VM {
    globals: Vec<ObjPointer>,
    stack: Vec<ObjPointer>,
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

    pub fn disassemble(&self) {
        println!("== PROG_START ==");
        for fc in 0..self.chunks.len() {
            self.chunk_disassemble(fc as FuncCounter);
            println!();
        }
        println!("== PROG_START ==");
    }

    pub fn get_top_level_chunk(&mut self) -> &mut Chunk {
        &mut self.chunks[0]
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

    pub fn chunk_load_const(&mut self, fc: FuncCounter, obj: Object) -> Result<ObjCounter, &'static str> {
        let chunk = &mut self.chunks[fc as usize];
        let oc = chunk.objs.len() as ObjCounter;
        if oc >= ObjCounter::MAX {
            Err("can't define more constants in chunk")
        } else {
            chunk.objs.push(obj);
            chunk.push(ByteCode::Push((fc, oc)));
            Ok(oc)
        }
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
}

#[derive(Clone, Debug)]
pub struct Chunk {
    code: Vec<ByteCode>,
    objs: Vec<Object>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            objs: Vec::new(),
            code: Vec::new(),
        }
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn push(&mut self, b: ByteCode) -> usize {
        self.code.push(b);
        self.code.len() - 1
    }

    pub fn pushs(&mut self, b1: ByteCode, b2: ByteCode) -> usize {
        self.push(b1);
        self.push(b2)
    }

    pub fn set(&mut self, idx: usize, b: ByteCode) {
        self.code[idx] = b;
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

    Ret,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Array),
    Function(Func),
    Nil,
}

impl Object {
    fn to_bool(self) -> bool {
        match self {
            Object::Bool(false) | Object::Nil => false,
            _ => true,
        }
    }

    fn expect_integer(self, msg: &'static str) -> Result<i32, &'static str> {
        match self {
            Object::Integer(x) => Ok(x),
            _ => Err(msg),
        }
    }

    fn expect_bool(self, msg: &'static str) -> Result<bool, &'static str> {
        match self {
            Object::Bool(x) => Ok(x),
            _ => Err(msg),
        }
    }
    
    fn get_var_type(&self) -> Type {
        match self {
            Object::Integer(_) => Type::Integer,
            Object::Float(_) => Type::Float,
            Object::Bool(_) => Type::Bool,
            Object::String(_) => Type::String,
            Object::Function(_) => Type::Function,
            Object::Array(_) => Type::Array,
            Object::Nil => Type::None,
        }
    }

    fn is_of_type(&self, other: &Object) -> bool {
        let t1 = self.get_var_type();
        let t2 = other.get_var_type();
        t1 == t2 || t1 == Type::None || t2 == Type::None
    }

    fn to_string(&self, prog: &Program) -> String {
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
            Object::Array(val) => {
                let mut s = String::from("[");
                for i in 0..val.items.len() {
                    let ptr = val.items[i];
                    let obj_str = prog.chunks[ptr.0 as usize].objs[val.items[i].1 as usize].to_string(prog);
                    if i+1 < val.items.len() {
                        s = format!("{}{}, ", s, obj_str);
                    } else {
                        s = format!("{}{}]", s, obj_str);
                    }
                }
                s
            }
            Object::Nil => {
                format!("nil")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub arity: usize,
    pub name: String,
    pub retype: Type,
    pub chunk: FuncCounter,
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
        }
    }
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
    Struct(String),
    None,
}

macro_rules! err {
    ($x: expr) => {
        match $x {
            Ok(idx) => idx,
            Err(msg) => return Err(VMError::from(msg))
        }
    };
}

impl VM {
    pub fn new() -> Self {
        Self {
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
    fn get_obj<'a>(&self, ptr: ObjPointer, prog: &'a Program) -> &'a Object {
        &prog.chunks[ptr.0 as usize].objs[ptr.1 as usize]
    }

    #[inline]
    fn get_obj_mut<'a>(&self, ptr: ObjPointer, prog: &'a mut Program) -> &'a mut Object {
        &mut prog.chunks[ptr.0 as usize].objs[ptr.1 as usize]
    }

    #[inline]
    fn push_obj(&mut self, obj: Object, prog: &mut Program, fc: FuncCounter) {
        let chunk = &mut prog.chunks[fc as usize];
        let ptr = (fc, chunk.objs.len() as ObjCounter) as ObjPointer;
        chunk.objs.push(obj);
        self.push(ptr)
    }

    #[inline]
    fn peek_obj<'a>(&self, lvl: usize, prog: &'a Program) -> &'a Object {
        let ptr = self.peek(lvl);
        self.get_obj(ptr, prog)
    }

    fn peek_obj_mut<'a>(&self, lvl: usize, prog: &'a mut Program) -> &'a mut Object {
        let ptr = self.peek(lvl);
        self.get_obj_mut(ptr, prog)
    }

    #[inline]
    // delete the const out of the chunk
    fn pop_obj(&mut self, prog: &Program) -> Object {
        let ptr = self.pop();
        self.get_obj(ptr, prog).clone()
    }

    fn must_op<T>(&self, r: Option<T>, msg: &'static str) -> Result<T, VMError> {
        match r {
            Some(v) => Ok(v),
            None => Err(VMError::from(msg)),
        }
    }

    pub fn exec(mut self, mut prog: Program) -> Result<(), VMError> {
        let mut frame = CallFrame {
            ip: 0,
            stack_offset: 0,
            func: Func::new(),
        };
        while frame.ip < prog.chunks[frame.func.chunk as usize].count() {
            // chunk.disassemble_one(frame.ip);
            let byte = prog.chunks[frame.func.chunk as usize].code[frame.ip];
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
                    let y = self.pop_obj(&prog);
                    let x = self.pop_obj(&prog);
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
                        self.push_obj(obj, &mut prog, frame.func.chunk);
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
                        self.push_obj(obj, &mut prog, frame.func.chunk);
                    } else if let Object::String(x) = x
                        && let Object::String(y) = y
                    {
                        let obj = match byte {
                            ByteCode::Add => {
                                Object::String(String::from(x + y.as_str()))
                            }
                            _ => return Err(format!("you can only concatenate strings with '+'"))
                        };
                        self.push_obj(obj, &mut prog, frame.func.chunk);
                    } else {
                        return Err(format!("invalid operands for {:?}", byte))
                    };
                }
                ByteCode::Print(count) => {
                    // TODO: utterly slow
                    let mut buffer = String::new();
                    for _ in 0..count {
                        let ptr = self.pop();
                        let obj = self.get_obj(ptr, &prog);
                        buffer = format!("{}{}", obj.to_string(&prog), buffer);
                    }
                    println!("{}", buffer);
                }
                ByteCode::Neg => {
                    let x = err!(self.pop_obj(&prog).expect_integer("operand is required to be an integer"));
                    let obj = Object::Integer(-x);
                    self.push_obj(obj, &mut prog, frame.func.chunk);
                }
                ByteCode::Or | ByteCode::And => {
                    let y = err!(self.pop_obj(&prog).expect_bool("left operand is required to be a boolean"));
                    let x = err!(self.pop_obj(&prog).expect_bool("right operand is require to be a boolean"));
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
                    self.push_obj(obj, &mut prog, frame.func.chunk);
                }
                ByteCode::Equal => {
                    let y = self.pop_obj(&prog);
                    let x = self.pop_obj(&prog);
                    let obj = Object::Bool(x == y);
                    self.push_obj(obj, &mut prog, frame.func.chunk);
                }
                ByteCode::Not => {
                    let x = err!(self.pop_obj(&prog).expect_bool("operand expected to be a boolean"));
                    let obj = Object::Bool(!x);
                    self.push_obj(obj, &mut prog, frame.func.chunk);
                }
                ByteCode::Pop => {
                    self.pop_obj(&prog);
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
                    // RUNTIME_TYPECHECKING_START
                    let new_val = self.get_obj(new_ptr, &prog);
                    let old_ptr = self.globals[idx as usize];
                    let old_val = self.get_obj(old_ptr, &prog);
                    if !new_val.is_of_type(old_val) {
                        return Err(format!(
                            "mismatched types ({:?} != {:?})",
                            new_val.get_var_type(),
                            old_val.get_var_type()
                        ));
                    }
                    // RUNTIME_TYPECHECKING_END
                    self.globals[idx as usize] = new_ptr;
                }
                ByteCode::LDef => (), // yeah
                ByteCode::LGet(idx) => {
                    self.push(self.stack[frame.stack_offset + idx as usize])
                }
                // TODO: move typechecking to compile time
                ByteCode::LSet(idx) => {
                    let new_ptr = self.peek(0);
                    // RUNTIME_TYPECHECKING_START
                    let new_val = self.get_obj(new_ptr, &prog);
                    let old_ptr = self.stack[frame.stack_offset + idx as usize];
                    let old_val = self.get_obj(old_ptr, &prog);
                    if !new_val.is_of_type(old_val) {
                        return Err(format!(
                            "mismatched types ({:?} != {:?})",
                            new_val.get_var_type(),
                            old_val.get_var_type()
                        ));
                    }
                    // RUNTIME_TYPECHECKING_END
                    self.stack[frame.stack_offset + idx as usize] = new_ptr;
                }
                ByteCode::Jump(dest) => {
                    frame.ip = dest - 1; // to make room for iterating
                }
                ByteCode::JumpIfFalse(dest) => {
                    if !self.pop_obj(&prog).to_bool() {
                        frame.ip = dest - 1;
                    }
                }
                ByteCode::Call(args_c) => {
                    let args = (0..args_c).map(|_| self.pop()).rev().collect::<Vec<ObjPointer>>();
                    // TODO: move call validation to compile-time so that you only need to store
                    // the function's pointer in the CallFrame and get rid of the chunk pointers
                    let obj = self.pop_obj(&prog);
                    if let Object::Function(func) = obj {
                        // TODO: typecheck args types also
                        if func.arity != args_c as usize {
                            return Err(format!("expected {} args got {}", func.arity, args_c));
                        }
                        self.push_func(frame.clone());
                        frame.ip = 0;
                        frame.func = func;
                        frame.stack_offset = self.stack.len();
                        for arg in args {
                            self.push(arg);
                        }
                        continue;
                    } else {
                        return Err(VMError::from("invalid call target"));
                    }
                }
                ByteCode::Ret => { // don't `continue;`
                    let ret_val_ptr = self.pop();
                    while self.stack.len() > frame.stack_offset { // pop all garbage
                                                                  // NOTE: this could have been
                                                                  // done at compile-time similar
                                                                  // to how blocks manage locals
                                                                  // but because of args I'll just
                                                                  // let it here as it is the same
                        self.pop();
                    }
                    frame = self.pop_func();
                    self.push(ret_val_ptr);
                }
                // NOTE: I'm not gonna typecheck arrays here as it is uselessly painful
                ByteCode::Array(count) => {
                    let typ = Type::None;
                    let mut items: Vec<ObjPointer> = (0..count).map(|_| self.pop()).collect();
                    items.reverse();
                    let obj = Object::Array(Array { typ, items });
                    self.push_obj(obj, &mut prog, frame.func.chunk);
                }
                ByteCode::ArrayGet => {
                    let idx = self.pop_obj(&prog).expect_integer("invalid index")?;
                    let ptr = self.pop();
                    if let Object::Array(arr) = self.get_obj(ptr, &prog) {
                        if !(0 <= idx && (idx as usize) < arr.items.len()) {
                            return Err(VMError::from("index out of bounds"));
                        }
                        self.push(arr.items[idx as usize]);
                    } else {
                        return Err(VMError::from("invalid index target"));
                    }
                }
                ByteCode::ArraySet => {
                    let val = self.pop();
                    let idx = self.pop_obj(&prog).expect_integer("invalid index")?;
                    let ptr = self.pop();
                    if let Object::Array(arr) = self.get_obj_mut(ptr, &mut prog) {
                        if !(0 <= idx && (idx as usize) < arr.items.len()) {
                            return Err(VMError::from("index out of bounds"));
                        }
                        arr.items[idx as usize] = val;
                    } else {
                        return Err(VMError::from("invalid index target"));
                    }
                    self.push(val);
                }
            }
            println!("{:?}", self.stack);
            frame.ip += 1;
        }
        return Ok(());
    }

    #[allow(unused_variables)]
    fn push_func(&mut self, frame: CallFrame) {
        self.call_stack.push(frame);
    }

    fn pop_func(&mut self) -> CallFrame {
        self.call_stack.pop().unwrap()
    }
}

type VMError = String;
pub type LocalCounter = usize;
pub type GlobalCounter = u8;
pub type ArgsCounter = u8;
pub type FuncCounter = u8;
pub type ObjCounter = u8;
pub type ObjPointer = (FuncCounter, ObjCounter);
pub type InstPtr = usize;
