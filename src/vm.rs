use std::rc::Rc;
use std::collections::HashMap;

pub struct VM {
    stack: Vec<Constant>,
    call_stack: Vec<CallFrame>,
    globals: HashMap<String, Var>,
}

// TODO: work with a reference
pub struct CallFrame {
    ret: IdxPtr,
    func: *mut Func,
    stack: *const Constant,
}

type IdxPtr = usize;

#[derive(Debug, Clone)]
pub struct Func {
    pub arity: usize,
    pub name: String,
    pub chunk: Rc<Chunk>,
    pub retype: VarType,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name &&
        self.arity == other.arity &&
        self.retype == other.retype
    }
}

impl Func {
    pub fn new(name: String, arity: usize, retype: VarType, chunk: Chunk) -> Self {
        Self {
            name,
            arity,
            retype,
            chunk: Rc::new(chunk),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, PartialEq, Debug)]
pub enum Constant {
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Function(Func),
    Nil
}

impl Constant {
    fn to_bool(self) -> bool {
        match self {
            Constant::Bool(false) | Constant::Nil => false,
            _ => true
        }
    }

    fn expect_string(self) -> String {
        match self {
            Constant::String(x) => x,
            _ => panic!("expected string")
        }
    }

    fn expect_integer(self) -> i32 {
        match self {
            Constant::Integer(x) => x,
            _ => panic!("expected integer")
        }
    }

    fn expect_bool(self) -> bool {
        match self {
            Constant::Bool(x) => x,
            _ => panic!("expected bool")
        }
    }

    fn get_var_type(&self) -> VarType {
        match self {
            Constant::Integer(_) => VarType::Integer,
            Constant::Float(_) => VarType::Float,
            Constant::Bool(_) => VarType::Bool,
            Constant::String(_) => VarType::String,
            Constant::Function(_) => VarType::Function,
            Constant::Nil => VarType::None,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum VarType {
    Integer,
    Float,
    Bool,
    String,
    Function,
    None 
}

#[derive(Debug)]
struct Var {
    typ: VarType,
    val: Constant
}

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum ByteCode {
    Push(u8),
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

    Print,

    GDef,
    GGet,
    GSet,
    LDef,
    LGet(u8),
    LSet(u8),

    Jump(usize),
    JumpIfFalse(usize),
    Call(u8),

    Ret
}

type Program = Chunk;

#[derive(Clone, Debug)]
pub struct Chunk {
    consts: Vec<Constant>,
    code: Vec<ByteCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
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

    pub fn load_const(&mut self, c: Constant) -> Result<u8, &'static str> {
        let index = self.consts.len();
        if index > 0xFF {
            Err("can't define more than 255 constants in one chunks")
        } else {
            self.push(ByteCode::Push(index as u8));
            self.consts.push(c);
            Ok(index as u8)
        }
    }

    pub fn disassemble_one(&self, idx: usize) {
        print!("{:?} :: ", idx);
        match self.code[idx] {
            ByteCode::Push(i) => {
                println!("Push {} --> {:?}", i, self.consts[i as usize]);
            },
            byte => println!("{:?}", byte)
        };
    }

    pub fn disassemble(&self) {
        println!("=======");
        for i in 0..self.code.len() {
            self.disassemble_one(i);
        }
        println!("=======");
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            call_stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    #[inline]
    fn pop(&mut self) -> Constant {
        self.stack.pop().unwrap()
    }

    #[inline]
    fn peek(&self) -> &Constant {
        self.stack.last().unwrap()
    }

    fn push(&mut self, c: Constant) {
        self.stack.push(c)
    }

    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn exec(mut self, prog: Chunk) -> Result<(), &'static str> {
        let mut ip: IdxPtr = 0;
        let mut chunk = Rc::new(prog);
        while ip < chunk.count() {
            // self.chunk.disassemble_one(ip);
            let byte = chunk.code[ip]; 
            match byte {
                ByteCode::Push(i) => {
                    if i as usize >= chunk.consts.len() {
                        return Err("constant doesn't exist");
                    }
                    self.push(chunk.consts[i as usize].clone());
                },
                ByteCode::Add | ByteCode::Sub |
                ByteCode::Mul | ByteCode::Div |
                ByteCode::Less | ByteCode::Greater => {
                    let y = self.pop();
                    let x = self.pop();
                    if let Constant::Integer(x) = x && let Constant::Integer(y) = y {
                        self.push(match byte {
                            ByteCode::Add => Constant::Integer(x + y),
                            ByteCode::Sub => Constant::Integer(x - y),
                            ByteCode::Mul => Constant::Integer(x * y),
                            ByteCode::Div => Constant::Integer(x / y),
                            ByteCode::Less => Constant::Bool(x < y),
                            ByteCode::Greater => Constant::Bool(x > y),
                            _ => unreachable!()
                        });
                    } else if let Constant::Float(x) = x && let Constant::Float(y) = y {
                        self.push(match byte {
                            ByteCode::Add => Constant::Float(x + y),
                            ByteCode::Sub => Constant::Float(x - y),
                            ByteCode::Mul => Constant::Float(x * y),
                            ByteCode::Div => Constant::Float(x / y),
                            ByteCode::Less => Constant::Bool(x < y),
                            ByteCode::Greater => Constant::Bool(x > y),
                            _ => unreachable!()
                        });
                    } else {
                        return Err("operands must both be numbers")
                    }
                },
                ByteCode::Print => {
                    println!("{}", self.pop());
                },
                ByteCode::Neg => {
                    let x = self.pop().expect_integer();
                    self.push(Constant::Integer(-x))
                }
                ByteCode::Or | ByteCode::And => {
                    let y = self.pop().expect_bool();
                    let x = self.pop().expect_bool();
                    self.push(match byte {
                        ByteCode::Or => Constant::Bool(x || y),
                        ByteCode::And => Constant::Bool(x && y),
                        _ => unreachable!()
                    });
                },
                ByteCode::Equal => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(Constant::Bool(x == y));
                }
                ByteCode::Not => {
                    let b = self.pop().expect_bool();
                    self.push(Constant::Bool(!b));
                },
                ByteCode::Pop => {
                    self.pop();
                },
                ByteCode::GDef => {
                    let val = self.pop();
                    let name = self.pop().expect_string();
                    self.globals.insert(name.into(), Var {
                        typ: val.get_var_type(),
                        val
                    });
                },
                ByteCode::GGet => {
                    let name = self.pop().expect_string();
                    match self.globals.get(&name) {
                        Some(var) => {
                            if var.typ == VarType::None {
                                return Err("can't use unintialized variable");
                            }
                            self.push(var.val.clone());
                        },
                        None => return Err("undefined variable")
                    };
                }
                ByteCode::GSet => {
                    let val = self.pop();
                    let name = self.pop().expect_string();
                    match self.globals.get(&name) {
                        Some(var) => {
                            // TODO: typechecking on every access is tedious
                            if val.get_var_type() != var.typ {
                                return Err("mismatched types");
                            }
                            self.globals.insert(name.into(), Var {
                                typ: val.get_var_type(),
                                val: val.clone()
                            });
                        },
                        None => return Err("undefined variable"),
                    };
                    self.push(val);
                },
                ByteCode::LDef => (), // yeah
                ByteCode::LGet(offset) => self.push(self.stack[offset as usize].clone()),
                ByteCode::LSet(offset) => {
                    let val = self.peek();
                    if val.get_var_type() != self.stack[offset as usize].get_var_type() {
                        return Err("mismatched types");
                    }
                    self.stack[offset as usize] = val.clone();
                },
                ByteCode::Jump(dest) => {
                    ip = dest - 1; // to make room for iterating
                }, 
                ByteCode::JumpIfFalse(dest) => {
                    if !self.pop().to_bool() {
                        ip = dest - 1;
                    }
                },
                ByteCode::Call(args_c) => {
                    let name = self.pop().expect_string();
                    if let Some(var) = self.globals.get_mut(&name) {
                         if let Constant::Function(ref mut func) = var.val {
                             if func.arity != args_c as usize {
                                 return Err("wrong argument count");
                             }
                             chunk = Rc::clone(&func.chunk);
                             let func_ptr = func as *mut Func;
                             self.push_func(func_ptr, ip + 1, self.get_stack_ptr());
                             ip = 0;
                             continue;
                         } else {
                             return Err("can only call a function");
                         }
                    } else {
                         return Err("undefined variable");
                    }
                },
                ByteCode::Ret => {
                    let frame = self.pop_func();
                    chunk = Rc::clone(&(*frame.func).chunk);
                    ip = frame.ret;
                }
            }
            ip += 1;
        }
        return Ok(());
    }

    fn get_stack_ptr(&self) -> *const Constant {
        let mut offset = self.stack.len();
        if offset > 0 {
            offset -= 1;
        }
        unsafe {
            self.stack.as_ptr().add(offset)
        }
    }

    #[allow(unused_variables)]
    fn push_func(&mut self, func: *mut Func, ret: IdxPtr, stack: *const Constant) {
        let call_frame = CallFrame { func, stack, ret };
        self.call_stack.push(call_frame);
    }

    fn pop_func(&mut self) -> CallFrame {
        self.call_stack.pop().unwrap()
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            Constant::Integer(val) => write!(f, "{}", val),
            Constant::Float(val) => write!(f, "{}", val),
            Constant::Bool(val) => write!(f, "{}", val),
            Constant::String(val) => write!(f, "{}", val),
            Constant::Function(val) => write!(f, "Func<{}>", val.name),
            Constant::Nil => write!(f, "nil"),
        }
    }
}
