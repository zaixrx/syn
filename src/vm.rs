use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Func {
    pub arity: usize,
    pub name: String,
    pub chunk: Chunk,
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
    pub fn new(name: String) -> Self {
        Self {
            name,
            arity: 0,
            chunk: Chunk::new(),
            retype: VarType::None,
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
}

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

pub struct VM {
    chunk: Chunk,
    stack: Vec<Constant>,
    globals: HashMap<String, Var>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    fn pop(&mut self) -> Constant {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> &Constant {
        self.stack.last().unwrap()
    }

    fn push(&mut self, c: Constant) {
        self.stack.push(c)
    }

    pub fn exec(&mut self) -> Result<(), &'static str> {
        let mut ip = 0;
        while ip < self.chunk.count() {
            let byte = self.chunk.code[ip];
            // self.chunk.disassemble_one(ip);
            match byte {
                ByteCode::Push(i) => {
                    if i as usize >= self.chunk.consts.len() {
                        return Err("constant doesn't exist");
                    }
                    self.push(self.chunk.consts[i as usize].clone());
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
                ByteCode::Jump(dest) => ip = dest - 1, // to make room for iterating
                ByteCode::JumpIfFalse(dest) => {
                    if !self.pop().to_bool() {
                        ip = dest - 1;
                    }
                },
            }
            ip += 1;
        }
        return Ok(());
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
