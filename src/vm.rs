use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(String),
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
            Constant::Nil => VarType::None,
        }
    }
}

#[derive(PartialEq)]
enum VarType {
    Integer,
    Float,
    Bool,
    String,
    None 
}

struct Var {
    typ: VarType,
    val: Constant
}

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum ByteCode {
    Load(u8),

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
    Pop,

    GDef,
    GGet,
    GSet,
    LDef,
    LGet(u8),
    LSet(u8),

    Jump(u16),
    JumpIfFalse(u16),
    Loop(u16),
}

pub struct Chunk {
    consts: Vec<Constant>,
    globals: HashMap<String, Var>,
    bytes: Vec<ByteCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            globals: HashMap::new(),
            bytes: Vec::new(),
        }
    }

    pub fn get_count(&self) -> usize {
        self.bytes.len()
    }

    pub fn push_byte(&mut self, b: ByteCode) {
        self.bytes.push(b);
    }

    pub fn set_byte(&mut self, offset: usize, b: ByteCode) {
        self.bytes[offset] = b;
    }
    
    pub fn push_bytes(&mut self, b1: ByteCode, b2: ByteCode) {
        self.push_byte(b1);
        self.push_byte(b2);
    }

    pub fn load_const(&mut self, c: Constant) -> Result<u8, &'static str> {
        let index = self.consts.len();
        if index > 0xFF {
            Err("can't define more than 255 constants in one chunks")
        } else {
            self.push_byte(ByteCode::Load(index as u8));
            self.consts.push(c);
            Ok(index as u8)
        }
    }

    pub fn disassemble(&self) {
        println!("=======");
        for byte in self.bytes.iter().cloned() {
            match byte {
                ByteCode::Load(i) => {
                    println!("Load {} --> {:?}", i, self.consts[i as usize]);
                },
                _ => println!("{:?}", byte)
            };
        }
        println!("=======");
    }
}

pub struct VM {
    chunk: Chunk,
    stack: Vec<Constant>
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            stack: Vec::new(),
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
        while ip < self.chunk.get_count() {
            let byte = self.chunk.bytes[ip];
            match byte {
                ByteCode::Load(i) => {
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
                    self.chunk.globals.insert(name.into(), Var {
                        typ: val.get_var_type(),
                        val
                    });
                },
                ByteCode::GGet => {
                    let name = self.pop().expect_string();
                    match self.chunk.globals.get(&name) {
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
                    match self.chunk.globals.get(&name) {
                        Some(var) => {
                            // TODO: typechecking on every access is tedious
                            if val.get_var_type() != var.typ {
                                return Err("mismatched types");
                            }
                            self.chunk.globals.insert(name.into(), Var {
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
                ByteCode::Jump(jump) => {
                    ip += jump as usize;
                },
                ByteCode::JumpIfFalse(jump) => {
                    if !self.pop().to_bool() {
                        ip += jump as usize;
                    }
                },
                ByteCode::Loop(jump) => {
                    ip -= jump as usize;
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
            Constant::Nil => write!(f, "nil"),
        }
    }
}
