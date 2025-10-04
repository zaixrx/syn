use std::collections::HashMap;

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum Op {
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil
}

impl Value {
    fn expect_string(self) -> String {
        match self {
            Value::String(x) => x,
            _ => panic!("expected string")
        }
    }

    fn expect_integer(self) -> i64 {
        match self {
            Value::Integer(x) => x,
            _ => panic!("expected integer")
        }
    }

    fn expect_bool(self) -> bool {
        match self {
            Value::Bool(x) => x,
            _ => panic!("expected bool")
        }
    }

    fn get_var_type(&self) -> VarType {
        match self {
            Value::Integer(_) => VarType::Integer,
            Value::Float(_) => VarType::Float,
            Value::Bool(_) => VarType::Bool,
            Value::String(_) => VarType::String,
            Value::Nil => VarType::None,
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
    val: Value
}

type ByteInfo = (usize, usize);
pub struct Chunk {
    values: Vec<Value>,
    bytes: Vec<Op>,
    bytes_info: Vec<ByteInfo>,
    globals: HashMap<String, Var>
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            bytes: Vec::new(),
            bytes_info: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn push_byte(&mut self, b: Op, bi: ByteInfo) {
        self.bytes.push(b);
        self.bytes_info.push(bi);
    }

    pub fn push_bytes(&mut self, b1: Op, bi1: ByteInfo, b2: Op, bi2: ByteInfo) {
        self.push_byte(b1, bi1);
        self.push_byte(b2, bi2);
    }

    pub fn push_value(&mut self, val: Value, bi: ByteInfo) -> Result<u8, &'static str> {
        let index = self.values.len();
        if index > 0xFF {
            Err("can't define more than 255 constants in one chunks")
        } else {
            self.push_byte(Op::Load(index as u8), bi);
            self.values.push(val);
            Ok(index as u8)
        }
    }

    pub fn disassemble(&self) {
        println!("=======");
        for byte in self.bytes.iter().cloned() {
            match byte {
                Op::Load(i) => {
                    println!("Load {} --> {:?}", i, self.values[i as usize]);
                },
                _ => println!("{:?}", byte)
            };
        }
        println!("=======");
    }
}

pub struct VM {
    chunk: Chunk,
    stack: Vec<Value>
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            stack: Vec::new(),
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val)
    }

    pub fn exec(&mut self) -> Result<(), &'static str> {
        for i in 0..self.chunk.bytes.len() {
            let opbyte = self.chunk.bytes[i];
            match opbyte {
                Op::Load(i) => {
                    if i as usize >= self.chunk.values.len() {
                        return Err("constant doesn't exist");
                    }
                    self.push(self.chunk.values[i as usize].clone());
                },
                Op::Add | Op::Sub |
                Op::Mul | Op::Div |
                Op::Less | Op::Greater => {
                    let y = self.pop();
                    let x = self.pop();
                    if let Value::Integer(x) = x && let Value::Integer(y) = y {
                        self.push(match opbyte {
                            Op::Add => Value::Integer(x + y),
                            Op::Sub => Value::Integer(x - y),
                            Op::Mul => Value::Integer(x * y),
                            Op::Div => Value::Integer(x / y),
                            Op::Less => Value::Bool(x < y),
                            Op::Greater => Value::Bool(x > y),
                            _ => unreachable!()
                        });
                    } else if let Value::Float(x) = x && let Value::Float(y) = y {
                        self.push(match opbyte {
                            Op::Add => Value::Float(x + y),
                            Op::Sub => Value::Float(x - y),
                            Op::Mul => Value::Float(x * y),
                            Op::Div => Value::Float(x / y),
                            Op::Less => Value::Bool(x < y),
                            Op::Greater => Value::Bool(x > y),
                            _ => unreachable!()
                        });
                    } else {
                        return Err("operands must both be numbers")
                    }
                },
                Op::Print => {
                    println!("{}", self.pop());
                },
                Op::Neg => {
                    let x = self.pop().expect_integer();
                    self.push(Value::Integer(-x))
                }
                Op::Or | Op::And => {
                    let y = self.pop().expect_bool();
                    let x = self.pop().expect_bool();
                    self.push(match opbyte {
                        Op::Or => Value::Bool(x || y),
                        Op::And => Value::Bool(x && y),
                        _ => unreachable!()
                    });
                },
                Op::Equal => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(Value::Bool(x == y));
                }
                Op::Not => {
                    let b = self.pop().expect_bool();
                    self.push(Value::Bool(!b));
                },
                Op::Pop => {
                    self.pop();
                },
                Op::GDef => {
                    let val = self.pop();
                    let name = self.pop().expect_string();
                    self.chunk.globals.insert(name.into(), Var {
                        typ: val.get_var_type(),
                        val
                    });
                },
                Op::GGet => {
                    let name = self.pop().expect_string();
                    match self.chunk.globals.get(&name) {
                        Some(var) => {
                            if var.typ == VarType::None {
                                return Err("can't use non intialized variable");
                            }
                            self.push(var.val.clone());
                        },
                        None => return Err("undefined variable")
                    };
                }
                Op::GSet => {
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
                Op::LDef => (), // yeah
                Op::LGet(offset) => self.push(self.stack[offset as usize].clone()),
                Op::LSet(offset) => {
                    let val = self.pop();
                    if val.get_var_type() != self.stack[offset as usize].get_var_type() {
                        return Err("mismatched types");
                    }
                    self.stack[offset as usize] = val;
                }
            }
        }
        return Ok(());
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            Value::Integer(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
        }
    }
}
