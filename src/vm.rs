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

    GDef(u8),
    GGet(u8),
    GSet(u8),
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
    fn must_expect_string(&self) -> &String {
        match self.expect_string() {
            Ok(s) => s,
            Err(e) => panic!("{}", e)
        }
    }

    fn expect_string(&self) -> Result<&String, &'static str> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err("expected string")
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
            println!("{:?}", byte);
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

    pub fn exec(&mut self) -> Result<(), &'static str> {
        for opbyte in self.chunk.bytes.iter().cloned() {
            match opbyte {
                Op::Load(i) => {
                    if i as usize >= self.chunk.values.len() {
                        return Err("constant doesn't exist");
                    }
                    self.stack.push(self.chunk.values[i as usize].clone());
                },
                Op::Add | Op::Sub |
                Op::Mul | Op::Div |
                Op::Less | Op::Greater => {
                    if self.stack.len() < 2 {
                        return Err("instruction requires at least 2 operands");
                    }
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();
                    if let Value::Integer(x) = x && let Value::Integer(y) = y {
                        self.stack.push(match opbyte {
                            Op::Add => Value::Integer(x + y),
                            Op::Sub => Value::Integer(x - y),
                            Op::Mul => Value::Integer(x * y),
                            Op::Div => Value::Integer(x / y),
                            Op::Less => Value::Bool(x < y),
                            Op::Greater => Value::Bool(x > y),
                            _ => unreachable!()
                        });
                    } else if let Value::Float(x) = x && let Value::Float(y) = y {
                        self.stack.push(match opbyte {
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
                    match self.stack.pop() {
                        Some(x) => println!("{x}"),
                        None => return Err("printing requires at least a value")
                    }
                },
                Op::Neg => {
                    match self.stack.pop() {
                        Some(Value::Integer(x)) => self.stack.push(Value::Integer(-x)),
                        _ => return Err("unary '-' requires at least an integer")
                    }
                }
                Op::Or | Op::And => {
                    if self.stack.len() < 2 {
                        return Err("instruction requires at least 2 operands");
                    }
                    let y = match self.stack.pop().unwrap() {
                        Value::Bool(a) => a,
                        _ => return Err("left operaand must both be a boolean")
                    };
                    let x = match self.stack.pop().unwrap() {
                        Value::Bool(a) => a,
                        _ => return Err("right operand must both be a boolean")
                    };
                    self.stack.push(match opbyte {
                        Op::Or => Value::Bool(x || y),
                        Op::And => Value::Bool(x && y),
                        _ => unreachable!()
                    });
                },
                Op::Equal => {
                    if self.stack.len() < 2 {
                        return Err("instruction requires at least 2 operands");
                    }
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(x == y));
                }
                Op::Not => {
                    match self.stack.pop() {
                        Some(Value::Bool(x)) => self.stack.push(Value::Bool(!x)),
                        _ => return Err("unary '!' requires at least a boolean")
                    }
                },
                Op::Pop => {
                    self.stack.pop();
                },
                Op::GDef(idx) => {
                    let name = self.chunk.values[idx as usize].must_expect_string();
                    let val = self.stack.pop().unwrap();
                    self.chunk.globals.insert(name.into(), Var {
                        typ: val.get_var_type(),
                        val
                    });
                },
                Op::GGet(idx) => {
                    let name = self.chunk.values[idx as usize].must_expect_string();
                    match self.chunk.globals.get(name) {
                        Some(var) => {
                            if var.typ == VarType::None {
                                return Err("can't use non intialized variable");
                            }
                            self.stack.push(var.val.clone());
                        },
                        None => return Err("undefined variable")
                    };
                }
                Op::GSet(idx) => {
                    let name = self.chunk.values[idx as usize].must_expect_string();
                    let val = self.stack.pop().unwrap();
                    match self.chunk.globals.get(name) {
                        Some(var) => {
                            // TODO: typechecking on every access is tedious
                            if val.get_var_type() != var.typ {
                                return Err("mismatched types");
                            }
                            self.chunk.globals.insert(name.into(), Var {
                                typ: val.get_var_type(),
                                val
                            });
                        },
                        None => return Err("undefined variable"),
                    };
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