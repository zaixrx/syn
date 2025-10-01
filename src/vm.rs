#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum Op {
    Load(u8),

    Add,
    Sub,
    Mul,
    Div,
    Neg,

    Or,
    And,
    Less,
    Great,
    LessEq,
    GreatEq,
    Cmp(bool),
    Not,

    Print,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    Nil
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            Value::Integer(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub struct Chunk {
    bytes: Vec<Op>,
    values: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn push_byte(&mut self, b: Op) {
        self.bytes.push(b);
    }

    pub fn push_val(&mut self, val: Value) -> Result<u8, &'static str> {
        let index = self.values.len();
        if index > 0xFF {
            Err("can't define more than 255 constants in one chunks")
        } else {
            self.push_byte(Op::Load(index as u8));
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
        for &opbyte in self.chunk.bytes.iter() {
            match opbyte {
                Op::Load(i) => {
                    if i as usize >= self.chunk.values.len() {
                        return Err("constant doesn't exist");
                    }
                    self.stack.push(self.chunk.values[i as usize]);
                },
                Op::Add | Op::Sub |
                Op::Mul | Op::Div |
                Op::Less | Op::Great |
                Op::LessEq | Op::GreatEq => {
                    if self.stack.len() < 2 {
                        return Err("instruction requires at least 2 operands");
                    }
                    let y = match self.stack.pop().unwrap() {
                        Value::Integer(a) => a,
                        _ => return Err("operands must both be integers")
                    };
                    let x = match self.stack.pop().unwrap() {
                        Value::Integer(a) => a,
                        _ => return Err("operands must both be integers")
                    };
                    self.stack.push(match opbyte {
                        Op::Add => Value::Integer(x + y),
                        Op::Sub => Value::Integer(x - y),
                        Op::Mul => Value::Integer(x * y),
                        Op::Div => Value::Integer(x / y),
                        Op::Less => Value::Bool(x < y),
                        Op::Great => Value::Bool(x > y),
                        Op::LessEq => Value::Bool(x <= y),
                        Op::GreatEq => Value::Bool(x >= y),
                        _ => unreachable!()
                    });
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
                Op::Cmp(target) => {
                    if self.stack.len() < 2 {
                        return Err("instruction requires at least 2 operands");
                    }
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool((x == y) == target));
                }
                Op::Not => {
                    match self.stack.pop() {
                        Some(Value::Bool(x)) => self.stack.push(Value::Bool(!x)),
                        _ => return Err("unary '!' requires at least a boolean")
                    }
                },
            }
        }
        return Ok(());
    }
}
