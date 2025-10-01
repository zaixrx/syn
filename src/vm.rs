#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum Op {
    Load(u8),
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Print,
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            Value::Integer(a) => write!(f, "{}", a),
            Value::Float(a) => write!(f, "{}", a),
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
        for byte in self.bytes.iter().cloned() {
            println!("{:?}", byte);
        }
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
                Op::Mul | Op::Div => {
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
                    self.stack.push(Value::Integer(match opbyte {
                        Op::Add => x + y,
                        Op::Sub => x - y,
                        Op::Mul => x * y,
                        Op::Div => x / y,
                        _ => unreachable!()
                    }));
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
            }
        }
        return Ok(());
    }
}
