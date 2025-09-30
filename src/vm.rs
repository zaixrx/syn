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

    pub fn push_bytes(&mut self, b1: Op, b2: Op) {
        self.push_byte(b2);
        self.push_byte(b1);
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

    fn binary<U>(&mut self, fun: fn(Value, Value) -> U) -> Result<U, String> { // TODO: custom error
        if self.stack.len() < 2 {
            return Err(
                String::from("Add instruction requires at least 2 operands")
            );
        }
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        Ok(fun(a, b))
    }

    pub fn exec(&mut self) -> Result<(), &'static str> {
        for opbyte in self.chunk.bytes.iter().cloned() {
            match opbyte {
                Op::Load(i) => {
                    if i as usize >= self.chunk.values.len() {
                        return Err("constant doesn't exist");
                    }
                    self.stack.push(self.chunk.values[i as usize]);
                },
                Op::Add => {
                    self.binary(|x, y| -> Result<i64, &'static str> {
                        if let Value::Integer(x) = x && let Value::Integer(y) = y {
                            Ok(x + y)
                        } else {
                            Err("'+' operands must both be integers")
                        }
                    });
                },
                Op::Sub => {
                    self.binary(|x, y| -> Result<i64, &'static str> {
                        if let Value::Integer(x) = x && let Value::Integer(y) = y {
                            Ok(x - y)
                        } else {
                            Err("'-' operands must both be integers")
                        }
                    });
                },
                Op::Mul => {
                    self.binary(|x, y| -> Result<i64, &'static str> {
                        if let Value::Integer(x) = x && let Value::Integer(y) = y {
                            Ok(x * y)
                        } else {
                            Err("'*' operands must both be integers")
                        }
                    });
                },
                Op::Div => {
                    self.binary(|x, y| -> Result<i64, &'static str> {
                        if let Value::Integer(x) = x && let Value::Integer(y) = y {
                            Ok(x / y)
                        } else {
                            Err("'/' operands must both be integers")
                        }
                    });
                },
                Op::Print => {
                    match self.stack.pop() {
                        Some(x) => println!("{:?}", x),
                        None => return Err("printing requires at least a value")
                    }
                },
                Op::Neg => {
                    match self.stack.pop() {
                        Some(Value::Integer(x)) => self.stack.push(Value::Integer(-x)),
                        None => return Err("unary '-' requires at least a numerical value")
                    }
                }
            }
        }
        return Ok(());
    }
}
