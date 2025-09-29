#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum Op {
    Push,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Print,
}

pub enum Value {
    Integer(i64),
    Float(f64)
}

pub struct Chunk {
    byts: Vec<u8>,
    vals: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            byts: Vec::new(),
            vals: Vec::new(),
        }
    }

    pub fn push_byte(&mut self, b: u8) {
        self.byts.push(b);
    }

    pub fn push_bytes(&mut self, b1: u8, b2: u8) {
        self.push_byte(b2);
        self.push_byte(b1);
    }

    pub fn push_val(&mut self, val: Value) -> Result<u8, &'static str> {
        let index = self.vals.len();
        if index > 0xFF {
            Err("can't define more than 255 constants in one chunks")
        } else {
            self.vals.push(val);
            Ok(index as u8)
        }
    }
}

pub fn exec(prog: Vec<Inst>) -> Result<(), String> {
    let mut stack = Vec::<i64>::new();
    for (_, inst) in prog.iter().enumerate() {
        match *inst {
            Inst::PushInt(num) => {
                stack.push(num);
            },
            Inst::Add => {
                if stack.len() < 2 {
                    return Err(String::from("Add instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y + x);
            },
            Inst::Sub => {
                if stack.len() < 2 {
                    return Err(String::from("Sub instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y - x);
            },
            Inst::Mul => {
                if stack.len() < 2 {
                    return Err(String::from("Mul instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y * x);
            },
            Inst::Div => {
                if stack.len() < 2 {
                    return Err(String::from("Div instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y / x);
            },
            Inst::Print => {
                if stack.len() < 1 {
                    return Err(String::from("Print instruction requires at least 1 operand"));
                }
                println!("{}", stack.pop().unwrap_or_default());
            },
        }
    }
    return Ok(());
}
