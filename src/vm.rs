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
    Float(f64)
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

// pub fn exec(chunk: Chunk) -> Result<(), String> {
//     let mut stack = Vec::<i64>::new();
//     for (_, inst) in prog.iter().enumerate() {
//         match *inst {
//             Inst::PushInt(num) => {
//                 stack.push(num);
//             },
//             Inst::Add => {
//                 if stack.len() < 2 {
//                     return Err(String::from("Add instruction requires at least 2 operands"));
//                 }
//                 let x = stack.pop().unwrap_or_default();
//                 let y = stack.pop().unwrap_or_default();
//                 stack.push(y + x);
//             },
//             Inst::Sub => {
//                 if stack.len() < 2 {
//                     return Err(String::from("Sub instruction requires at least 2 operands"));
//                 }
//                 let x = stack.pop().unwrap_or_default();
//                 let y = stack.pop().unwrap_or_default();
//                 stack.push(y - x);
//             },
//             Inst::Mul => {
//                 if stack.len() < 2 {
//                     return Err(String::from("Mul instruction requires at least 2 operands"));
//                 }
//                 let x = stack.pop().unwrap_or_default();
//                 let y = stack.pop().unwrap_or_default();
//                 stack.push(y * x);
//             },
//             Inst::Div => {
//                 if stack.len() < 2 {
//                     return Err(String::from("Div instruction requires at least 2 operands"));
//                 }
//                 let x = stack.pop().unwrap_or_default();
//                 let y = stack.pop().unwrap_or_default();
//                 stack.push(y / x);
//             },
//             Inst::Print => {
//                 if stack.len() < 1 {
//                     return Err(String::from("Print instruction requires at least 1 operand"));
//                 }
//                 println!("{}", stack.pop().unwrap_or_default());
//             },
//         }
//     }
//     return Ok(());
// }
