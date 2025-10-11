type VMError = String;
pub type ChunksCount = u8;
pub type ConstsCount = u8;
pub type ArgsCount = u8;
pub type LocalsCount = u8;
pub type GlobalsCount = u8;
pub type ChunkSize = usize;

pub struct VM {
    stack: Vec<Constant>,
    globals: Vec<Constant>,
    call_stack: Vec<CallFrame>,
}

pub struct Program {
    pub chunks: Vec<Chunk>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            chunks: vec![Chunk::new()],
        }
    }

    pub fn get_top_level_chunk(&mut self) -> &mut Chunk {
        &mut self.chunks[0]
    }

    pub fn push(&mut self, c: Chunk) -> Result<ChunksCount, &'static str> {
        let idx = self.chunks.len() as ChunksCount;
        if idx == ChunksCount::MAX {
            Err("exceeded the number of possible functions in a program")
        } else {
            self.chunks.push(c);
            Ok(idx)
        }
    }
}

#[derive(Clone)]
pub struct CallFrame {
    func: Func,
    ip: ChunkSize,
    stack_offset: usize,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub arity: usize,
    pub name: String,
    pub retype: Type,
    pub chunk: ChunksCount,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity && self.retype == other.retype
    }
}

impl Func {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: 0,
            retype: Type::None,
            name: String::new(),
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
    Nil,
}

impl Constant {
    fn to_bool(self) -> bool {
        match self {
            Constant::Bool(false) | Constant::Nil => false,
            _ => true,
        }
    }

    fn expect_integer(self) -> i32 {
        match self {
            Constant::Integer(x) => x,
            _ => panic!("expected integer"),
        }
    }

    fn expect_bool(self) -> bool {
        match self {
            Constant::Bool(x) => x,
            _ => panic!("expected bool"),
        }
    }

    fn expect_function(self, msg: &'static str) -> Result<Func, &'static str> {
        match self {
            Constant::Function(func) => Ok(func),
            _ => Err(msg),
        }
    }

    fn get_var_type(&self) -> Type {
        match self {
            Constant::Integer(_) => Type::Integer,
            Constant::Float(_) => Type::Float,
            Constant::Bool(_) => Type::Bool,
            Constant::String(_) => Type::String,
            Constant::Function(_) => Type::Function,
            Constant::Nil => Type::None,
        }
    }

    fn is_of_type(&self, other: &Constant) -> bool {
        let t1 = self.get_var_type();
        let t2 = other.get_var_type();
        t1 == t2 || t1 == Type::None || t2 == Type::None
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Integer,
    Float,
    Bool,
    String,
    Function,
    None,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone)]
pub enum ByteCode {
    Push(ConstsCount),
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

    Print(ArgsCount),

    GDef,
    GGet(GlobalsCount),
    GSet(GlobalsCount),
    LDef,
    LGet(LocalsCount),
    LSet(LocalsCount),

    Jump(ChunkSize),
    JumpIfFalse(ChunkSize),
    Call(ArgsCount),

    Ret,
}

#[derive(Clone, Debug)]
pub struct Chunk {
    code: Vec<ByteCode>,
    consts: Vec<Constant>,
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
        let index = self.consts.len() as ConstsCount;
        if index > ConstsCount::MAX {
            Err("can't define more constants in chunk")
        } else {
            self.push(ByteCode::Push(index));
            self.consts.push(c);
            Ok(index)
        }
    }

    #[allow(unused)]
    pub fn disassemble_one(&self, idx: usize) {
        print!("{:?} :: ", idx);
        match self.code[idx] {
            ByteCode::Push(i) => {
                println!("Push {} --> {:?}", i, self.consts[i as usize]);
            }
            byte => println!("{:?}", byte),
        };
    }

    #[allow(unused)]
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
            globals: Vec::new(),
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

    fn must_op<T>(&self, r: Option<T>, msg: &'static str) -> Result<T, VMError> {
        match r {
            Some(v) => Ok(v),
            None => Err(VMError::from(msg)),
        }
    }

    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn exec(mut self, prog: Program) -> Result<(), VMError> {
        let mut frame = CallFrame {
            ip: 0,
            stack_offset: 0,
            func: Func::new(),
        };
        while frame.ip < prog.chunks[frame.func.chunk as usize].count() {
            // chunk.disassemble_one(frame.ip);
            let chunk = &prog.chunks[frame.func.chunk as usize];
            let byte = chunk.code[frame.ip];
            match byte {
                ByteCode::Push(i) => {
                    if i as usize >= chunk.consts.len() {
                        return Err(format!("constant doesn't exist"));
                    }
                    self.push(chunk.consts[i as usize].clone());
                }
                ByteCode::Add
                | ByteCode::Sub
                | ByteCode::Mul
                | ByteCode::Div
                | ByteCode::Less
                | ByteCode::Greater => {
                    let y = self.pop();
                    let x = self.pop();
                    if let Constant::Integer(x) = x
                        && let Constant::Integer(y) = y
                    {
                        self.push(match byte {
                            ByteCode::Add => Constant::Integer(
                                self.must_op(x.checked_add(y), "addition overflow")?,
                            ),
                            ByteCode::Sub => Constant::Integer(
                                self.must_op(x.checked_sub(y), "subtraction overflow")?,
                            ),
                            ByteCode::Mul => Constant::Integer(
                                self.must_op(x.checked_mul(y), "multiplication overflow")?,
                            ),
                            ByteCode::Div => Constant::Integer(
                                self.must_op(x.checked_div(y), "division overflow")?,
                            ),
                            ByteCode::Less => Constant::Bool(x < y),
                            ByteCode::Greater => Constant::Bool(x > y),
                            _ => unreachable!(),
                        });
                    } else if let Constant::Float(x) = x
                        && let Constant::Float(y) = y
                    {
                        self.push(match byte {
                            ByteCode::Add => Constant::Float({
                                let r = x + y;
                                self.must_op(r.is_finite().then_some(r), "addition overflow")?
                            }),
                            ByteCode::Sub => Constant::Float({
                                let r = x - y;
                                self.must_op(r.is_finite().then_some(r), "subtraction overflow")?
                            }),
                            ByteCode::Mul => Constant::Float({
                                let r = x * y;
                                self.must_op(r.is_finite().then_some(r), "multiplication overflow")?
                            }),
                            ByteCode::Div => Constant::Float({
                                let r = x / y;
                                self.must_op(r.is_finite().then_some(r), "division overflow")?
                            }),
                            ByteCode::Less => Constant::Bool(x < y),
                            ByteCode::Greater => Constant::Bool(x > y),
                            _ => unreachable!(),
                        });
                    } else if let Constant::String(x) = x
                        && let Constant::String(y) = y
                    {
                        match byte {
                            ByteCode::Add => {
                                self.push(Constant::String(String::from(x + y.as_str())))
                            }
                            _ => return Err(format!("you can only concatenate strings with '+'")),
                        }
                    } else {
                        return Err(format!("invalid operands for {:?}", byte));
                    }
                }
                ByteCode::Print(count) => {
                    // TODO: utterly slow
                    let mut buffer = String::new();
                    for _ in 0..count {
                        buffer = format!("{} {}", self.pop(), buffer);
                    }
                    println!("{}", buffer);
                }
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
                        _ => unreachable!(),
                    });
                }
                ByteCode::Equal => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(Constant::Bool(x == y));
                }
                ByteCode::Not => {
                    let b = self.pop().expect_bool();
                    self.push(Constant::Bool(!b));
                }
                ByteCode::Pop => {
                    self.pop();
                }
                ByteCode::GDef => {
                    let val = self.pop();
                    self.globals.push(val);
                }
                ByteCode::GGet(idx) => {
                    self.push(self.globals[idx as usize].clone());
                }
                ByteCode::GSet(idx) => {
                    let val = self.peek().clone();
                    // TODO: move typechecking to compile time
                    if !self.globals[idx as usize].is_of_type(&val) {
                        return Err(format!(
                            "mismatched types ({:?} != {:?})",
                            val.get_var_type(),
                            self.stack[idx as usize].get_var_type()
                        ));
                    }
                    self.globals[idx as usize] = val;
                }
                ByteCode::LDef => (), // yeah
                ByteCode::LGet(offset) => {
                    self.push(self.stack[frame.stack_offset + offset as usize].clone())
                }
                ByteCode::LSet(offset) => {
                    let val = self.peek();
                    if !&self.stack[frame.stack_offset + offset as usize].is_of_type(val) {
                        return Err(format!(
                            "mismatched types ({:?} != {:?})",
                            val.get_var_type(),
                            self.stack[frame.stack_offset + offset as usize].get_var_type()
                        ));
                    }
                    self.stack[frame.stack_offset + offset as usize] = val.clone();
                }
                ByteCode::Jump(dest) => {
                    frame.ip = dest - 1; // to make room for iterating
                }
                ByteCode::JumpIfFalse(dest) => {
                    if !self.pop().to_bool() {
                        frame.ip = dest - 1;
                    }
                }
                ByteCode::Call(args_c) => {
                    // TODO: move additional check to typechecker at compile-time
                    let args = (0..args_c).map(|_| self.pop()).collect::<Vec<Constant>>();
                    let func = self.pop().expect_function("invalid call target")?;
                    if func.arity != args_c as usize {
                        return Err(format!("expected {} args got {}", func.arity, args_c));
                    }
                    self.push_func(frame.clone());
                    frame = CallFrame {
                        func,
                        ip: 0,
                        stack_offset: self.stack.len(),
                    };
                    for arg in args {
                        self.push(arg);
                    }
                    continue;
                }
                ByteCode::Ret => {
                    let val = self.pop();
                    while self.stack.len() > frame.stack_offset {
                        self.pop();
                    }
                    frame = self.pop_func();
                    self.push(val);
                }
            }
            frame.ip += 1;
        }
        return Ok(());
    }

    #[allow(unused_variables)]
    fn push_func(&mut self, frame: CallFrame) {
        self.call_stack.push(frame);
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
