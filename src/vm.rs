#[derive(Debug, Copy, Clone)]
pub enum Inst {
    PushInt(i64),
    Add,
    Sub,
    Mul,
    Div,
    Print,
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
