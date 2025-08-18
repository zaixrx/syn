#[derive(Debug, Copy, Clone)]
pub enum Inst {
    PUSH(f64),
    ADD,
    SUB,
    MUL,
    DIV,
    PRINT,
}

pub fn exec(prog: &Vec<Inst>) -> Result<(), String> {
    let mut stack = Vec::<f64>::new();
    for (_, inst) in prog.iter().enumerate() {
        match *inst {
            Inst::PUSH(num) => {
                stack.push(num);
            },
            Inst::ADD => {
                if stack.len() < 2 {
                    return Err(String::from("ADD instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y + x);
            },
            Inst::SUB => {
                if stack.len() < 2 {
                    return Err(String::from("SUB instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y - x);
            },
            Inst::MUL => {
                if stack.len() < 2 {
                    return Err(String::from("MUL instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y * x);
            },
            Inst::DIV => {
                if stack.len() < 2 {
                    return Err(String::from("DIV instruction requires at least 2 operands"));
                }
                let x = stack.pop().unwrap_or_default();
                let y = stack.pop().unwrap_or_default();
                stack.push(y / x);
            },
            Inst::PRINT => {
                if stack.len() < 1 {
                    return Err(String::from("PRINT instruction requires at least 1 operand"));
                }
                println!("{}", stack.pop().unwrap_or_default());
            },
        }
    }
    return Ok(());
}
