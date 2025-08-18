mod vm;
use crate::vm::{
    Inst,
    exec
};

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

struct Compiler {
    start: usize,
    curr: usize,
    src: String,
}

impl Compiler {
    fn new(src: String) -> Compiler {
        return Compiler {
            start: 0,
            curr: 0,
            src,
        };
    }

    fn peek_char(&mut self) -> Option<char> {
        self.src.chars().nth(self.curr)
    }

    fn consume_char(&mut self) {
        self.curr += 1;
    }

    fn consume_number(&mut self) {
        loop {
            let c = self.peek_char();
            if c.is_none() || !c.unwrap().is_digit(10) {
                break;
            }
            self.curr += 1;
        }
    }
    
    fn emit_inst(&mut self) -> Option<Inst> {
        self.start = self.curr;
        if self.peek_char().is_none() {
            return None;
        }
        let c: char = self.peek_char().unwrap();
        if c.is_digit(10) {
            self.consume_number();
            return match self.src[self.start..self.curr].parse::<f64>() {
                Ok(num) => Some(Inst::PUSH(num)),
                Err(why) => panic!("failed parsing number: {}", why),
            };
        }
        self.consume_char();
        match c {
            '+' => Some(Inst::ADD),
            '-' => Some(Inst::SUB),
            '*' => Some(Inst::MUL),
            '/' => Some(Inst::DIV),
            ' ' => self.emit_inst(),
            _ => None,
        }
    }
}

fn get_operator_precedance(src: &Inst) -> i32 {
    match src {
        Inst::DIV => 1,
        Inst::MUL => 1,
        Inst::SUB => 0,
        Inst::ADD => 0,
        _ => -1,
    }
}

fn infix_to_postfix(src: Vec<Inst>) -> Vec<Inst> {
    let mut dest = Vec::<Inst>::new();
    let mut stack = Vec::<Inst>::new();
    for i in 0..src.len() {
        let inst: Inst = *src.get(i).unwrap();
        match inst {
            Inst::PUSH(_) => {
                dest.push(inst);
            },
            _ => {
                let pr = get_operator_precedance(&inst);
                assert_ne!(pr, -1);
                if stack.is_empty() {
                    stack.push(inst);
                    continue;
                }
                while !stack.is_empty() && get_operator_precedance(stack.last().unwrap()) >= pr {
                    dest.push(stack.pop().unwrap());
                }
                stack.push(inst);
            }
        };
    }
    while !stack.is_empty() {
        dest.push(stack.pop().unwrap());
    }
    print_program(&dest);
    return dest;
}

fn print_program(program: &Vec<Inst>) {
    println!("--- PROGRAM_START ---");
    for i in 0..program.len() {
        println!("{}: {:?}", i, program.get(i).unwrap());
    }
    println!("--- PROGRAM_FINISH ---");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage {} <file.raml>", args[0]);
    }

    let mut src = String::new();
    {
        let path = Path::new(&args[1]);
        let display = path.display();

        let mut file = match File::open(path) {
            Err(why) => panic!("failed to open: {}: {}", display, why),
            Ok(file) => file,
        };

        match file.read_to_string(&mut src) {
            Err(why) => panic!("failed to read: {}: {}", display, why),
            Ok(size) => print!("{} ({} bytes) contains:\n{}", display, size, src),
        };
    }

    let mut program = Vec::<Inst>::new();
    let mut compiler = Compiler::new(src);
    loop {
        match compiler.emit_inst() {
            Some(inst) => {
                program.push(inst);
            },
            None => break,
        };
    }

    program = infix_to_postfix(program);
    program.push(Inst::PRINT);

    match exec(&program) {
        Err(msg) => {
            println!("VM_ERROR: {msg}");
        }
        Ok(_) => ()
    }
}
