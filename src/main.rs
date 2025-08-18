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

enum Token {
    Num(f64),
    Plus,
    Minus,
    Star,
    Slash,
    LeftParen,
    RightParent,
}

impl Token {
    fn get_precedance(&self) -> i32 {
        match self {
            Token::Plus => 0,
            Token::Minus => 0,
            Token::Star => 1,
            Token::Slash => 1,
            _ => -1,
        }
    }
    fn get_instruction(&self) -> Option<Inst> {
        match self {
            Token::Num(num) => Some(Inst::PUSH(*num)),
            Token::Plus => Some(Inst::ADD),
            Token::Minus => Some(Inst::SUB),
            Token::Star => Some(Inst::MUL),
            Token::Slash => Some(Inst::DIV),
            _ => None,
        }
    }
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
    
    fn parse_token(&mut self) -> Option<Token> {
        self.start = self.curr;
        if self.peek_char().is_none() {
            return None;
        }
        let c: char = self.peek_char().unwrap();
        if c.is_digit(10) {
            self.consume_number();
            return match self.src[self.start..self.curr].parse::<f64>() {
                Ok(num) => Some(Token::Num(num)),
                Err(why) => panic!("failed parsing number: {}", why),
            };
        }
        self.consume_char();
        match c {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Star),
            '/' => Some(Token::Slash),
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParent),
            ' ' => self.parse_token(),
            _ => None,
        }
    }

    fn parse(&mut self, dest: &mut Vec<Inst>) {
        let mut stack = Vec::<Token>::new();
        loop {
            match self.parse_token() {
                Some(tok) => {
                    match tok {
                        Token::Num(num) => {
                            dest.push(Inst::PUSH(num));
                        },
                        Token::LeftParen => {
                            self.parse(dest);
                        },
                        Token::RightParent => {
                            while !stack.is_empty() {
                                    dest.push(stack.pop().unwrap().get_instruction().unwrap());
                            }
                            return;
                        },
                        _ => {
                            let pr = tok.get_precedance();
                            assert_ne!(pr, -1); // must be an operator
                            if stack.is_empty() {
                                stack.push(tok);
                                continue;
                            }
                            while !stack.is_empty() && stack.last().unwrap().get_precedance() >= pr {
                                dest.push(stack.pop().unwrap().get_instruction().unwrap());
                            }
                            stack.push(tok);
                        }
                    }
                },
                None => break,
            };
        }
        while !stack.is_empty() {
                dest.push(stack.pop().unwrap().get_instruction().unwrap());
        }
    }
}

fn print_program(program: &Vec<Inst>) {
    println!("--- PROGRAM_StarT ---");
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
    
    let mut compiler = Compiler::new(src);
    let mut program = Vec::<Inst>::new();
    compiler.parse(&mut program);
    program.push(Inst::PRINT);

    print_program(&program);

    match exec(&program) {
        Err(msg) => {
            println!("VM_ERROR: {msg}");
        }
        Ok(_) => ()
    }
}
