use crate::lexer::{
    Lexer,
    Token
};

use crate::vm::{
    Inst
};

pub struct Compiler {
    lexer: Lexer,
    insts: Vec<Inst>,
}

#[derive(Debug)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Grt,
    Les,
    Gte,
    Lse,
    Eql,
    Nql,
}

impl Compiler {
    pub fn new(src: String) -> Compiler {
        Compiler {
            lexer: Lexer::new(src),
            insts: Vec::new()
        }
    }

    pub fn compile(&mut self) -> Result<Vec<Inst>, &'static str>{
        if self.lexer.had(Token::Print)? {
            self.expr(0)?;
            self.insts.push(Inst::Print);
            println!("Print");
        } else {
            return Err("statement require")
        }
        Ok(self.insts.clone())
    }

    fn infix_binding_power(&self, tok: &Operator) -> (u8, u8) {
        match tok {
            Operator::Eql | Operator::Nql => (1, 2),
            Operator::Les | Operator::Lse |
            Operator::Grt | Operator::Gte => (3, 4),
            Operator::Add | Operator::Sub => (5, 6),
            Operator::Mul | Operator::Div => (7, 8),
        }
    }

    pub fn expr(&mut self, min_bp: u8) -> Result<(), &'static str> {
        match self.lexer.next()?.val {
            Token::LiteralInt(val) => {
                println!("PUSH {}", val);
                self.insts.push(Inst::PushInt(val));
            },
            Token::LiteralString(val) => {
                println!("STORE {}", val);
                todo!("STORE type_string");
            },
            Token::Identifer(id) => {
                println!("LOAD {}", id);
                todo!("LOAD identifier");
            },
            _ => return Err("unexpected token")
        };

        loop {
            let op = match self.lexer.peak()?.val {
                Token::Minus => Operator::Sub,
                Token::Slash => Operator::Div,
                Token::Plus => Operator::Add,
                Token::Star => Operator::Mul,
                Token::EOF => return Ok(()),
                _ => return Err("unexpected token got operator")
            };

            let (l_bp, r_bp) = self.infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.lexer.next()?;
            self.expr(r_bp)?;

            match op {
                Operator::Sub => {
                    println!("Sub");
                    self.insts.push(Inst::Sub);
                },
                Operator::Div => {
                    println!("Div");
                    self.insts.push(Inst::Div);
                },
                Operator::Add => {
                    println!("Add");
                    self.insts.push(Inst::Add);
                },
                Operator::Mul => {
                    println!("Mul");
                    self.insts.push(Inst::Mul);
                },
                Operator::Eql => {
                    println!("Eql");
                },
                Operator::Nql => {
                    println!("Nql");
                },
                Operator::Les => {
                    println!("Les");
                },
                Operator::Lse => {
                    println!("Lse");
                },
                Operator::Grt => {
                    println!("Grt");
                },
                Operator::Gte => {
                    println!("Gte");
                },
            };
        }
        Ok(())
    }
}