use std::error::Error;

use crate::lexer::{
    Lexer,
    Token,
    TokenHeader
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


#[derive(Debug)]
pub struct CompilerError {
    token: TokenHeader,
    message: String,
}

impl CompilerError {
    pub fn new(message: String, token: TokenHeader) -> CompilerError {
        CompilerError {
            token,
            message,
        }
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "at {}-{}: {}, got token {:?}", self.token.line, self.token.pos, self.message, self.token.val)
    }
}

impl std::error::Error for CompilerError {}

impl Compiler {
    pub fn new(filename: String, src: String) -> Compiler {
        Compiler {
            lexer: Lexer::new(filename, src),
            insts: Vec::new()
        }
    }

    pub fn compile(&mut self) -> Result<Vec<Inst>, Box<dyn Error>> {
        let tok = self.lexer.next()?;
        match tok.val {
            Token::Print => {
                self.insts.push(Inst::Print);
                self.expr(0)?;
            },
            _ => return Err(Box::new(
                CompilerError::new(String::from("unsupported statement"), tok)
            ))
        };
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

    pub fn expr(&mut self, min_bp: u8) -> Result<(), Box<dyn Error>> {
        let tok = self.lexer.next()?;
        match tok.val {
            Token::LiteralInt(val) => {
                self.insts.push(Inst::PushInt(val));
            },
            Token::LiteralString(_val) => {
                todo!("STORE type_string");
            },
            Token::Identifer(_id) => {
                todo!("LOAD identifier");
            },
            _ => return Err(Box::new(
                CompilerError::new(String::from("unsupported expression"), tok)
            ))
        };

        loop {
            let tok = self.lexer.peak()?;

            let op = match tok.val {
                Token::Minus => Operator::Sub,
                Token::Slash => Operator::Div,
                Token::Plus => Operator::Add,
                Token::Star => Operator::Mul,
                Token::EOF => return Ok(()),
                _ => return Err(Box::new(
                    CompilerError::new(String::from("unsupported operator"), tok)
                ))
            };

            let (l_bp, r_bp) = self.infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.lexer.next()?;
            self.expr(r_bp)?;

            match op {
                Operator::Sub => {
                    self.insts.push(Inst::Sub);
                },
                Operator::Div => {
                    self.insts.push(Inst::Div);
                },
                Operator::Add => {
                    self.insts.push(Inst::Add);
                },
                Operator::Mul => {
                    self.insts.push(Inst::Mul);
                },
                Operator::Eql => {
                    todo!("Eql")
                },
                Operator::Nql => {
                    todo!("Nql")
                },
                Operator::Les => {
                    todo!("Less")
                },
                Operator::Lse => {
                    todo!("Lse")
                },
                Operator::Grt => {
                    todo!("Grt")
                },
                Operator::Gte => {
                    todo!("Gte")
                },
            };
        }
        Ok(())
    }
}