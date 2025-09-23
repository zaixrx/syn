use crate::lexer::{
    Lexer,
    Token
};

pub struct Compiler {
    lexer: Lexer,
}

enum Operator {
    Add,
    Sub,
    Mul,
    Div
}

impl Compiler {
    pub fn new(src: String) -> Compiler {
        Compiler {
            lexer: Lexer::new(src)
        }
    }

    pub fn compile(&mut self) -> Result<(), &'static str>{
        let _ = self.expr(0)?;
        Ok(())
    }

    fn infix_binding_power(&self, tok: &Operator) -> (u8, u8) {
        match tok {
            Operator::Add |
            Operator::Sub => (1, 2),
            Operator::Mul |
            Operator::Div => (3, 4),
        }
    }

    pub fn expr(&mut self, min_bp: u8) -> Result<(), &'static str> {
        match self.lexer.next()?.val {
            Token::LiteralInt(val) => {
                println!("PUSH {}", val);
            },
            Token::LiteralString(val) => {
                println!("STORE {}", val)
            },
            Token::Identifer(id) => {
                println!("LOAD {}", id);
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
                Operator::Add => println!("ADD"),
                Operator::Sub => println!("Sub"),
                Operator::Div => println!("Div"),
                Operator::Mul => println!("Mul"),
            };
        }
        Ok(())
    }
}