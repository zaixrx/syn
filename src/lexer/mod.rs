pub struct Lexer {
    src: String,
    curr: usize,
    start: usize,
    line: usize,
}

pub struct Token {
    typ: TokenType,
    pos: usize,
    lxm: String // WARN: individual heap allocation will isn't cache friendly
}

pub enum TokenType {
    LeftBrace,
	RightBrace,
	SemiColon,

    Minus,
	Plus,
	Splash,
	Star,

	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
    And,
	Or,

    Number,
    True,
    False,

	While,
    Break,
	Continue,

    If,
    Else,
	
	Let,
	Print,
}

/*

a := 1;
b := 1;
for i := 0; i < 10; i += 1 {
    a = a + b;
    b = a - b;
}

*/

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer {
            src,
            curr: 0,
            start: 0,
            line: 0,
        }
    }

    fn consume_

    fn emit_token(&mut self) -> Option<Token> {
        self.start = self.curr;
        let chars = self.src.chars().collect::<Vec<char>>();
        match chars[self.start] {
            '0'..='9' => {

            },
            _ => unreachable!()
        };
        None
    }
    
    // TODO: return an iterator
    pub fn get_tokens(&mut self) -> Vec<Token> {
        let mut toks = Vec::new();
        while let Some(tok) = self.emit_token() {
            toks.push(tok);
        }
        toks
    }
}

