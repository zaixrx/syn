mod vm;
mod lexer;
mod compiler;

use vm::VM;
use compiler::Compiler;

use std::fs;
use std::env;
use std::process;

struct Args {
    filepath: String,
}

fn read_args(mut args: impl Iterator<Item = String>) -> Result<Args, &'static str> {
    args.next(); // ignore exectuable
    let filepath = match args.next() {
        Some(s) => s,
        None => return Err("Expected \"filepath\" found none")
    };
    Ok(Args { filepath })
}

fn main() {
    let args = read_args(env::args()).unwrap_or_else(|err| {
        eprintln!("ArgsParser failed: {}", err);
        process::exit(69)
    });

    let src = fs::read_to_string(&args.filepath).unwrap_or_else(|err| {
        eprintln!("Lexer failed: {}", err);
        process::exit(69)
    });

    let compiler = Compiler::new(src);

    let chunk = compiler.compile().unwrap_or_else(|err| {
        eprintln!("Codegen failed: {}", err);
        process::exit(69)
    });
    chunk.disassemble();

    let mut vm = VM::new(chunk);
    vm.exec().unwrap_or_else(|err| {
        eprintln!("VM failed: {}", err);
        process::exit(69)
    });
}
