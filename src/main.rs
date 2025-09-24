mod lexer;
mod compiler;
mod vm;

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
        eprintln!("Failed parsing args: {}", err);
        process::exit(69)
    });

    let src = fs::read_to_string(args.filepath).unwrap_or_else(|err| {
        eprintln!("Failed reading file: {}", err);
        process::exit(69)
    });

    let mut compiler = Compiler::new(src);
    let prog = compiler.compile().unwrap_or_else(|err| {
        eprintln!("Failed generating bytecode: {}", err);
        process::exit(69)
    });

    vm::exec(prog).unwrap_or_else(|err| {
        eprintln!("Failed executing bytecode: {}", err);
        process::exit(69)
    });
}
