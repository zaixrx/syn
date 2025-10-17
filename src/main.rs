mod vm;
mod lexer;
mod compiler;

use vm::VM;
use compiler::Compiler;

use std::fs;
use std::process;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filepath: String,

    #[arg(short, long, default_value="false")]
    bytecode: bool
}

fn main() {
    let args = Args::parse();

    let src = fs::read_to_string(&args.filepath).unwrap_or_else(|err| {
        eprintln!("Lexer failed: {}", err);
        process::exit(69)
    });

    let compiler = Compiler::new(src);
    let prog = compiler.compile().unwrap_or_else(|errs| {
        eprintln!("Codegen failed:");
        for err in errs {
            eprintln!("{}", err);
        }
        process::exit(69)
    });

    if args.bytecode {
        prog.disassemble();
    }

    VM::new(prog).exec().unwrap_or_else(|err| {
        eprintln!("VM failed: {}", err);
        process::exit(69)
    });
}
