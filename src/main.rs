mod vm;
mod lexer;
mod compiler;

use vm::{Program, VM};
use compiler::Compiler;

use std::fs;
use std::process;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filepath: String,

    #[arg(short, long, default_value="false")]
    bytecode: bool,
    
    // TODO: handle cache invalidtion
    // make sure that cache maps to the right TU
    #[arg(short, long, default_value="false")]
    cache: bool,
}

const CACHE_DIR: &'static str = ".syncache";

fn main() {
    let args = Args::parse();

    let src_path = std::path::Path::new(&args.filepath);
    let src_filename = src_path.file_name().and_then(|s| s.to_str()).unwrap_or("unknown");

    let prog = Program::read_from_file(format!("{}/{}c", CACHE_DIR, src_filename)).unwrap_or_else(|err| {
        println!("LOG: counldn't find cache here: {err}");

        let src = fs::read_to_string(src_path).unwrap_or_else(|err| {
            eprintln!("Lexer failed: {}", err);
            process::exit(69)
        });

        let compiler = Compiler::new(src);
        compiler.compile().unwrap_or_else(|errs| {
            eprintln!("Codegen failed:");
            for err in errs {
                eprintln!("{}", err);
            }
            process::exit(69)
        })
    });

    if args.bytecode {
        prog.disassemble();
    }

    if args.cache {
        let cachedir_exists = std::fs::exists(CACHE_DIR).expect("idk"); if !cachedir_exists {
            std::fs::create_dir(CACHE_DIR).expect("idk");
        }
        let file_path = format!("{}/{}c", CACHE_DIR, src_filename);
        prog.write_to_file(file_path).unwrap_or_else(|err| {
            eprintln!("Caching Failed: {}", err);
            process::exit(69); // TODO: exit?
        });
    }

    let vm = VM::new(prog);
    vm.exec().unwrap_or_else(|err| {
        eprintln!("VM failed: {}", err);
        process::exit(69)
    });
}
