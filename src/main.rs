mod vm;
mod lexer;
mod compiler;

use vm::{Program, VM};
use compiler::Compiler;
use std::path::Path;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filepath: String,

    #[arg(short, long, default_value="false")]
    bytecode: bool,
    
    // TODO: handle cache invalidtion
    // make sure that cache maps to the right TU
    #[arg(short, long, default_value="true")]
    load_cache: bool,

    #[arg(short, long, default_value="true")]
    write_cache: bool,
}

const CACHE_DIR: &'static str = ".syncache";

fn load_program(src_path: &Path, load_cache: bool) -> Program {
    if load_cache {
        let src_stem = src_path.file_stem().and_then(|p| p.to_str()).unwrap_or_else(|| {
            eprintln!("ERROR: expected valid source file path");
            std::process::exit(69)
        });
        let cache_file_path = format!("{CACHE_DIR}/{src_stem}.sync");
        if let Ok(prog) = Program::read_from_file(&cache_file_path) {
            return prog;
        }
    }
    let src = std::fs::read_to_string(src_path).unwrap_or_else(|err| {
        eprintln!("Lexer failed: {}", err);
        std::process::exit(69)
    });
    let compiler = Compiler::new(src);
    compiler.compile(|err| {
        eprintln!("{}", err);
    }).unwrap_or_else(|| {
        eprintln!("Failed to compile syn program.");
        std::process::exit(69)
    })
}

fn write_cache(src_path: &Path, prog: &Program) {
    if std::fs::exists(CACHE_DIR).unwrap_or(false) || std::fs::create_dir(CACHE_DIR).is_ok() {
        let src_stem = src_path.file_stem().and_then(|p| p.to_str()).unwrap();
        let cache_file_path = format!("{CACHE_DIR}/{src_stem}.sync");
        if let Err(err) = prog.write_to_file(&cache_file_path) {
            eprintln!("ERROR: Failed writing {cache_file_path}: {err}");
            std::process::exit(69); // exit?
        }
    } else {
        // exit?
    }
}

fn exec(prog: Program) {
    let vm = VM::new(prog);
    vm.exec().unwrap_or_else(|err| {
        eprintln!("VM_ERROR: {err}");
        std::process::exit(69)
    });
}

fn main() {
    let args = Args::parse();

    let src_path = Path::new(&args.filepath);

    let prog = load_program(src_path, args.load_cache);

    if args.bytecode {
        prog.disassemble();
    }

    if args.write_cache {
        write_cache(&src_path, &prog);
    }

    exec(prog);
}
