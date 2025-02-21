#![allow(
    dead_code,
    non_camel_case_types,
    clippy::upper_case_acronyms,
    clippy::needless_lifetimes
)]

mod codegen;
mod frontend;
mod regalloc;
mod static_analysis;
mod temps;
mod translation;

use std::fs::read_to_string;

use clap::{Parser, ValueEnum};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Target {
    asm,
    x86_64,
    ARM,
    LLVM,
}

impl From<Target> for codegen::Target {
    fn from(value: Target) -> Self {
        match value {
            Target::asm => codegen::Target::AbstractAssembly,
            Target::x86_64 => codegen::Target::x86_64,
            Target::ARM => codegen::Target::ARM,
            Target::LLVM => codegen::Target::LLVM,
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file to compile
    #[arg(short, long)]
    input: String,

    /// File to save emitted code to
    #[arg(short, long)]
    output: Option<String>,

    /// Target to emit
    #[arg(short, long)]
    target: Target,

    // Verbose mode
    #[arg(short, long)]
    verbose: bool,
}

fn compile(
    program: &str,
    verbose: bool,
    target: codegen::Target,
) -> Result<String, ()> {
    if verbose {
        println!("program to compile: \n[{program}]");
    }

    let lexer = frontend::lexer::Lexer::new_c0c_lexer(program);
    let parser = frontend::c0parser::ProgramParser::new();
    let ast = match parser.parse(program, lexer) {
        Ok(p) => p,
        Err(_) => {
            eprint!("failed parsing...");
            return Err(());
        }
    };
    if verbose {
        println!("program parsed as:\n{ast}\n");
    }
    let elab_ast = frontend::elaboration::elaborate(ast)?;
    if verbose {
        println!("program elaborated to:\n{elab_ast}\n");
    }
    if !static_analysis::check(&elab_ast) {
        return Err(());
    }
    if verbose {
        println!("program passed static analysis!\n");
    }

    let mut tf = temps::TempFactory::new();
    let ir_tree = translation::translate(elab_ast, &mut tf);
    if verbose {
        println!("program translated to:\n{ir_tree}\n");
    }

    let abstract_assembly = codegen::codegen(ir_tree, target, &mut tf);
    if verbose {
        println!("abstract assembly:\n{abstract_assembly}\n");
    }

    Ok(abstract_assembly)
}

fn main() {
    // handle command line arguments
    let cli = Args::parse();

    println!("cli input: {:?}", cli.input);
    println!("cli output: {:?}", cli.output);
    println!("cli target: {:?}", cli.target);

    // bellsprout-return02-l2.l1

    let program = match read_to_string(&cli.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Could not open file \"{}\": {}", &cli.input, e);
            return;
        }
    };

    if compile(&program, cli.verbose, cli.target.into()).is_err() {
        println!("could not compile");
    }

    println!("done!");
}

#[cfg(test)]
mod tests {
    mod l1 {
        use std::fs::{read_dir, read_to_string};

        use crate::compile;

        enum TestResult {
            Return(i64), // compile and return value
            DivZero,     // compile but raise divzero
            Error,       // fail to compile
        }

        fn get_test_result(contents: &str) -> Option<TestResult> {
            // only needs first line
            let line = contents.lines().next()?.trim();

            if line.starts_with("//test return ") {
                Some(TestResult::Return(
                    line.strip_prefix("//test return ")?
                        .parse::<i64>()
                        .expect("test files should start with proper expected return values"),
                ))
            } else if line.starts_with("//test div-by-zero") {
                Some(TestResult::DivZero)
            } else if line.starts_with("//test error") {
                Some(TestResult::Error)
            } else {
                None
            }
        }

        fn test_directory(path: &str) {
            let files = read_dir(path).expect("testing directory should exist");
            for entry in files {
                let entry = entry.expect("test file should exist");

                eprint!("testing {}...", entry.path().to_str().unwrap());

                let file = read_to_string(entry.path())
                    .expect("test file should be readable");

                let expected = get_test_result(&file)
                    .expect("test file should have valid expected result");

                // compile each file without verbose mode
                let output = compile(
                    &file,
                    false,
                    crate::codegen::Target::AbstractAssembly,
                );

                if !match expected {
                    TestResult::Return(_) => output.is_ok(),
                    TestResult::Error => output.is_err(),
                    TestResult::DivZero => output.is_ok(),
                } {
                    let output = compile(
                        &file,
                        true,
                        crate::codegen::Target::AbstractAssembly,
                    );
                    dbg!(&output);
                    let _ = output.unwrap();
                    unreachable!();
                }

                eprintln!("passed");

                // save to output file
                // run output file
                // test against expected output
                // delete output file
            }
        }

        #[test]
        fn basic() {
            test_directory("tests/l1-basic");
        }

        #[test]
        fn large() {
            test_directory("tests/l1-large");
        }
    }
}
