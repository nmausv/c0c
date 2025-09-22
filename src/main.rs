#![allow(
    dead_code,
    non_camel_case_types,
    clippy::upper_case_acronyms,
    clippy::needless_lifetimes
)]

mod codegen;
mod frontend;
mod heap_recursion;
mod static_analysis;
mod temps;
mod translation;

use std::{fs::read_to_string, io::Write};

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
    let lexer = frontend::lexer::Lexer::new_c0c_lexer(program);
    let parser = frontend::c0parser::ProgramParser::new();

    if verbose {
        println!("attempting parsing...\n");
        println!("program read as:\n{program}\n");
    }
    let ast = match parser.parse(program, lexer) {
        Ok(p) => p,
        Err(_) => {
            if verbose {
                eprint!("failed parsing...");
            }
            return Err(());
        }
    };
    if verbose {
        println!("program successfully parsed into AST");
        println!("program parsed as:\n{ast}\n");
    }
    let elab_ast = frontend::elaboration::elaborate(ast)?;
    if verbose {
        println!("AST successfully elaborated");
        println!("program elaborated to:\n{elab_ast}\n");
    }
    if !static_analysis::check(&elab_ast) {
        if verbose {
            eprintln!("program failed static analysis");
        }
        return Err(());
    }
    if verbose {
        println!("program passed static analysis!\n");
    }

    let mut tf = temps::TempFactory::new();
    let ir_tree = translation::translate(elab_ast, &mut tf);
    if verbose {
        println!("elaborated AST translated to IR");
        println!("program translated to:\n{ir_tree}\n");
    }

    let assembly = codegen::codegen(ir_tree, target, &mut tf);
    if verbose {
        println!("final output generated from IR");
        println!("final output:\n{assembly}\n");
    }

    Ok(assembly)
}

fn main() {
    // handle command line arguments
    let cli = Args::parse();

    eprintln!("cli input: {:?}", cli.input);
    eprintln!("cli output: {:?}", cli.output);
    eprintln!("cli target: {:?}", cli.target);

    let program = match read_to_string(&cli.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Could not open file \"{}\": {}", &cli.input, e);
            return;
        }
    };

    let output = match compile(&program, cli.verbose, cli.target.into()) {
        Ok(s) => s,
        Err(()) => {
            eprintln!("could not compile");
            return;
        }
    };

    let outfile_path = match cli.output {
        Some(_) => cli.output.as_ref().unwrap(),
        None => "a.out",
    };

    let mut outfile_options = std::fs::File::options();
    outfile_options.create(true);
    outfile_options.write(true);
    match outfile_options.open(outfile_path) {
        Ok(mut outfile) => {
            outfile.write_all(output.as_bytes()).unwrap_or_else(|_| {
                panic!("could not write to file {outfile_path}")
            });
        }
        Err(e) => eprintln!("could not open file {outfile_path}: {e}"),
    }

    println!("done!");
}

#[cfg(test)]
mod tests {
    use std::{
        fs::{read_dir, remove_file, File},
        io::{Read, Write},
        process::Command,
        thread,
    };

    use crate::compile;
    #[derive(Debug, PartialEq, Eq)]
    enum TestResult {
        Return(u8), // compile and return value
        DivZero,    // compile but raise divzero
        Error,      // fail to compile
    }

    fn get_test_result(contents: &str) -> Option<TestResult> {
        // only needs first line
        let line = contents.lines().next()?.trim();

        if line.starts_with("//test return ") {
            match line.strip_prefix("//test return ")?.parse::<i32>() {
                Ok(result) => {
                    // note that exit codes are the current method to interact
                    // with executables, and exit codes are truncated to one
                    // unsigned byte, so the intended return value should also
                    // be truncated.
                    Some(TestResult::Return(result as u8))
                }
                Err(_) => None,
            }
        } else if line.starts_with("//test div-by-zero") {
            Some(TestResult::DivZero)
        } else if line.starts_with("//test error") {
            Some(TestResult::Error)
        } else {
            None
        }
    }

    fn test_file(file: String, path: &str, verbose: bool) -> bool {
        let expected = match get_test_result(&file) {
            Some(result) => result,
            None => return false,
        };

        // compile each file without verbose mode
        let output = compile(&file, verbose, crate::codegen::Target::ARM);

        if !match expected {
            TestResult::Return(_) => output.is_ok(),
            TestResult::Error => output.is_err(),
            TestResult::DivZero => output.is_ok(),
        } {
            eprintln!(
                "file {} did not pass: expected {:?}, got success result {:?}",
                path,
                expected,
                output.is_ok(),
            );
            return false;
        }

        let Ok(output) = output else {
            return true;
        };
        let output = output.into_bytes();

        // TODO

        // save to output file
        let assembly_path = path
            .strip_suffix(".l1")
            .map_or(format!("{path}.s"), |name| format!("{name}.s"));
        let executable_path = path
            .strip_suffix(".l1")
            .map_or(path.to_string(), |name| name.to_string());
        let mut outfile = match File::create(&assembly_path) {
            Ok(outfile) => outfile,
            Err(e) => {
                eprintln!("could not create output file for {path}: {e}");
                return false;
            }
        };

        let mut bytes_written = 0;
        while bytes_written < output.len() {
            bytes_written += match outfile.write(&output[bytes_written..]) {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("could not write to output file for {path}: {e}");
                    return false;
                }
            };
        }

        // run output file

        // TODO
        let mut clang = Command::new("clang");
        clang.arg(&assembly_path);
        clang.arg("-o").arg(&executable_path);
        let clang_output = clang.output();

        remove_file(&assembly_path).unwrap_or_else(|e| {
            panic!("could not remove file {assembly_path}: {e}")
        });

        match clang_output {
            Ok(_) => {}
            Err(e) => {
                eprintln!("linking or assembling failed: {e}");
                return false;
            }
        }

        let mut executable = Command::new(&executable_path);
        let executable_output = executable.status();

        remove_file(&executable_path).unwrap_or_else(|e| {
            panic!("could not remove file {executable_path}: {e}")
        });

        // test against expected output
        // NOTE: ARM does not create an interrupt for division by zero, instead it sets the result
        // to zero and moves on. Thus, division by zero cannot be relied upon to trigger an
        // interrupt, and so correctness must be ascertained by checking against a standard C
        // compiler.
        // This procedure will need modification for later labs with invalid C syntax, such as `alloc` and
        // `alloc_array`. It is possible that a compatibility library which defines alloc and
        // alloc_array would suffice.
        match executable_output {
            Ok(result) => {
                let status = result.code().unwrap_or_else(|| {
                    panic!("process {executable_path} should have an exit code")
                });
                if expected == TestResult::Return(status as u8) {
                    // success, pass
                    true
                } else if expected == TestResult::DivZero {
                    // divzero can't be checked against the expected value, since no exceptions are
                    // raised on ARM

                    // default to pass, check against "correct" compiler later
                    true
                } else {
                    eprintln!("incorrect result for execution of {executable_path}: expected {expected:?}, got {status}");
                    false
                }
            }
            Err(e) => {
                eprintln!("could not execute file {executable_path}: {e}");
                false
            }
        }
    }

    fn test_directory(path: &str) {
        let files = read_dir(path).expect("testing directory should exist");

        #[derive(Debug)]
        enum TestResult {
            Success(String),
            Failure(String),
        }

        let (sender, receiver) = std::sync::mpsc::channel();

        for entry in files {
            let entry = entry.expect("test file should exist");
            let path = entry.path();
            let path_str = path.to_str().unwrap().to_string();

            if !path_str.ends_with("l1") {
                // ignore non L1 files
                continue;
            }

            let mut file = File::open(&path_str).unwrap();
            let mut buf = Vec::new();
            let _ = file.read_to_end(&mut buf);
            drop(file);
            let content = String::from_utf8_lossy(&buf).into_owned();

            let sender = sender.clone();

            thread::spawn(move || {
                let result = if test_file(content, &path_str, false) {
                    TestResult::Success(path_str.to_string())
                } else {
                    TestResult::Failure(path_str.to_string())
                };
                sender.send(result).unwrap();
            });
        }

        // close the channel
        drop(sender);

        let mut passed = 0;
        let mut failed = 0;
        let mut results = Vec::new();

        for result in receiver {
            match result {
                TestResult::Success(_) => passed += 1,
                TestResult::Failure(_) => failed += 1,
            };
            results.push(result);
        }

        eprintln!(
            "total tests: {}, tests passed: {}, tests failed: {}",
            passed + failed,
            passed,
            failed,
        );

        if failed > 0 {
            let _: Vec<_> = results
                .iter()
                .filter(|t| matches!(t, TestResult::Failure(_)))
                .map(|t| eprintln!("{t:?}"))
                .collect();
            panic!();
        }
    }

    mod l1 {
        use super::*;

        #[test]
        fn basic() {
            test_directory("tests/l1-basic");
        }

        #[test]
        fn large() {
            test_directory("tests/l1-large");
        }

        #[test]
        fn custom() {
            test_directory("tests/l1-custom");
        }

        #[test]
        fn wip() {
            test_directory("tests/wip");
        }
    }
}
