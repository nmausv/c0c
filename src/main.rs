use std::fs::read_to_string;

mod c0c_lib;
use c0c_lib::frontend::lexer;

fn main() {
    // decide if default patterns or custom ones
    let file =
        read_to_string("../c0c/tests/fib.l0").expect("could not read file");

    let lexer = lexer::Lexer::new_c0c_lexer();
    let mtokens = lexer.tokenize(&file).unwrap();
    let tokens: Vec<_> = mtokens.iter().cloned().map(|t| t.unmark()).collect();
    dbg!(tokens);
}
