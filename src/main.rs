use std::fs::read_to_string;

mod c0c_lib;

fn main() {
    // decide if default patterns or custom ones
    let file = read_to_string("../tests/fib.l0").expect("could not read file");

    let lexer = c0c_lib::lexer::Lexer::new_c0c_lexer();
    let mtokens = lexer.tokenize(&file).unwrap();
    let tokens: Vec<_> = mtokens.iter().cloned().map(|t| t.unmark()).collect();
    dbg!(tokens);

    println!("Hello, world!");
}
