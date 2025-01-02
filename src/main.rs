#![allow(dead_code)]

mod codegen;
mod frontend;
mod regalloc;
mod static_analysis;
mod temps;
mod translation;

fn main() {
    let program = "int main() {int x = 3 / 4 / 5 / 6 / 7; return x;}";
    println!("program to compile: \n[{program}]");

    let mut tf = temps::TempFactory::new();
    let parser = frontend::c0parser::ProgramParser::new();
    let program = parser.parse(program).unwrap();
    println!("program parsed as:\n{program}");
    let elab_program = frontend::elaboration::elaborate(program);
    println!("program elaborated to:\n{elab_program}");
    if !static_analysis::check(&elab_program) {
        println!("program failed to typecheck!");
        return;
    }
    println!("program typechecked!");

    let ir_tree = translation::translate(elab_program, &mut tf);
    println!("program translated to:\n{ir_tree}");

    let abstract_assembly =
        codegen::codegen(ir_tree, codegen::Target::AbstractAssembly, &mut tf);

    println!("abstract assembly:\n{abstract_assembly}");

    println!("done!");
}
