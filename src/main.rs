#![allow(dead_code)]

mod codegen;
mod frontend;
mod regalloc;
mod static_analysis;
mod temps;
mod translation;

fn main() {
    let program = "
int main() {
    int x = 0;
    for (int i = 0; i < 10; i++)
        x += i;

    // test dangling else
    if (x * x != 2025)
        if (x != 45)
            return -1;
        else
            return -2;

    return x;
}
";
    println!("program to compile: \n[{program}]");

    let mut tf = temps::TempFactory::new();
    let parser = frontend::c0parser::ProgramParser::new();
    let program = parser.parse(program).unwrap();
    println!("program parsed as:\n{program}");
    let elab_program = frontend::elaboration::elaborate(program);
    println!("program elaborated to:\n{elab_program}");
    if !static_analysis::check(&elab_program) {
        println!("program failed static analysis!");
        return;
    }
    println!("program passed static analysis!");

    let ir_tree = translation::translate(elab_program, &mut tf);
    println!("program translated to:\n{ir_tree}");

    let abstract_assembly =
        codegen::codegen(ir_tree, codegen::Target::AbstractAssembly, &mut tf);

    println!("abstract assembly:\n{abstract_assembly}");

    println!("done!");
}
