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
    if (x >= 45 && x * x != 2025)
        if (x == 46)
            return -1;
        else
            return -2;

    return x < 45 ? ~0 : ~~0;
}
";
    println!("program to compile: \n[{program}]");

    let parser = frontend::c0parser::ProgramParser::new();
    let program = parser.parse(program).unwrap();
    println!("program parsed as:\n{program}\n");
    let elab_program = frontend::elaboration::elaborate(program);
    println!("program elaborated to:\n{elab_program}\n");
    if !static_analysis::check(&elab_program) {
        println!("program failed static analysis! aborting...\n");
        return;
    }
    println!("program passed static analysis!\n");

    let mut tf = temps::TempFactory::new();
    let ir_tree = translation::translate(elab_program, &mut tf);
    println!("program translated to:\n{ir_tree}\n");

    let abstract_assembly =
        codegen::codegen(ir_tree, codegen::Target::AbstractAssembly, &mut tf);
    println!("abstract assembly:\n{abstract_assembly}\n");

    println!("done!");
}
