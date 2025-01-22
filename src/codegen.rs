pub enum Target {
    AbstractAssembly,
    Arm,
    Llvm,
}

mod abstract_assembly;

use crate::translation::tree::Program;

pub fn codegen(
    ir: Program,
    target: Target,
    tf: &mut crate::temps::TempFactory,
) -> String {
    match target {
        Target::AbstractAssembly => {
            let instructions = abstract_assembly::ir_to_abstract(ir, tf);
            instructions
                .into_iter()
                .map(|i| match i {
                    abstract_assembly::Instruction::Label(_) => format!("{i} "),
                    _ => format!("{i}\n"),
                })
                .collect()
        }
        Target::Arm => todo!(),
        Target::Llvm => todo!(),
    }
}
