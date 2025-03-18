pub enum Target {
    AbstractAssembly,
    ARM,
    LLVM,
    x86_64,
}

pub mod abstract_assembly;
mod regalloc;

use regalloc::regalloc;

use crate::translation::tree::Program;

pub fn codegen(
    ir: Program,
    target: Target,
    tf: &mut crate::temps::TempFactory,
) -> String {
    let instructions = abstract_assembly::ir_to_abstract(ir, tf);

    match target {
        Target::AbstractAssembly => {
            regalloc(&instructions);

            instructions
                .into_iter()
                .map(|i| match i {
                    abstract_assembly::Instruction::Label(_) => format!("{i} "),
                    _ => format!("{i}\n"),
                })
                .collect()
        }
        Target::ARM => todo!(),
        Target::LLVM => todo!(),
        Target::x86_64 => todo!(),
    }
}
