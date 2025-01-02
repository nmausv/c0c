pub enum Target {
    AbstractAssembly,
    ARM,
    LLVMIR,
}

mod abstract_assembly;

pub fn codegen(
    ir: crate::translation::tree::Program,
    target: Target,
    tf: &mut crate::temps::TempFactory,
) -> String {
    match target {
        Target::AbstractAssembly => {
            let instructions = abstract_assembly::ir_to_abstract(ir, tf);
            instructions.into_iter().map(|i| format!("{i}\n")).collect()
        }
        Target::ARM => todo!(),
        Target::LLVMIR => todo!(),
    }
}
