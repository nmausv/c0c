mod abstract_assembly;
mod arm;
mod regalloc;

#[allow(unused_imports)]
use regalloc::regalloc;

use crate::{temps::TempFactory, translation::tree::Program};

pub enum Target {
    AbstractAssembly,
    ARM,
    LLVM,
    x86_64,
}

const DEBUG: bool = true;

impl abstract_assembly::Program {}

pub fn codegen(ir: Program, target: Target, tf: &mut TempFactory) -> String {
    let instructions = abstract_assembly::ir_to_abstract(ir, tf);

    #[allow(clippy::needless_if)]
    if DEBUG {
        eprintln!("codegen debugging");
        eprintln!(
            "{}",
            instructions
                .as_ref()
                .iter()
                .map(|i| match i {
                    abstract_assembly::Instruction::Label(_) => format!("{i} "),
                    _ => format!("{i}\n"),
                })
                .collect::<String>(),
        );
    }

    match target {
        Target::AbstractAssembly => instructions
            .into_iter()
            .map(|i| match i {
                abstract_assembly::Instruction::Label(_) => format!("{i} "),
                _ => format!("{i}\n"),
            })
            .collect(),
        Target::ARM => {
            let instructions = arm::specialize(instructions);

            let mut result = String::new();

            // need to give ARM metadata information
            result.push_str(
                " .section __TEXT,__text,regular,pure_instructions\n",
            );
            result.push_str(" .build_version macos, 15, 0\n");
            result.push_str(" .globl _main\n");
            result.push_str(" .p2align 2\n");
            result.push_str("_main:\n");
            result.push_str(" .cfi_startproc\n");

            // this is basically just a guess to make the reserve sufficient
            const INSTRUCTION_SIZE: usize = 16;
            result.reserve(INSTRUCTION_SIZE * instructions.as_ref().len());
            for line in instructions.lines() {
                result.push(' ');
                result.push_str(&line);
                result.push('\n');
            }
            result.shrink_to_fit();

            result.push_str(" .cfi_endproc\n");

            result
        }
        Target::LLVM => todo!(),
        Target::x86_64 => todo!(),
    }
}
