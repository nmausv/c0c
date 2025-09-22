use std::collections::HashMap;

use crate::codegen::abstract_assembly;

use crate::frontend::ast::{Binop, Unop};

use super::{
    abstract_assembly::Destination,
    regalloc::{self, regalloc, StackLocation},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    X0,
    /// X0 : return register
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    /// X15: third last caller saved register, used for scratch space, volatile
    X16,
    /// X16: second to last caller saved register, used for scratch space, volatile
    X17,
    /// X17: last caller saved register, used for scratch space, volatile
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,
}

impl From<abstract_assembly::Register> for Register {
    fn from(value: abstract_assembly::Register) -> Self {
        match value {
            abstract_assembly::Register::Return => Register::X0,
            abstract_assembly::Register::Remainder => Register::X1,
        }
    }
}

impl Register {
    fn as_32_bit(self) -> String {
        match self {
            Register::X0 => "w0".to_string(),
            Register::X1 => "w1".to_string(),
            Register::X2 => "w2".to_string(),
            Register::X3 => "w3".to_string(),
            Register::X4 => "w4".to_string(),
            Register::X5 => "w5".to_string(),
            Register::X6 => "w6".to_string(),
            Register::X7 => "w7".to_string(),
            Register::X8 => "w8".to_string(),
            Register::X9 => "w9".to_string(),
            Register::X10 => "w10".to_string(),
            Register::X11 => "w11".to_string(),
            Register::X12 => "w12".to_string(),
            Register::X13 => "w13".to_string(),
            Register::X14 => "w14".to_string(),
            Register::X15 => "w15".to_string(),
            Register::X16 => "w16".to_string(),
            Register::X17 => "w17".to_string(),
            Register::X18 => "w18".to_string(),
            Register::X19 => "w19".to_string(),
            Register::X20 => "w20".to_string(),
            Register::X21 => "w21".to_string(),
            Register::X22 => "w22".to_string(),
            Register::X23 => "w23".to_string(),
            Register::X24 => "w24".to_string(),
            Register::X25 => "w25".to_string(),
            Register::X26 => "w26".to_string(),
            Register::X27 => "w27".to_string(),
            Register::X28 => "w28".to_string(),
            Register::X29 => "w29".to_string(),
            Register::X30 => "w30".to_string(),
        }
    }

    fn as_64_bit(self) -> String {
        self.to_string()
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::X0 => write!(f, "x0"),
            Register::X1 => write!(f, "x1"),
            Register::X2 => write!(f, "x2"),
            Register::X3 => write!(f, "x3"),
            Register::X4 => write!(f, "x4"),
            Register::X5 => write!(f, "x5"),
            Register::X6 => write!(f, "x6"),
            Register::X7 => write!(f, "x7"),
            Register::X8 => write!(f, "x8"),
            Register::X9 => write!(f, "x9"),
            Register::X10 => write!(f, "x10"),
            Register::X11 => write!(f, "x11"),
            Register::X12 => write!(f, "x12"),
            Register::X13 => write!(f, "x13"),
            Register::X14 => write!(f, "x14"),
            Register::X15 => write!(f, "x15"),
            Register::X16 => write!(f, "x16"),
            Register::X17 => write!(f, "x17"),
            Register::X18 => write!(f, "x18"),
            Register::X19 => write!(f, "x19"),
            Register::X20 => write!(f, "x20"),
            Register::X21 => write!(f, "x21"),
            Register::X22 => write!(f, "x22"),
            Register::X23 => write!(f, "x23"),
            Register::X24 => write!(f, "x24"),
            Register::X25 => write!(f, "x25"),
            Register::X26 => write!(f, "x26"),
            Register::X27 => write!(f, "x27"),
            Register::X28 => write!(f, "x28"),
            Register::X29 => write!(f, "x29"),
            Register::X30 => write!(f, "x30"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MemoryLocation {
    Stack(StackLocation),
    Register(Register),
}

impl From<regalloc::MemoryLocation> for MemoryLocation {
    fn from(value: regalloc::MemoryLocation) -> Self {
        match value {
            regalloc::MemoryLocation::Stack(stack_location) => {
                MemoryLocation::Stack(stack_location)
            }
            regalloc::MemoryLocation::Register(register) => {
                MemoryLocation::Register(register.into())
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Stack(StackLocation),
    Register(Register),
    IntConst(i32),
}

impl From<MemoryLocation> for Operand {
    fn from(value: MemoryLocation) -> Self {
        match value {
            MemoryLocation::Stack(loc) => Operand::Stack(loc),
            MemoryLocation::Register(reg) => Operand::Register(reg),
        }
    }
}

impl Operand {
    fn convert(
        value: abstract_assembly::Operand,
        memory_map: &HashMap<&Destination, MemoryLocation>,
    ) -> Operand {
        if let abstract_assembly::Operand::IntConst(n) = value {
            Operand::IntConst(n)
        } else {
            let dest: abstract_assembly::Destination = value
                .try_into()
                .expect("should not be IntConst by if guard");
            memory_map[&dest].into()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Move {
        d: MemoryLocation,
        s: Operand,
    },
    Binop {
        d: Register,
        s1: Register,
        op: Binop,
        s2: Register,
    },
    Unop {
        d: Register,
        op: Unop,
        s: Register,
    },
    Return,
    SetupStack(usize),
    CleanupStack(usize),
}

fn format_binop(
    dest: &Register,
    s1: &Register,
    op: &Binop,
    s2: &Register,
) -> Vec<String> {
    let dest = dest.as_32_bit();
    let s1 = s1.as_32_bit();
    let s2 = s2.as_32_bit();

    match op {
        Binop::Plus => vec![format!("add {dest}, {s1}, {s2}")],
        Binop::Minus => {
            vec![format!("sub {dest}, {s1}, {s2}")]
        }
        Binop::Times => {
            vec![format!("mul {dest}, {s1}, {s2}")]
        }
        Binop::Less => todo!(),
        Binop::LessEq => todo!(),
        Binop::Greater => todo!(),
        Binop::GreaterEq => todo!(),
        Binop::Eq => todo!(),
        Binop::NotEq => todo!(),
        Binop::BitAnd => todo!(),
        Binop::BitXor => todo!(),
        Binop::BitOr => todo!(),
        Binop::Divide => {
            vec![format!("sdiv {dest}, {s1}, {s2}")]
        }
        Binop::Modulo => {
            // arm doesn't have modulo, so use division to jury rig it, same thing macOS clang does
            // z <- x % y
            // equivalent to
            // z <- x / y
            // z <- z * y
            // z <- x - z
            //
            // BUG: the above rewrite is only correct when x, y, z, are distinct memory locations.
            // but this shouldn't happen in three address assembly?

            dbg!(&dest);
            dbg!(&s1);
            dbg!(&s2);

            vec![
                format!("sdiv {dest}, {s1}, {s2}"),
                format!("mul {dest}, {dest}, {s2}"),
                format!("sub {dest}, {s1}, {dest}"),
            ]
        }
        Binop::LogAnd => unreachable!(),
        Binop::LogOr => unreachable!(),
        Binop::Shl => todo!(),
        Binop::Shr => todo!(),
    }
}

fn format_unop(dest: &Register, s: &Register, op: &Unop) -> String {
    let dest = dest.as_32_bit();
    let s = s.as_32_bit();
    match op {
        Unop::LogNegate => unreachable!(),
        Unop::BitNegate => todo!(),
        Unop::Negative => {
            format!("neg {dest}, {s}")
        }
    }
}

pub struct Program(Vec<Instruction>);

impl AsRef<[Instruction]> for Program {
    fn as_ref(&self) -> &[Instruction] {
        &self.0
    }
}

impl IntoIterator for Program {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Program {
    #[allow(unused_variables)]
    pub fn lines(&self) -> Vec<String> {
        let mut lines = Vec::new();
        for instr in &self.0 {
            match instr {
                // move register to register
                Instruction::Move {
                    d: MemoryLocation::Register(d_reg),
                    s: Operand::Register(s_reg),
                } => {
                    let d_reg = d_reg.as_32_bit();
                    let s_reg = s_reg.as_32_bit();
                    lines.push(format!("mov {d_reg}, {s_reg}"));
                }
                // move stack to register
                Instruction::Move {
                    d: MemoryLocation::Register(d_reg),
                    s: Operand::Stack(stack_location),
                } => {
                    let stack = stack_location.index;
                    let d_reg = d_reg.as_32_bit();
                    lines.push(format!("ldr {d_reg}, [SP, #{stack}]"));
                }
                // move immediate to register
                Instruction::Move {
                    d: MemoryLocation::Register(d_reg),
                    s: Operand::IntConst(n),
                } => {
                    let d_reg = d_reg.as_32_bit();
                    lines.push(format!("mov {d_reg}, #{n}"));
                }
                // move register to stack
                Instruction::Move {
                    d: MemoryLocation::Stack(stack_location),
                    s: Operand::Register(reg),
                } => {
                    let stack = stack_location.index;
                    let reg = reg.as_32_bit();
                    lines.push(format!("str {reg}, [SP, #{stack}]"));
                }
                // move stack to stack
                Instruction::Move {
                    d: MemoryLocation::Stack(destination),
                    s: Operand::Stack(source),
                } => {
                    let destination = destination.index;
                    let source = source.index;

                    let swap = Register::X17.as_32_bit();

                    lines.push(format!("ldr {swap}, [SP, #{source}]"));
                    lines.push(format!("str {swap}, [SP, #{destination}]"));
                }
                // move immediate to stack
                Instruction::Move {
                    d: MemoryLocation::Stack(stack_location),
                    s: Operand::IntConst(n),
                } => {
                    let stack = stack_location.index;

                    let reg = Register::X17.as_32_bit();
                    lines.push(format!("mov {reg}, #{n}"));
                    lines.push(format!("str {reg}, [SP, #{stack}]"));
                }
                // return from function
                Instruction::Return => lines.push("ret".to_string()),
                // binary operations
                Instruction::Binop { d, s1, op, s2 } => {
                    lines.append(&mut format_binop(d, s1, op, s2));
                }
                Instruction::Unop { d, op, s } => {
                    lines.push(format_unop(d, s, op));
                }
                Instruction::SetupStack(n) => {
                    lines.push(format!("sub sp, sp, #{n}"));
                }
                Instruction::CleanupStack(n) => {
                    lines.push(format!("add sp, sp, #{n}"));
                }
            }
        }
        lines
    }
}

fn assign_memory(
    instruction: &abstract_assembly::Instruction,
    memory_map: &HashMap<&Destination, MemoryLocation>,
) -> Vec<Instruction> {
    match instruction {
        abstract_assembly::Instruction::Move { d, s } => {
            vec![Instruction::Move {
                d: memory_map[d],
                s: Operand::convert(s.clone(), memory_map),
            }]
        }
        abstract_assembly::Instruction::Binop { d, s1, op, s2 } => {
            let mut instructions = Vec::new();

            let s1 = match Operand::convert(s1.clone(), memory_map) {
                Operand::Stack(stack_location) => {
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Register(Register::X15),
                        s: Operand::Stack(stack_location),
                    });
                    Register::X15
                }
                Operand::Register(register) => register,
                Operand::IntConst(n) => {
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Register(Register::X15),
                        s: Operand::IntConst(n),
                    });
                    Register::X15
                }
            };

            let s2 = match Operand::convert(s2.clone(), memory_map) {
                Operand::Stack(stack_location) => {
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Register(Register::X16),
                        s: Operand::Stack(stack_location),
                    });
                    Register::X16
                }
                Operand::Register(register) => register,
                Operand::IntConst(n) => {
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Register(Register::X16),
                        s: Operand::IntConst(n),
                    });
                    Register::X16
                }
            };

            // TODO: bug here in modulo conversion to ARM, conversion assumes distinct destination
            // location, while this conversion reuses a register for the destination

            match memory_map[d] {
                MemoryLocation::Stack(stack_location) => {
                    instructions.push(Instruction::Binop {
                        d: Register::X17,
                        s1: Register::X15,
                        op: *op,
                        s2: Register::X16,
                    });
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Stack(stack_location),
                        s: Operand::Register(Register::X17),
                    });
                }
                MemoryLocation::Register(register) => {
                    instructions.push(Instruction::Binop {
                        d: register,
                        s1,
                        op: *op,
                        s2,
                    });
                }
            };

            instructions
        }
        abstract_assembly::Instruction::Unop { d, op, s } => {
            let mut instructions = Vec::new();
            let s = match Operand::convert(s.clone(), memory_map) {
                Operand::Stack(stack_location) => {
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Register(Register::X17),
                        s: Operand::Stack(stack_location),
                    });
                    Register::X17
                }
                Operand::Register(register) => register,
                Operand::IntConst(n) => {
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Register(Register::X17),
                        s: Operand::IntConst(n),
                    });
                    Register::X17
                }
            };

            match memory_map[d] {
                MemoryLocation::Stack(stack_location) => {
                    instructions.push(Instruction::Unop {
                        d: Register::X17,
                        op: *op,
                        s,
                    });
                    instructions.push(Instruction::Move {
                        d: MemoryLocation::Stack(stack_location),
                        s: Operand::Register(Register::X17),
                    });
                }
                MemoryLocation::Register(register) => {
                    instructions.push(Instruction::Unop {
                        d: register,
                        op: *op,
                        s,
                    });
                }
            };

            instructions
        }
        abstract_assembly::Instruction::Return => vec![Instruction::Return],
        abstract_assembly::Instruction::If { .. } => todo!(),
        abstract_assembly::Instruction::Goto(..) => todo!(),
        abstract_assembly::Instruction::Label(..) => todo!(),
    }
}

pub fn specialize(instructions: abstract_assembly::Program) -> Program {
    let memory_map = regalloc(instructions.as_ref(), 0);

    eprintln!("memory map:\n{memory_map}");

    // align stack space
    let stack_space = if memory_map.stack_space % 16 == 0 {
        memory_map.stack_space
    } else {
        (1 + (memory_map.stack_space / 16)) * 16
    };

    let memory_map: HashMap<&Destination, MemoryLocation> = memory_map
        .map
        .into_iter()
        .map(|(d, loc)| (d, loc.into()))
        .collect();

    let mut arm_instrs = Vec::new();

    // TODO: set up stack space
    arm_instrs.push(Instruction::SetupStack(stack_space));

    for i in instructions.as_ref() {
        if *i == abstract_assembly::Instruction::Return {
            // clean up stack space before returning
            arm_instrs.push(Instruction::CleanupStack(stack_space));
        }
        let mut to_add = assign_memory(i, &memory_map);
        arm_instrs.append(&mut to_add);
    }

    Program(arm_instrs)
}

#[cfg(test)]
mod tests {
    use crate::codegen::arm::Program;

    use super::{Instruction, MemoryLocation, Operand, Register};

    #[test]
    fn large_immediate() {
        let i = Program(vec![Instruction::Move {
            d: MemoryLocation::Register(Register::X0),
            s: Operand::IntConst(2147483647),
        }]);

        println!(
            "instruction written as : {}",
            i.lines().into_iter().collect::<String>()
        );
        panic!();
    }
}
