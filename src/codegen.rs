use crate::translation::tree::{Command, PureExp};

pub enum Target {
    AbstractAssembly,
    ARM,
    LLVMIR,
}

mod abstract_assembly {
    pub type Program = Vec<Instruction>;
    #[derive(PartialEq, Eq, Clone, Debug)]
    pub enum Instruction {
        Move {
            d: Destination,
            s: Source,
        },
        Binop {
            d: Destination,
            s1: Source,
            op: crate::frontend::ast::BinOp,
            s2: Source,
        },
        Return,
    }

    impl std::fmt::Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Return => write!(f, "return"),
                Self::Binop { d, s1, op, s2 } => {
                    write!(f, "{d} <- {s1} {op} {s2}")
                }
                Self::Move { d, s } => write!(f, "{d} <- {s}"),
            }
        }
    }

    pub type Destination = Operand;
    pub type Source = Operand;
    #[derive(PartialEq, Eq, Clone, Debug)]
    pub enum Operand {
        Register(String),
        IntConst(i32),
        Temp(String),
    }

    impl std::fmt::Display for Operand {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::IntConst(n) => write!(f, "{n}"),
                Self::Temp(t) => write!(f, "{t}"),
                Self::Register(r) => write!(f, "{r}"),
            }
        }
    }
}

fn ir_to_abstract(
    ir: crate::translation::tree::Program,
    tf: &mut crate::temps::TempFactory,
) -> abstract_assembly::Program {
    use abstract_assembly::{Destination, Instruction, Operand, Program};

    fn cogen_exp(
        dest: Destination,
        exp: PureExp,
        tf: &mut crate::temps::TempFactory,
    ) -> Program {
        match exp {
            PureExp::Num(n) => {
                vec![Instruction::Move {
                    d: dest,
                    s: Operand::IntConst(n),
                }]
            }
            PureExp::Ident(x) => {
                vec![Instruction::Move {
                    d: dest,
                    s: Operand::Temp(x),
                }]
            }
            PureExp::PureBinOp(e1, op, e2) => {
                let t1 = Operand::Temp(tf.make_temp());
                let t2 = Operand::Temp(tf.make_temp());
                let mut first = cogen_exp(t1.clone(), *e1, tf);
                let mut second = cogen_exp(t2.clone(), *e2, tf);
                first.append(&mut second);
                first.push(Instruction::Binop {
                    d: dest,
                    s1: t1,
                    op: op.forget_purity(),
                    s2: t2,
                });
                first
            }
        }
    }

    fn cogen_command(
        command: Command,
        tf: &mut crate::temps::TempFactory,
    ) -> Vec<Instruction> {
        match command {
            Command::Return(e) => {
                let return_register = Operand::Register(String::from("r_ret"));
                let mut program = cogen_exp(return_register, e, tf);
                program.push(Instruction::Return);
                program
            }
            Command::Store(var, e) => cogen_exp(Operand::Temp(var), e, tf),
            Command::StoreImpureBinOp(var, e1, op, e2) => {
                let t1 = Operand::Temp(tf.make_temp());
                let t2 = Operand::Temp(tf.make_temp());
                let mut first = cogen_exp(t1.clone(), e1, tf);
                let mut second = cogen_exp(t2.clone(), e2, tf);
                first.append(&mut second);
                first.push(Instruction::Binop {
                    d: Operand::Temp(var),
                    s1: t1,
                    op: op.forget_purity(),
                    s2: t2,
                });
                first
            }
        }
    }

    ir.0.into_iter()
        .flat_map(|command| cogen_command(command, tf))
        .collect()
}

pub fn codegen(
    ir: crate::translation::tree::Program,
    target: Target,
    tf: &mut crate::temps::TempFactory,
) -> String {
    match target {
        Target::AbstractAssembly => {
            let instructions = ir_to_abstract(ir, tf);
            instructions.into_iter().map(|i| format!("{i}\n")).collect()
        }
        Target::ARM => todo!(),
        Target::LLVMIR => todo!(),
    }
}
