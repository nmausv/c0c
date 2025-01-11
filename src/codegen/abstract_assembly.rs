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
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
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

use crate::translation::tree::{Command, PureExp};

pub fn ir_to_abstract(
    ir: crate::translation::tree::Program,
    tf: &mut crate::temps::TempFactory,
) -> Program {
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
                    op: crate::frontend::ast::BinOp::from(op),
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
                    op: crate::frontend::ast::BinOp::from(op),
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

#[cfg(test)]
mod abs_asm_tests {

    use std::collections::HashMap;

    use super::{Instruction, Operand};

    /// stores the instruction number that caused the error
    #[derive(Debug, PartialEq, Eq)]
    enum SimulationError {
        MoveIntoConst(usize),
        UninitializedVariable(usize),
    }

    /// returns Ok(Some(n)) if the instruction returns n, Ok(None) if the
    /// instruction executed correctly but did not return anything
    fn execute_abs_instruction(
        state: &mut HashMap<Operand, i32>,
        instruction: &Instruction,
        index: usize,
    ) -> Result<Option<i32>, SimulationError> {
        match instruction {
            Instruction::Move {
                d: Operand::IntConst(_),
                s: _,
            } => return Err(SimulationError::MoveIntoConst(index)),
            Instruction::Move {
                d,
                s: Operand::IntConst(n),
            } => {
                state.insert(d.clone(), *n);
                Ok(None)
            }
            Instruction::Move { d, s } => {
                let stored_s = match state.get(s) {
                    Some(n) => *n,
                    None => {
                        return Err(SimulationError::UninitializedVariable(
                            index,
                        ))
                    }
                };
                state.insert(d.clone(), stored_s);
                Ok(None)
            }
            Instruction::Binop { d, s1, op, s2 } => {
                let stored_s1 = match s1 {
                    Operand::IntConst(n) => *n,
                    _ => match state.get(s1) {
                        Some(n) => *n,
                        None => {
                            return Err(SimulationError::UninitializedVariable(
                                index,
                            ))
                        }
                    },
                };
                let stored_s2 = match s2 {
                    Operand::IntConst(n) => *n,
                    _ => match state.get(s2) {
                        Some(n) => *n,
                        None => {
                            return Err(SimulationError::UninitializedVariable(
                                index,
                            ))
                        }
                    },
                };
                let result = match op {
                    crate::frontend::ast::BinOp::Plus => stored_s1 + stored_s2,
                    crate::frontend::ast::BinOp::Minus => stored_s1 - stored_s2,
                    crate::frontend::ast::BinOp::Times => stored_s1 * stored_s2,
                    crate::frontend::ast::BinOp::Divide => {
                        stored_s1 / stored_s2
                    }
                    crate::frontend::ast::BinOp::Modulo => {
                        stored_s1 % stored_s2
                    }
                };

                state.insert(d.clone(), result);
                Ok(None)
            }
            Instruction::Return => {
                return Ok(Some(
                    *state
                        .get(&Operand::Register(String::from("r_ret")))
                        .unwrap(),
                ))
            }
        }
    }

    /// Executes the abstract assembly and returns the output (if any).
    /// Typechecker ensures all variables initialized before being used.
    fn abs_asm_runner(
        program: &Vec<Instruction>,
    ) -> Result<Option<i32>, SimulationError> {
        let mut state: HashMap<Operand, i32> = HashMap::new();

        for (index, instruction) in program.iter().enumerate() {
            dbg!(instruction);
            dbg!(&state);
            match execute_abs_instruction(&mut state, instruction, index) {
                Ok(Some(n)) => return Ok(Some(n)),
                _ => (),
            };
        }

        Ok(None)
    }

    #[test]
    fn empty_program() {
        let program = vec![];
        assert_eq!(abs_asm_runner(&program), Ok(None));
    }

    #[test]
    fn arithmetic_program() {
        // t1 <- 3 + 8      = 11
        // t2 <- t1 - 5     = 6
        // t3 <- t2 * 6     = 36
        // t4 <- t3 % 5     = 1
        // r_ret <- t4      = 1
        // return

        let t1 = Operand::Temp(String::from("t1"));
        let t2 = Operand::Temp(String::from("t2"));
        let t3 = Operand::Temp(String::from("t3"));
        let t4 = Operand::Temp(String::from("t4"));
        let program = vec![
            Instruction::Binop {
                d: t1.clone(),
                s1: Operand::IntConst(3),
                op: crate::frontend::ast::BinOp::Plus,
                s2: Operand::IntConst(8),
            },
            Instruction::Binop {
                d: t2.clone(),
                s1: t1.clone(),
                op: crate::frontend::ast::BinOp::Minus,
                s2: Operand::IntConst(5),
            },
            Instruction::Binop {
                d: t3.clone(),
                s1: t2.clone(),
                op: crate::frontend::ast::BinOp::Times,
                s2: Operand::IntConst(6),
            },
            Instruction::Binop {
                d: t4.clone(),
                s1: t3.clone(),
                op: crate::frontend::ast::BinOp::Modulo,
                s2: Operand::IntConst(5),
            },
            Instruction::Move {
                d: Operand::Register(String::from("r_ret")),
                s: t4.clone(),
            },
            Instruction::Return,
        ];
        let result = abs_asm_runner(&program);
        dbg!(&result);
        assert_eq!(result, Ok(Some(1)));
    }
}
