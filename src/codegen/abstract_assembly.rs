use std::rc::Rc;

use crate::frontend::ast::Binop;
use crate::frontend::elab_ast::{PureBinop, Unop};
use crate::heap_recursion::FrameProgress::{self, Done, New};
use crate::temps::{Label, Temp, TempFactory};
use crate::translation::tree::{self, Command, PureExp};

pub struct Program(Vec<Instruction>);

impl IntoIterator for Program {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl AsRef<[Instruction]> for Program {
    fn as_ref(&self) -> &[Instruction] {
        &self.0
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Instruction {
    Move {
        d: Destination,
        s: Operand,
    },
    Binop {
        d: Destination,
        s1: Operand,
        op: Binop,
        s2: Operand,
    },
    Unop {
        d: Destination,
        op: Unop,
        s: Operand,
    },
    Return,
    If {
        left: Operand,
        comp: Binop,
        right: Operand,
        branch_true: Label,
        branch_false: Label,
    },
    Goto(Label),
    Label(Label),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return => write!(f, "return"),
            Self::Binop { d, s1, op, s2 } => {
                write!(f, "{d} <- {s1} {op} {s2}")
            }
            Self::Unop { d, op, s } => {
                write!(f, "{d} <- ({op}{s})")
            }
            Self::Move { d, s } => write!(f, "{d} <- {s}"),
            Self::If {
                left,
                comp,
                right,
                branch_true,
                branch_false,
            } => {
                write!(f, "if ({left} {comp} {right}) then (goto {branch_true}) else (goto {branch_false})")
            }
            Self::Label(l) => write!(f, "{l}:"),
            Self::Goto(l) => write!(f, "goto {l}"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Destination {
    Register(Register),
    Temp(Temp),
}

impl From<Register> for Destination {
    fn from(value: Register) -> Self {
        Destination::Register(value)
    }
}

impl From<Temp> for Destination {
    fn from(value: Temp) -> Self {
        Destination::Temp(value)
    }
}

impl TryFrom<Operand> for Destination {
    type Error = ();
    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        match value {
            Operand::Register(register) => Ok(Destination::Register(register)),
            Operand::IntConst(_) => Err(()),
            Operand::Temp(temp) => Ok(Destination::Temp(temp)),
        }
    }
}

impl std::fmt::Display for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Destination::Register(register) => write!(f, "{register}"),
            Destination::Temp(temp) => write!(f, "{temp}"),
        }
    }
}

/// Note that the `Return` register pulls double duty:
///     1. standard return register
///     2. stores the quotient of division/modulo operations
///
/// Additionally, the `Remainder` register stores the
/// remainder of division/modulo operations.
#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum Register {
    Return,
    Remainder,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Return => write!(f, "r_ret"),
            Register::Remainder => write!(f, "r_rem"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Operand {
    Register(Register),
    IntConst(i32),
    Temp(Temp),
}

impl From<Destination> for Operand {
    fn from(value: Destination) -> Self {
        match value {
            Destination::Register(reg) => Operand::Register(reg),
            Destination::Temp(temp) => Operand::Temp(temp),
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntConst(n) => write!(f, "{n}"),
            Self::Temp(t) => write!(f, "{t}"),
            Self::Register(reg) => write!(f, "{reg}"),
        }
    }
}

enum ExpFrame {
    Num {
        dest: Destination,
        n: i32,
    },
    Ident {
        dest: Destination,
        var: Rc<str>,
    },
    PureBinop {
        dest: Destination,
        left: FrameProgress<tree::PureExp, Operand>,
        op: PureBinop,
        right: FrameProgress<tree::PureExp, Operand>,
    },
    Unop {
        dest: Destination,
        op: Unop,
        exp: FrameProgress<tree::PureExp, Operand>,
    },
}

impl ExpFrame {
    fn new(exp: tree::PureExp, dest: Destination) -> Self {
        match exp {
            PureExp::Num(n) => Self::Num { dest, n },
            PureExp::Ident(var) => Self::Ident { dest, var },
            PureExp::PureBinop(left, op, right) => Self::PureBinop {
                dest,
                left: New(*left),
                op,
                right: New(*right),
            },
            PureExp::Unop(op, exp) => Self::Unop {
                dest,
                op,
                exp: New(*exp),
            },
        }
    }
}

impl tree::PureExp {
    fn cogen_recursive(
        self,
        dest: Destination,
        tf: &mut TempFactory,
    ) -> Program {
        match self {
            PureExp::Num(n) => Program(vec![Instruction::Move {
                d: dest,
                s: Operand::IntConst(n),
            }]),
            PureExp::Ident(x) => Program(vec![Instruction::Move {
                d: dest,
                s: Operand::Temp(x.into()),
            }]),
            PureExp::PureBinop(e1, op, e2) => {
                let t1 = Destination::Temp(tf.make_temp());
                let t2 = Destination::Temp(tf.make_temp());
                let Program(mut first) = e1.cogen_recursive(t1.clone(), tf);
                let Program(mut second) = e2.cogen_recursive(t2.clone(), tf);
                first.append(&mut second);
                first.push(Instruction::Binop {
                    d: dest,
                    s1: t1.into(),
                    op: crate::frontend::ast::Binop::from(op),
                    s2: t2.into(),
                });
                Program(first)
            }
            PureExp::Unop(op, exp) => {
                let t1 = Destination::Temp(tf.make_temp());
                let Program(mut instructions) =
                    exp.cogen_recursive(t1.clone(), tf);
                instructions.push(Instruction::Unop {
                    d: dest,
                    op,
                    s: t1.into(),
                });

                Program(instructions)
            }
        }
    }

    fn cogen_iterative(
        self,
        dest: Destination,
        tf: &mut TempFactory,
    ) -> Vec<Instruction> {
        let mut program = Vec::new();
        let mut stack = Vec::new();

        stack.push(ExpFrame::new(self, dest));

        while let Some(frame) = stack.pop() {
            match frame {
                ExpFrame::Num { dest, n } => {
                    program.push(Instruction::Move {
                        d: dest,
                        s: Operand::IntConst(n),
                    });
                }
                ExpFrame::Ident { dest, var } => {
                    program.push(Instruction::Move {
                        d: dest,
                        s: Operand::Temp(var.into()),
                    });
                }
                ExpFrame::PureBinop {
                    dest,
                    left,
                    op,
                    right,
                } => match (left, right) {
                    (New(left), New(right)) => {
                        let t1 = Destination::Temp(tf.make_temp());
                        stack.push(ExpFrame::PureBinop {
                            dest,
                            left: Done(t1.clone().into()),
                            op,
                            right: New(right),
                        });
                        stack.push(ExpFrame::new(left, t1));
                    }
                    (Done(t1), New(right)) => {
                        let t2 = Destination::Temp(tf.make_temp());
                        stack.push(ExpFrame::PureBinop {
                            dest,
                            left: Done(t1),
                            op,
                            right: Done(t2.clone().into()),
                        });
                        stack.push(ExpFrame::new(right, t2));
                    }
                    (Done(t1), Done(t2)) => {
                        program.push(Instruction::Binop {
                            d: dest,
                            s1: t1,
                            op: op.into(),
                            s2: t2,
                        });
                    }
                    _ => unreachable!(),
                },
                ExpFrame::Unop { dest, op, exp } => match exp {
                    New(exp) => {
                        let t = Destination::Temp(tf.make_temp());
                        stack.push(ExpFrame::Unop {
                            dest,
                            op,
                            exp: Done(t.clone().into()),
                        });
                        stack.push(ExpFrame::new(exp, t));
                    }
                    Done(t) => {
                        program.push(Instruction::Unop { d: dest, op, s: t });
                    }
                    _ => unreachable!(),
                },
            }
        }

        program
    }

    fn cogen(
        self,
        dest: Destination,
        tf: &mut TempFactory,
    ) -> Vec<Instruction> {
        self.cogen_iterative(dest, tf)
    }
}

fn cogen_command(
    command: Command,
    tf: &mut crate::temps::TempFactory,
) -> Vec<Instruction> {
    match command {
        Command::Return(e) => {
            let return_register = Destination::Register(Register::Return);
            let mut program = e.cogen(return_register, tf);
            program.push(Instruction::Return);
            program
        }
        Command::Store(var, e) => e.cogen(Destination::Temp(var.into()), tf),
        Command::StoreImpureBinop {
            dest,
            left,
            op,
            right,
        } => {
            let t1 = Destination::Temp(tf.make_temp());
            let t2 = Destination::Temp(tf.make_temp());
            let mut first = left.cogen(t1.clone(), tf);
            let mut second = right.cogen(t2.clone(), tf);
            first.append(&mut second);
            first.push(Instruction::Binop {
                d: Destination::Temp(dest.into()),
                s1: t1.into(),
                op: op.into(),
                s2: t2.into(),
            });

            first
        }
        Command::If {
            left,
            comp,
            right,
            branch_true,
            branch_false,
        } => {
            // compute left, right
            let t1 = Destination::Temp(tf.make_temp());
            let t2 = Destination::Temp(tf.make_temp());

            let mut left_instr = left.cogen(t1.clone(), tf);
            let mut right_instr = right.cogen(t2.clone(), tf);

            left_instr.append(&mut right_instr);

            left_instr.push(Instruction::If {
                left: t1.into(),
                comp: comp.into(),
                right: t2.into(),
                branch_true,
                branch_false,
            });

            left_instr
        }
        Command::Goto(l) => vec![Instruction::Goto(l)],
        Command::Label(l) => vec![Instruction::Label(l)],
    }
}

pub fn ir_to_abstract(ir: tree::Program, tf: &mut TempFactory) -> Program {
    Program(
        ir.into_iter()
            .flat_map(|command| cogen_command(command, tf))
            .collect(),
    )
}

#[cfg(test)]
mod abs_asm_tests {

    use std::collections::HashMap;

    use crate::{codegen::abstract_assembly::ir_to_abstract, temps::Label};

    use super::{Destination, Instruction, Operand, Register};

    /// stores the instruction number that caused the error
    #[derive(Debug, PartialEq, Eq)]
    enum SimulationError {
        MoveIntoConst(usize),
        UninitializedVariable(usize),
        UnknownLabel(Label),
    }

    fn get<'a, 'b>(
        state: &'a HashMap<Destination, i32>,
        d: &'b Operand,
    ) -> Option<i32>
    where
        'a: 'b,
    {
        match d {
            Operand::Register(register) => {
                state.get(&Destination::Register(*register)).copied()
            }
            Operand::Temp(temp) => {
                state.get(&Destination::Temp(temp.clone())).copied()
            }
            Operand::IntConst(n) => Some(*n),
        }
    }

    /// returns Ok(Some(n)) if the instruction returns n, Ok(None) if the
    /// instruction executed correctly but did not return anything
    /// Err(..) if the instruction could not execute
    fn execute_abs_instruction(
        state: &mut HashMap<Destination, i32>,
        program: &[Instruction],
        ip: &mut usize,
    ) -> Result<Option<i32>, SimulationError> {
        match &program[*ip] {
            Instruction::Move { d, s } => {
                let stored_s = match get(state, s) {
                    Some(n) => n,
                    None => {
                        return Err(SimulationError::UninitializedVariable(*ip))
                    }
                };
                state.insert(d.clone(), stored_s);
                Ok(None)
            }
            Instruction::Binop { d, s1, op, s2 } => {
                let stored_s1 = match get(state, s1) {
                    Some(n) => n,
                    None => {
                        return Err(SimulationError::UninitializedVariable(*ip))
                    }
                };
                let stored_s2 = match get(state, s2) {
                    Some(n) => n,
                    None => {
                        return Err(SimulationError::UninitializedVariable(*ip))
                    }
                };
                let result = match op {
                    crate::frontend::ast::Binop::Plus => stored_s1 + stored_s2,
                    crate::frontend::ast::Binop::Minus => stored_s1 - stored_s2,
                    crate::frontend::ast::Binop::Times => stored_s1 * stored_s2,
                    crate::frontend::ast::Binop::Divide => {
                        stored_s1 / stored_s2
                    }
                    crate::frontend::ast::Binop::Modulo => {
                        stored_s1 % stored_s2
                    }
                    _ => todo!("abstract assembly simulation: binops"),
                };

                state.insert(d.clone(), result);
                Ok(None)
            }
            Instruction::Return => Ok(Some(
                *state
                    .get(&Destination::Register(Register::Return))
                    .expect("program state should have the given value mapped"),
            )),
            Instruction::Unop { .. } => todo!(),
            Instruction::If {
                left,
                comp,
                right,
                branch_true,
                branch_false,
            } => {
                let left = match get(state, left) {
                    Some(val) => val,
                    None => {
                        return Err(SimulationError::UninitializedVariable(*ip))
                    }
                };
                let right = match get(state, right) {
                    Some(val) => val,
                    None => {
                        return Err(SimulationError::UninitializedVariable(*ip))
                    }
                };
                let branch_true = match program
                    .iter()
                    .position(|i| *i == Instruction::Label(branch_true.clone()))
                {
                    Some(ind) => ind,
                    None => {
                        return Err(SimulationError::UnknownLabel(
                            branch_true.clone(),
                        ));
                    }
                };
                let branch_false = match program.iter().position(|i| {
                    *i == Instruction::Label(branch_false.clone())
                }) {
                    Some(ind) => ind,
                    None => {
                        return Err(SimulationError::UnknownLabel(
                            branch_false.clone(),
                        ));
                    }
                };
                // check condition satisfied
                let sat = match comp {
                    crate::frontend::ast::Binop::Less => left < right,
                    crate::frontend::ast::Binop::LessEq => left <= right,
                    crate::frontend::ast::Binop::Greater => left > right,
                    crate::frontend::ast::Binop::GreaterEq => left >= right,
                    crate::frontend::ast::Binop::Eq => left == right,
                    crate::frontend::ast::Binop::NotEq => left != right,
                    crate::frontend::ast::Binop::LogAnd => todo!(),
                    crate::frontend::ast::Binop::LogOr => todo!(),
                    _ => panic!(),
                };

                if sat {
                    *ip = branch_true;
                } else {
                    *ip = branch_false;
                }

                Ok(None)
            }
            Instruction::Goto(label) => {
                // find label in instructions
                match program
                    .iter()
                    .position(|i| *i == Instruction::Label(label.clone()))
                {
                    Some(ind) => {
                        *ip = ind;
                        Ok(None)
                    }
                    None => Err(SimulationError::UnknownLabel(label.clone())),
                }
            }
            Instruction::Label(_) => Ok(None),
        }
    }

    /// Executes the abstract assembly and returns the output (if any).
    /// Typechecker ensures all variables initialized before being used.
    fn abs_asm_runner(
        program: &[Instruction],
    ) -> Result<Option<i32>, SimulationError> {
        let mut state: HashMap<_, _> = HashMap::new();
        let mut ip = 0;

        loop {
            if ip >= program.len() {
                break;
            }
            let result = execute_abs_instruction(&mut state, program, &mut ip)?;
            if let Some(n) = result {
                return Ok(Some(n));
            };
            ip += 1;
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

        let t1 = Destination::Temp(String::from("t1").into());
        let t2 = Destination::Temp(String::from("t2").into());
        let t3 = Destination::Temp(String::from("t3").into());
        let t4 = Destination::Temp(String::from("t4").into());
        let program = vec![
            Instruction::Binop {
                d: t1.clone(),
                s1: Operand::IntConst(3),
                op: crate::frontend::ast::Binop::Plus,
                s2: Operand::IntConst(8),
            },
            Instruction::Binop {
                d: t2.clone(),
                s1: t1.clone().into(),
                op: crate::frontend::ast::Binop::Minus,
                s2: Operand::IntConst(5),
            },
            Instruction::Binop {
                d: t3.clone(),
                s1: t2.clone().into(),
                op: crate::frontend::ast::Binop::Times,
                s2: Operand::IntConst(6),
            },
            Instruction::Binop {
                d: t4.clone(),
                s1: t3.clone().into(),
                op: crate::frontend::ast::Binop::Modulo,
                s2: Operand::IntConst(5),
            },
            Instruction::Move {
                d: Destination::Register(Register::Return),
                s: t4.clone().into(),
            },
            Instruction::Return,
        ];
        let result = abs_asm_runner(&program);
        dbg!(&result);
        assert_eq!(result, Ok(Some(1)));
    }

    #[test]
    fn end_to_end() {
        use crate::frontend::c0parser::ProgramParser;
        use crate::frontend::elaboration;
        use crate::frontend::lexer::Lexer;
        use crate::temps::TempFactory;
        use crate::translation;

        let program = "int main() {bool x = false; bool y = true; bool z = x ? x && y : x || y; return z ? 1 : 0;}";
        let lexer = Lexer::new_c0c_lexer(program);
        let parser = ProgramParser::new();
        let ast = parser.parse(program, lexer).unwrap();
        let elab = elaboration::elaborate(ast).unwrap();
        let mut tf = TempFactory::new();
        let ir = translation::translate(elab, &mut tf);
        let commands = ir_to_abstract(ir, &mut tf);

        match abs_asm_runner(commands.as_ref()) {
            Ok(Some(1)) => (),
            _ => panic!("unexpected simulation result"),
        }
    }
}
