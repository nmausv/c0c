use crate::frontend::elab_ast::{Binop, ImpureBinop, PureBinop, Unop};

use crate::temps::Label;

pub struct Program(Vec<Command>);

impl IntoIterator for Program {
    type Item = Command;
    type IntoIter = std::vec::IntoIter<Command>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<Command>> for Program {
    fn from(value: Vec<Command>) -> Self {
        Self(value)
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().try_for_each(|s| match s {
            Command::Label(_) => write!(f, "{s} "),
            _ => writeln!(f, "{s}"),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    Store(String, PureExp),
    StoreImpureBinop {
        dest: String,
        left: PureExp,
        op: ImpureBinop,
        right: PureExp,
    },
    Return(PureExp),
    If {
        left: PureExp,
        comp: Binop,
        right: PureExp,
        branch_true: Label,
        branch_false: Label,
    },
    Goto(Label),
    Label(Label),
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Store(var, exp) => write!(f, "{var} <- {exp}"),
            Self::StoreImpureBinop {
                dest,
                left,
                op,
                right,
            } => {
                write!(f, "{dest} <- {left} {op} {right}")
            }
            Self::Return(exp) => write!(f, "return {exp}"),
            Self::If {
                left, comp, right, branch_true, branch_false
            }  => write!(f, "if ({left} {comp} {right}) then {branch_true} else {branch_false}"),
            Self::Goto(label) => write!(f, "goto {label}"),
            Self::Label(label) => write!(f, "{label}:"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PureExp {
    Num(i32),
    Ident(String),
    PureBinop(Box<PureExp>, PureBinop, Box<PureExp>),
    Unop(Unop, Box<PureExp>),
}

impl std::fmt::Display for PureExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Ident(var) => write!(f, "{var}"),
            Self::PureBinop(e1, op, e2) => write!(f, "{e1} {op} {e2}"),
            Self::Unop(op, exp) => write!(f, "{op}({exp})"),
        }
    }
}
