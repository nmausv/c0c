use crate::frontend::elab_ast::{Ident, ImpureBinOp, PureBinOp};

pub struct Program(pub Vec<Command>);

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().map(|s| write!(f, "{s}\n")).collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    Store(Ident, PureExp),
    StoreImpureBinOp(Ident, PureExp, ImpureBinOp, PureExp),
    Return(PureExp),
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Store(var, exp) => write!(f, "{var} <- {exp}"),
            Self::StoreImpureBinOp(var, e1, op, e2) => {
                write!(f, "{var} <- {e1} {op} {e2}")
            }
            Self::Return(exp) => write!(f, "return {exp}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PureExp {
    Num(i32),
    Ident(String),
    PureBinOp(Box<PureExp>, PureBinOp, Box<PureExp>),
}

impl std::fmt::Display for PureExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Ident(var) => write!(f, "{var}"),
            Self::PureBinOp(e1, op, e2) => write!(f, "{e1} {op} {e2}"),
        }
    }
}
