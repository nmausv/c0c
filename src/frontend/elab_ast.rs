// types for the elaborated AST with scope semantics

use std::collections::VecDeque;

use crate::frontend::ast::BinOp;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Declare(Ident, Type, Box<Stmt>),
    Assign(Ident, Exp),
    Return(Exp),
    Seq(VecDeque<Stmt>),
    Nop,
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t, scope) => {
                write!(f, "{{{t} {var};\n{scope}}}")
            }
            Self::Assign(var, exp) => write!(f, "{var} = {exp};"),
            Self::Nop => write!(f, ""),
            Self::Return(exp) => write!(f, "return {exp};"),
            Self::Seq(v) => {
                write!(f, "{{")?;
                let _ =
                    v.iter().map(|s| write!(f, "{s}\n")).collect::<Vec<_>>();
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Num(i32),
    Ident(String),
    PureBinop(Box<Exp>, PureBinOp, Box<Exp>),
    ImpureBinop(Box<Exp>, ImpureBinOp, Box<Exp>),
}

impl std::fmt::Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Ident(var) => write!(f, "{var}"),
            Self::PureBinop(e1, op, e2) => write!(f, "{e1} {op} {e2}"),
            Self::ImpureBinop(e1, op, e2) => write!(f, "{e1} {op} {e2}"),
        }
    }
}

// no effects
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PureBinOp {
    Plus,
    Minus,
    Times,
}

impl PureBinOp {
    pub fn forget_purity(self) -> BinOp {
        match self {
            Self::Plus => BinOp::Plus,
            Self::Minus => BinOp::Minus,
            Self::Times => BinOp::Times,
        }
    }
}

impl std::fmt::Display for PureBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Times => write!(f, "*"),
        }
    }
}

// can raise an exception
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImpureBinOp {
    Divide, // divide by zero
    Modulo,
}

impl ImpureBinOp {
    pub fn forget_purity(self) -> BinOp {
        match self {
            Self::Divide => BinOp::Divide,
            Self::Modulo => BinOp::Modulo,
        }
    }
}

impl std::fmt::Display for ImpureBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
        }
    }
}

pub type Ident = super::ast::Ident;
pub type Type = super::ast::Type;
