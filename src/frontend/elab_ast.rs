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
    If {
        cond: Exp,
        branch_true: Box<Stmt>,
        branch_false: Box<Stmt>,
    },
    While {
        cond: Exp,
        body: Box<Stmt>,
    },
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
            Self::If {
                cond,
                branch_true,
                branch_false,
            } => write!(
                f,
                "if ({cond}) {{\n{branch_true}\n}} else {{\n{branch_false}\n}}"
            ),
            Self::While { cond, body } => {
                write!(f, "while ({cond}) {{\n{body}\n}}")
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
    UnOp(UnOp, Box<Exp>),
    True,
    False,
    Ternary {
        cond: Box<Exp>,
        branch_true: Box<Exp>,
        branch_false: Box<Exp>,
    },
}

impl std::fmt::Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Ident(var) => write!(f, "{var}"),
            Self::PureBinop(e1, op, e2) => write!(f, "({e1} {op} {e2})"),
            Self::ImpureBinop(e1, op, e2) => write!(f, "({e1} {op} {e2})"),
            Self::UnOp(op, e) => write!(f, "{op}({e})"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Ternary {
                cond,
                branch_true,
                branch_false,
            } => write!(f, "{cond} ? {branch_true} : {branch_false}"),
        }
    }
}

// no effects
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PureBinOp {
    Plus,
    Minus,
    Times,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
    LogAnd,
    LogOr,
    BitAnd,
    BitXor,
    BitOr,
}

impl PureBinOp {
    pub fn forget_purity(self) -> BinOp {
        match self {
            Self::Plus => BinOp::Plus,
            Self::Minus => BinOp::Minus,
            Self::Times => BinOp::Times,
            Self::Less => BinOp::Less,
            Self::LessEq => BinOp::LessEq,
            Self::Greater => BinOp::Greater,
            Self::GreaterEq => BinOp::GreaterEq,
            Self::Eq => BinOp::Eq,
            Self::NotEq => BinOp::NotEq,
            Self::LogAnd => BinOp::LogAnd,
            Self::LogOr => BinOp::LogOr,
            Self::BitAnd => BinOp::BitAnd,
            Self::BitXor => BinOp::BitXor,
            Self::BitOr => BinOp::BitOr,
        }
    }
}

impl std::fmt::Display for PureBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = self.forget_purity();
        write!(f, "{op}")
    }
}

// UnOps always have no effects, can reuse the ast definition
pub type UnOp = super::ast::UnOp;

// can raise an exception
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImpureBinOp {
    Divide, // divide by zero
    Modulo,
    Shl,
    Shr,
}

impl ImpureBinOp {
    pub fn forget_purity(self) -> BinOp {
        match self {
            Self::Divide => BinOp::Divide,
            Self::Modulo => BinOp::Modulo,
            Self::Shl => BinOp::Shl,
            Self::Shr => BinOp::Shr,
        }
    }
}

impl std::fmt::Display for ImpureBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = self.forget_purity();
        write!(f, "{op}")
    }
}

pub type Ident = super::ast::Ident;
pub type Type = super::ast::Type;
