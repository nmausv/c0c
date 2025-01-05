// types for the elaborated AST with scope semantics

use std::collections::VecDeque;

use super::ast;

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
    Exp(Exp),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t, scope) => {
                write!(f, "{{start scope of {t} {var};\n{scope} end scope of {var}}}")
            }
            Self::Assign(var, exp) => write!(f, "{var} = {exp};"),
            Self::Nop => write!(f, "Nop;"),
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
            } => {
                write!(
                f,
                "if ({cond}) {{start if body\n{branch_true}\nend if body}} else {{start else body\n{branch_false}\nend else body}}"
            )
            }
            Self::While { cond, body } => {
                write!(f, "while ({cond}) \n{body}\n")
            }
            Self::Exp(exp) => write!(f, "{exp};"),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Pure(PureBinOp),
    Impure(ImpureBinOp),
}

impl BinOp {
    pub fn from(binop: ast::BinOp) -> Self {
        match binop {
            ast::BinOp::Plus => Self::Pure(PureBinOp::Plus),
            ast::BinOp::Minus => Self::Pure(PureBinOp::Minus),
            ast::BinOp::Times => Self::Pure(PureBinOp::Times),
            ast::BinOp::Divide => Self::Impure(ImpureBinOp::Divide),
            ast::BinOp::Modulo => Self::Impure(ImpureBinOp::Modulo),
            ast::BinOp::Less => Self::Pure(PureBinOp::Less),
            ast::BinOp::LessEq => Self::Pure(PureBinOp::LessEq),
            ast::BinOp::Greater => Self::Pure(PureBinOp::Greater),
            ast::BinOp::GreaterEq => Self::Pure(PureBinOp::GreaterEq),
            ast::BinOp::Eq => Self::Pure(PureBinOp::Eq),
            ast::BinOp::NotEq => Self::Pure(PureBinOp::NotEq),
            ast::BinOp::LogAnd => Self::Pure(PureBinOp::LogAnd),
            ast::BinOp::LogOr => Self::Pure(PureBinOp::LogOr),
            ast::BinOp::BitAnd => Self::Pure(PureBinOp::BitAnd),
            ast::BinOp::BitXor => Self::Pure(PureBinOp::BitXor),
            ast::BinOp::BitOr => Self::Pure(PureBinOp::BitOr),
            ast::BinOp::Shl => Self::Impure(ImpureBinOp::Shl),
            ast::BinOp::Shr => Self::Impure(ImpureBinOp::Shr),
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pure(b) => write!(f, "{b}"),
            Self::Impure(b) => write!(f, "{b}"),
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
    pub fn forget_purity(self) -> ast::BinOp {
        match self {
            Self::Plus => ast::BinOp::Plus,
            Self::Minus => ast::BinOp::Minus,
            Self::Times => ast::BinOp::Times,
            Self::Less => ast::BinOp::Less,
            Self::LessEq => ast::BinOp::LessEq,
            Self::Greater => ast::BinOp::Greater,
            Self::GreaterEq => ast::BinOp::GreaterEq,
            Self::Eq => ast::BinOp::Eq,
            Self::NotEq => ast::BinOp::NotEq,
            Self::LogAnd => ast::BinOp::LogAnd,
            Self::LogOr => ast::BinOp::LogOr,
            Self::BitAnd => ast::BinOp::BitAnd,
            Self::BitXor => ast::BinOp::BitXor,
            Self::BitOr => ast::BinOp::BitOr,
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
    pub fn forget_purity(self) -> ast::BinOp {
        match self {
            Self::Divide => ast::BinOp::Divide,
            Self::Modulo => ast::BinOp::Modulo,
            Self::Shl => ast::BinOp::Shl,
            Self::Shr => ast::BinOp::Shr,
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
