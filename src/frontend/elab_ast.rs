// types for the elaborated AST with scope semantics

use std::collections::VecDeque;

use super::ast;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program(Stmt);

impl From<Stmt> for Program {
    fn from(value: Stmt) -> Self {
        Self(value)
    }
}

impl AsRef<Stmt> for Program {
    fn as_ref(&self) -> &Stmt {
        &self.0
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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

impl From<Program> for Stmt {
    fn from(value: Program) -> Self {
        value.0
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t, scope) => {
                write!(
                    f,
                    "{{start scope of {t} {var};\n{scope} end scope of {var}}}"
                )
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OpType {
    Relational,
    Equality,
    Logical,
    Arithmetic,
}

impl BinOp {
    pub fn signature(self) -> OpType {
        match self {
            Self::Pure(PureBinOp::Plus) => OpType::Arithmetic,
            Self::Pure(PureBinOp::Minus) => OpType::Arithmetic,
            Self::Pure(PureBinOp::Times) => OpType::Arithmetic,
            Self::Impure(ImpureBinOp::Divide) => OpType::Arithmetic,
            Self::Impure(ImpureBinOp::Modulo) => OpType::Arithmetic,
            Self::Pure(PureBinOp::Less) => OpType::Relational,
            Self::Pure(PureBinOp::LessEq) => OpType::Relational,
            Self::Pure(PureBinOp::Greater) => OpType::Relational,
            Self::Pure(PureBinOp::GreaterEq) => OpType::Relational,
            Self::Pure(PureBinOp::Eq) => OpType::Equality,
            Self::Pure(PureBinOp::NotEq) => OpType::Equality,
            Self::Pure(PureBinOp::LogAnd) => OpType::Logical,
            Self::Pure(PureBinOp::LogOr) => OpType::Logical,
            Self::Pure(PureBinOp::BitAnd) => OpType::Arithmetic,
            Self::Pure(PureBinOp::BitXor) => OpType::Arithmetic,
            Self::Pure(PureBinOp::BitOr) => OpType::Arithmetic,
            Self::Impure(ImpureBinOp::Shl) => OpType::Arithmetic,
            Self::Impure(ImpureBinOp::Shr) => OpType::Arithmetic,
        }
    }
}

impl From<ImpureBinOp> for BinOp {
    fn from(value: ImpureBinOp) -> Self {
        Self::Impure(value)
    }
}

impl From<ast::BinOp> for BinOp {
    fn from(binop: ast::BinOp) -> Self {
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

impl From<PureBinOp> for ast::BinOp {
    fn from(value: PureBinOp) -> Self {
        match value {
            PureBinOp::Plus => Self::Plus,
            PureBinOp::Minus => Self::Minus,
            PureBinOp::Times => Self::Times,
            PureBinOp::Less => Self::Less,
            PureBinOp::LessEq => Self::LessEq,
            PureBinOp::Greater => Self::Greater,
            PureBinOp::GreaterEq => Self::GreaterEq,
            PureBinOp::Eq => Self::Eq,
            PureBinOp::NotEq => Self::NotEq,
            PureBinOp::LogAnd => Self::LogAnd,
            PureBinOp::LogOr => Self::LogOr,
            PureBinOp::BitAnd => Self::BitAnd,
            PureBinOp::BitXor => Self::BitXor,
            PureBinOp::BitOr => Self::BitOr,
        }
    }
}

impl From<PureBinOp> for BinOp {
    fn from(value: PureBinOp) -> Self {
        Self::Pure(value)
    }
}

impl PureBinOp {
    pub fn signature(self) -> OpType {
        BinOp::signature(self.into())
    }
}

impl std::fmt::Display for PureBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ast::BinOp::from(*self))
    }
}

// UnOps always have no effects, can reuse the ast definition
pub type UnOp = super::ast::UnOp;

impl UnOp {
    pub fn signature(self) -> OpType {
        match self {
            Self::Negative => OpType::Arithmetic,
            Self::LogNegate => OpType::Logical,
            Self::BitNegate => OpType::Arithmetic,
        }
    }
}

// can raise an exception
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImpureBinOp {
    Divide, // divide by zero
    Modulo,
    Shl,
    Shr,
}

impl From<ImpureBinOp> for ast::BinOp {
    fn from(value: ImpureBinOp) -> Self {
        match value {
            ImpureBinOp::Divide => Self::Divide,
            ImpureBinOp::Modulo => Self::Modulo,
            ImpureBinOp::Shl => Self::Shl,
            ImpureBinOp::Shr => Self::Shr,
        }
    }
}

impl ImpureBinOp {
    pub fn signature(self) -> OpType {
        BinOp::signature(self.into())
    }
}

impl std::fmt::Display for ImpureBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ast::BinOp::from(*self))
    }
}

pub type Ident = super::ast::Ident;
pub type Type = super::ast::Type;
