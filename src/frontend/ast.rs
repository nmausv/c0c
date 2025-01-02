// top level

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub name: String,
    pub body: Vec<Stmt>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} defined as {{\n", self.name)?;
        let _ = self
            .body
            .iter()
            .map(|s| write!(f, "{s}\n"))
            .collect::<Vec<_>>();
        write!(f, "}} end of {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Declare(Ident, Type),
    DeclareAssign(Ident, Type, Exp),
    Assign(Ident, AsnOp, Exp),
    Block(Vec<Stmt>),
    Return(Exp),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t) => write!(f, "{t} {var};"),
            Self::DeclareAssign(var, t, exp) => write!(f, "{t} {var} = {exp};"),
            Self::Assign(var, op, exp) => write!(f, "{var} {op} {exp};"),
            Self::Return(exp) => write!(f, "return {exp};"),
            Self::Block(v) => v.iter().map(|s| write!(f, "{s}\n")).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Num(Num),
    Ident(Ident),
    BinOp(Box<Exp>, BinOp, Box<Exp>),
}

impl std::fmt::Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Ident(var) => write!(f, "{var}"),
            Self::BinOp(e1, op, e2) => write!(f, "{e1} {op} {e2}"),
        }
    }
}

// Operators
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Times => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompareOp {
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
}

impl std::fmt::Display for CompareOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsnOp {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
}

impl std::fmt::Display for AsnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "="),
            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::TimesEq => write!(f, "*="),
            Self::DivEq => write!(f, "/="),
            Self::ModEq => write!(f, "%="),
        }
    }
}

// Types
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Bool,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
        }
    }
}

// terminals
pub type Num = i32;
pub type Ident = String;
