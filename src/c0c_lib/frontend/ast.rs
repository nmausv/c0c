// top level

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub name: String,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Declare(Ident, Type),
    DeclareAssign(Ident, Type, Exp),
    Assign(Ident, AsnOp, Exp),
    Block(Vec<Stmt>),
    Return(Exp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Num(Num),
    Ident(Ident),
    BinOp(Box<Exp>, BinOp, Box<Exp>),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompareOp {
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
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

// Types
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Bool,
}

// terminals
pub type Num = i32;
pub type Ident = String;
