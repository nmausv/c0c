// top level

#[derive(Debug)]
pub struct Program {
    pub name: String,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Block {
    Body(Vec<Stmt>),
}

#[derive(Debug)]
pub enum Stmt {
    Declare(Ident, Type),
    DeclareAssign(Ident, Type, Exp),
    Assign(Ident, AsnOp, Exp),
    Block(Block),
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp {
    Num(Num),
    Ident(Ident),
    BinOp(Box<Exp>, BinOp, Box<Exp>),
}

// Operators
#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
}

#[derive(Debug)]
pub enum CompareOp {
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
}

#[derive(Debug)]
pub enum AsnOp {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
}

// Types
#[derive(Debug)]
pub enum Type {
    Int,
    Char,
    Bool,
}

// terminals
pub type Num = i32;
pub type Ident = String;
