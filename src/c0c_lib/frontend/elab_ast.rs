// types for the elaborated AST with scope semantics

use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Declare(Ident, Type, Box<Stmt>),
    Assign(Ident, Exp),
    Return(Exp),
    Seq(VecDeque<Stmt>),
    Nop,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Num(i32),
    Ident(String),
    PureBinop(Box<Exp>, PureBinOp, Box<Exp>),
    ImpureBinop(Box<Exp>, ImpureBinOp, Box<Exp>),
}

// no effects
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PureBinOp {
    Plus,
    Minus,
    Times,
}

// can raise an exception
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImpureBinOp {
    Divide, // divide by zero
    Modulo,
}

pub type Ident = super::ast::Ident;
pub type Type = super::ast::Type;
