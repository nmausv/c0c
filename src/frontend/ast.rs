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
    If {
        cond: Exp,
        branch_true: Box<Stmt>,
        branch_false: Option<Box<Stmt>>,
    },
    While {
        cond: Exp,
        body: Box<Stmt>,
    },
    For {
        init: Option<Box<Stmt>>,
        cond: Exp,
        step: Option<Box<Stmt>>,
        body: Box<Stmt>,
    },
    PostOp(Ident, PostOp),
    Exp(Exp),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t) => write!(f, "{t} {var};"),
            Self::DeclareAssign(var, t, exp) => write!(f, "{t} {var} = {exp};"),
            Self::Assign(var, op, exp) => write!(f, "{var} {op} {exp};"),
            Self::PostOp(var, op) => write!(f, "{var} {op};"),
            Self::Return(exp) => write!(f, "return {exp};"),
            Self::Exp(exp) => write!(f, "{exp};"),
            Self::Block(v) => v.iter().map(|s| write!(f, "{s}\n")).collect(),
            Self::If {
                cond,
                branch_true,
                branch_false: Some(branch_false),
            } => write!(
                f,
                "if ({cond}) {{\n{branch_true}\n}} else {{\n{branch_false}\n}}"
            ),
            Self::If {
                cond,
                branch_true,
                branch_false: None,
            } => write!(f, "if ({cond}) {{\n{branch_true}\n}}"),
            Self::For {
                init,
                cond,
                step,
                body,
            } => {
                let init_string = match init {
                    None => String::new(),
                    Some(s) => s.to_string(),
                };
                let step_string = match step {
                    None => String::new(),
                    Some(s) => s.to_string(),
                };
                write!(
                    f,
                    "for ({init_string}; {cond}; {step_string}) {{\n{body}\n}}"
                )
            }
            Self::While { cond, body } => {
                write!(f, "while ({cond}) {{\n{body}\n}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Num(Num),
    Ident(Ident),
    BinOp(Box<Exp>, BinOp, Box<Exp>),
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
            Self::BinOp(e1, op, e2) => write!(f, "({e1} {op} {e2})"),
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

// Operators
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
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
    Shl,
    Shr,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Times => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
            Self::LogAnd => write!(f, "&&"),
            Self::LogOr => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitXor => write!(f, "^"),
            Self::BitOr => write!(f, "|"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    LogNegate,
    BitNegate,
    Negative,
}

impl std::fmt::Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogNegate => write!(f, "!"),
            Self::BitNegate => write!(f, "~"),
            Self::Negative => write!(f, "-"),
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
    AndEq,
    XorEq,
    OrEq,
    ShlEq,
    ShrEq,
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
            Self::AndEq => write!(f, "&="),
            Self::XorEq => write!(f, "^="),
            Self::OrEq => write!(f, "|="),
            Self::ShlEq => write!(f, "<<="),
            Self::ShrEq => write!(f, ">>="),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PostOp {
    PlusPlus,
    MinusMinus,
}

impl std::fmt::Display for PostOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PlusPlus => write!(f, "++"),
            Self::MinusMinus => write!(f, "--"),
        }
    }
}

// Types
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

// terminals
pub type Num = i32;
pub type Ident = String;
