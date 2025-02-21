// types for the elaborated AST with scope semantics

use std::collections::VecDeque;

use super::ast;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program<'input>(Stmt<'input>);

impl<'input> From<Stmt<'input>> for Program<'input> {
    fn from(value: Stmt<'input>) -> Self {
        Self(value)
    }
}

impl<'input> AsRef<Stmt<'input>> for Program<'input> {
    fn as_ref(&self) -> &Stmt<'input> {
        &self.0
    }
}

impl<'input> std::fmt::Display for Program<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<'input> {
    Declare(Ident<'input>, Type, Box<Stmt<'input>>),
    Assign(Lvalue<'input>, Exp<'input>),
    Return(Exp<'input>),
    Seq(VecDeque<Stmt<'input>>),
    Nop,
    If {
        cond: Exp<'input>,
        stmt_true: Box<Stmt<'input>>,
        stmt_false: Box<Stmt<'input>>,
    },
    While {
        cond: Exp<'input>,
        body: Box<Stmt<'input>>,
    },
    Exp(Exp<'input>),
}

impl<'input> From<Program<'input>> for Stmt<'input> {
    fn from(value: Program<'input>) -> Self {
        value.0
    }
}

impl<'input> std::fmt::Display for Stmt<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t, scope) => {
                write!(
                    f,
                    "{{start scope of {t} {var};\n{scope} end scope of {var}}}\n"
                )
            }
            Self::Assign(var, exp) => write!(f, "{var} = {exp};"),
            Self::Nop => write!(f, "nop;"),
            Self::Return(exp) => write!(f, "return {exp};"),
            Self::Seq(v) => {
                writeln!(f, "{{start seq")?;
                let _ =
                    v.iter().map(|s| writeln!(f, "{s}")).collect::<Vec<_>>();
                write!(f, " end seq}}")
            }
            Self::If {
                cond,
                stmt_true,
                stmt_false,
            } => {
                write!(
                f,
                "if ({cond}) {{start if body\n{stmt_true}\nend if body}} else {{start else body\n{stmt_false}\nend else body}}"
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
pub enum Exp<'input> {
    Num(Num),
    Lvalue(Lvalue<'input>),
    PureBinop(Box<Exp<'input>>, PureBinOp, Box<Exp<'input>>),
    ImpureBinop(Box<Exp<'input>>, ImpureBinOp, Box<Exp<'input>>),
    UnOp(UnOp, Box<Exp<'input>>),
    True,
    False,
    Ternary {
        cond: Box<Exp<'input>>,
        exp_true: Box<Exp<'input>>,
        exp_false: Box<Exp<'input>>,
    },
}

impl<'input> From<Lvalue<'input>> for Exp<'input> {
    fn from(value: Lvalue<'input>) -> Self {
        Self::Lvalue(value)
    }
}

impl<'input> std::fmt::Display for Exp<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Lvalue(l) => write!(f, "{l}"),
            Self::PureBinop(e1, op, e2) => write!(f, "({e1} {op} {e2})"),
            Self::ImpureBinop(e1, op, e2) => write!(f, "({e1} {op} {e2})"),
            Self::UnOp(op, e) => write!(f, "{op}({e})"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Ternary {
                cond,
                exp_true,
                exp_false,
            } => write!(f, "{cond} ? {exp_true} : {exp_false}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lvalue<'input> {
    Ident(&'input str),
}

impl<'input> From<Ident<'input>> for Lvalue<'input> {
    fn from(value: Ident<'input>) -> Self {
        Self::Ident(value)
    }
}

impl<'input> TryFrom<ast::Exp<'input>> for Lvalue<'input> {
    type Error = ();
    fn try_from(value: ast::Exp<'input>) -> Result<Self, Self::Error> {
        match value {
            ast::Exp::Lvalue(ast::Lvalue::Ident(s)) => Ok(Self::Ident(s)),
            _ => Err(()),
        }
    }
}

impl<'input> std::fmt::Display for Lvalue<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(s) => write!(f, "{s}"),
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

impl From<BinOp> for ast::BinOp {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Pure(bop) => ast::BinOp::from(bop),
            BinOp::Impure(bop) => ast::BinOp::from(bop),
        }
    }
}

impl TryFrom<ast::BinOp> for BinOp {
    // only possible error is that `&&` and `||` are elaborated away
    // and so do not have a corresponding elaborated type
    type Error = ();
    fn try_from(value: ast::BinOp) -> Result<Self, Self::Error> {
        match value {
            ast::BinOp::Plus => Ok(Self::Pure(PureBinOp::Plus)),
            ast::BinOp::Minus => Ok(Self::Pure(PureBinOp::Minus)),
            ast::BinOp::Times => Ok(Self::Pure(PureBinOp::Times)),
            ast::BinOp::Divide => Ok(Self::Impure(ImpureBinOp::Divide)),
            ast::BinOp::Modulo => Ok(Self::Impure(ImpureBinOp::Modulo)),
            ast::BinOp::Less => Ok(Self::Pure(PureBinOp::Less)),
            ast::BinOp::LessEq => Ok(Self::Pure(PureBinOp::LessEq)),
            ast::BinOp::Greater => Ok(Self::Pure(PureBinOp::Greater)),
            ast::BinOp::GreaterEq => Ok(Self::Pure(PureBinOp::GreaterEq)),
            ast::BinOp::Eq => Ok(Self::Pure(PureBinOp::Eq)),
            ast::BinOp::NotEq => Ok(Self::Pure(PureBinOp::NotEq)),
            ast::BinOp::LogAnd => Err(()),
            ast::BinOp::LogOr => Err(()),
            ast::BinOp::BitAnd => Ok(Self::Pure(PureBinOp::BitAnd)),
            ast::BinOp::BitXor => Ok(Self::Pure(PureBinOp::BitXor)),
            ast::BinOp::BitOr => Ok(Self::Pure(PureBinOp::BitOr)),
            ast::BinOp::Shl => Ok(Self::Impure(ImpureBinOp::Shl)),
            ast::BinOp::Shr => Ok(Self::Impure(ImpureBinOp::Shr)),
        }
    }
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

pub type Ident<'input> = super::ast::Ident<'input>;
pub type Num = i32;
pub type Type = super::ast::Type;
