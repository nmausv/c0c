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

// acceptable to have a `default` statement, only because `nop` exists,
// no similar acceptable expression exists
impl<'input> Default for Stmt<'input> {
    fn default() -> Self {
        Self::Nop
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
    PureBinop(Box<Exp<'input>>, PureBinop, Box<Exp<'input>>),
    ImpureBinop(Box<Exp<'input>>, ImpureBinop, Box<Exp<'input>>),
    Unop(Unop, Box<Exp<'input>>),
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
            Self::Unop(op, e) => write!(f, "{op}({e})"),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
pub enum Binop {
    Pure(PureBinop),
    Impure(ImpureBinop),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OpType {
    Relational,
    Equality,
    Logical,
    Arithmetic,
}

impl From<Binop> for ast::Binop {
    fn from(value: Binop) -> Self {
        match value {
            Binop::Pure(bop) => ast::Binop::from(bop),
            Binop::Impure(bop) => ast::Binop::from(bop),
        }
    }
}

impl TryFrom<ast::Binop> for Binop {
    // only possible error is that `&&` and `||` are elaborated away
    // and so do not have a corresponding elaborated type
    type Error = ();
    fn try_from(value: ast::Binop) -> Result<Self, Self::Error> {
        match value {
            ast::Binop::Plus => Ok(Self::Pure(PureBinop::Plus)),
            ast::Binop::Minus => Ok(Self::Pure(PureBinop::Minus)),
            ast::Binop::Times => Ok(Self::Pure(PureBinop::Times)),
            ast::Binop::Divide => Ok(Self::Impure(ImpureBinop::Divide)),
            ast::Binop::Modulo => Ok(Self::Impure(ImpureBinop::Modulo)),
            ast::Binop::Less => Ok(Self::Pure(PureBinop::Less)),
            ast::Binop::LessEq => Ok(Self::Pure(PureBinop::LessEq)),
            ast::Binop::Greater => Ok(Self::Pure(PureBinop::Greater)),
            ast::Binop::GreaterEq => Ok(Self::Pure(PureBinop::GreaterEq)),
            ast::Binop::Eq => Ok(Self::Pure(PureBinop::Eq)),
            ast::Binop::NotEq => Ok(Self::Pure(PureBinop::NotEq)),
            ast::Binop::LogAnd => Err(()),
            ast::Binop::LogOr => Err(()),
            ast::Binop::BitAnd => Ok(Self::Pure(PureBinop::BitAnd)),
            ast::Binop::BitXor => Ok(Self::Pure(PureBinop::BitXor)),
            ast::Binop::BitOr => Ok(Self::Pure(PureBinop::BitOr)),
            ast::Binop::Shl => Ok(Self::Impure(ImpureBinop::Shl)),
            ast::Binop::Shr => Ok(Self::Impure(ImpureBinop::Shr)),
        }
    }
}

impl Binop {
    pub fn signature(self) -> OpType {
        match self {
            Self::Pure(PureBinop::Plus) => OpType::Arithmetic,
            Self::Pure(PureBinop::Minus) => OpType::Arithmetic,
            Self::Pure(PureBinop::Times) => OpType::Arithmetic,
            Self::Impure(ImpureBinop::Divide) => OpType::Arithmetic,
            Self::Impure(ImpureBinop::Modulo) => OpType::Arithmetic,
            Self::Pure(PureBinop::Less) => OpType::Relational,
            Self::Pure(PureBinop::LessEq) => OpType::Relational,
            Self::Pure(PureBinop::Greater) => OpType::Relational,
            Self::Pure(PureBinop::GreaterEq) => OpType::Relational,
            Self::Pure(PureBinop::Eq) => OpType::Equality,
            Self::Pure(PureBinop::NotEq) => OpType::Equality,
            Self::Pure(PureBinop::BitAnd) => OpType::Arithmetic,
            Self::Pure(PureBinop::BitXor) => OpType::Arithmetic,
            Self::Pure(PureBinop::BitOr) => OpType::Arithmetic,
            Self::Impure(ImpureBinop::Shl) => OpType::Arithmetic,
            Self::Impure(ImpureBinop::Shr) => OpType::Arithmetic,
        }
    }
}

impl From<ImpureBinop> for Binop {
    fn from(value: ImpureBinop) -> Self {
        Self::Impure(value)
    }
}

impl std::fmt::Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pure(b) => write!(f, "{b}"),
            Self::Impure(b) => write!(f, "{b}"),
        }
    }
}

// no effects
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PureBinop {
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

impl From<PureBinop> for ast::Binop {
    fn from(value: PureBinop) -> Self {
        match value {
            PureBinop::Plus => Self::Plus,
            PureBinop::Minus => Self::Minus,
            PureBinop::Times => Self::Times,
            PureBinop::Less => Self::Less,
            PureBinop::LessEq => Self::LessEq,
            PureBinop::Greater => Self::Greater,
            PureBinop::GreaterEq => Self::GreaterEq,
            PureBinop::Eq => Self::Eq,
            PureBinop::NotEq => Self::NotEq,
            PureBinop::BitAnd => Self::BitAnd,
            PureBinop::BitXor => Self::BitXor,
            PureBinop::BitOr => Self::BitOr,
        }
    }
}

impl From<PureBinop> for Binop {
    fn from(value: PureBinop) -> Self {
        Self::Pure(value)
    }
}

impl PureBinop {
    pub fn signature(self) -> OpType {
        Binop::signature(self.into())
    }
}

impl std::fmt::Display for PureBinop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ast::Binop::from(*self))
    }
}

// Unops always have no effects, can reuse the ast definition
pub type Unop = super::ast::Unop;

impl Unop {
    #[inline(always)]
    pub const fn signature(self) -> OpType {
        match self {
            Self::Negative => OpType::Arithmetic,
            Self::LogNegate => OpType::Logical,
            Self::BitNegate => OpType::Arithmetic,
        }
    }
}

// can raise an exception
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImpureBinop {
    Divide, // divide by zero
    Modulo,
    Shl,
    Shr,
}

impl From<ImpureBinop> for ast::Binop {
    fn from(value: ImpureBinop) -> Self {
        match value {
            ImpureBinop::Divide => Self::Divide,
            ImpureBinop::Modulo => Self::Modulo,
            ImpureBinop::Shl => Self::Shl,
            ImpureBinop::Shr => Self::Shr,
        }
    }
}

impl ImpureBinop {
    pub fn signature(self) -> OpType {
        Binop::signature(self.into())
    }
}

impl std::fmt::Display for ImpureBinop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ast::Binop::from(*self))
    }
}

pub type Ident<'input> = super::ast::Ident<'input>;
pub type Num = i32;
pub type Type = super::ast::Type;
