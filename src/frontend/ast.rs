#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<'input> {
    pub name: &'input str,
    pub body: Vec<Stmt<'input>>,
}

impl<'input> std::fmt::Display for Program<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} defined as {{", self.name)?;
        let _ = self
            .body
            .iter()
            .map(|s| writeln!(f, "{s}"))
            .collect::<Vec<_>>();
        write!(f, "}} end of {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<'input> {
    Declare(Ident<'input>, Type),
    DeclareAssign(Ident<'input>, Type, Exp<'input>),
    Assign(Lvalue<'input>, AsnOp, Exp<'input>),
    Block(Vec<Stmt<'input>>),
    Return(Exp<'input>),
    If {
        cond: Exp<'input>,
        branch_true: Box<Stmt<'input>>,
        branch_false: Option<Box<Stmt<'input>>>,
    },
    While {
        cond: Exp<'input>,
        body: Box<Stmt<'input>>,
    },
    For {
        init: Option<Box<Stmt<'input>>>,
        cond: Exp<'input>,
        step: Option<Box<Stmt<'input>>>,
        body: Box<Stmt<'input>>,
    },
    PostOp(Lvalue<'input>, PostOp),
    Exp(Exp<'input>),
}

impl<'input> std::fmt::Display for Stmt<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(var, t) => write!(f, "{t} {var};"),
            Self::DeclareAssign(var, t, exp) => write!(f, "{t} {var} = {exp};"),
            Self::Assign(var, op, exp) => write!(f, "{var} {op} {exp};"),
            Self::PostOp(var, op) => write!(f, "{var}{op};"),
            Self::Return(exp) => write!(f, "return {exp};"),
            Self::Exp(exp) => write!(f, "{exp};"),
            Self::Block(v) => v.iter().try_for_each(|s| writeln!(f, "{s}")),
            Self::If {
                cond,
                branch_true,
                branch_false: Some(branch_false),
            } => write!(
                f,
                "if ({cond}) {{start if body\n{branch_true}\nend if body}} else {{start else body\n{branch_false}\nend else body}}"
            ),
            Self::If {
                cond,
                branch_true,
                branch_false: None,
            } => write!(f, "if ({cond}) {{start if body\n{branch_true}\nend if body}}"),
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
                    "for ({{{init_string}}}; {cond}; {{{step_string}}}) {{start for body\n{body}\nend for body}}"
                )
            }
            Self::While { cond, body } => {
                write!(f, "while ({cond}) {{start while body\n{body}\nend while body}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp<'input> {
    Num(Num),
    Lvalue(Lvalue<'input>),
    BinOp(Box<Exp<'input>>, BinOp, Box<Exp<'input>>),
    UnOp(UnOp, Box<Exp<'input>>),
    True,
    False,
    Ternary {
        cond: Box<Exp<'input>>,
        branch_true: Box<Exp<'input>>,
        branch_false: Box<Exp<'input>>,
    },
}

impl<'input> std::fmt::Display for Exp<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
            Self::Lvalue(var) => write!(f, "{var}"),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lvalue<'input> {
    Ident(&'input str),
}

impl<'input> TryFrom<Exp<'input>> for Lvalue<'input> {
    type Error = ();
    fn try_from(value: Exp<'input>) -> Result<Self, Self::Error> {
        match value {
            Exp::Lvalue(l) => Ok(l),
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
pub type Num = i128;
pub type Ident<'input> = &'input str;
