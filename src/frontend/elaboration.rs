// Elaborates parsed syntax into the first AST with semantic information

use std::collections::VecDeque;

use super::ast;
use super::elab_ast;

use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

enum ExpFrame<'a> {
    Num(ast::Num),
    Lvalue(ast::Lvalue<'a>),
    Binop {
        left: ast::Exp<'a>,
        elab_left: FrameProgress<elab_ast::Exp<'a>>,
        binop: ast::Binop,
        right: ast::Exp<'a>,
        elab_right: FrameProgress<elab_ast::Exp<'a>>,
    },
    Unop {
        op: ast::Unop,
        exp: ast::Exp<'a>,
        elab_exp: FrameProgress<()>,
    },
    True,
    False,
    Ternary {
        cond: ast::Exp<'a>,
        elab_cond: FrameProgress<elab_ast::Exp<'a>>,
        exp_true: ast::Exp<'a>,
        elab_true: FrameProgress<elab_ast::Exp<'a>>,
        exp_false: ast::Exp<'a>,
        elab_false: FrameProgress<elab_ast::Exp<'a>>,
    },
}

impl<'a> From<ast::Exp<'a>> for ExpFrame<'a> {
    fn from(value: ast::Exp<'a>) -> Self {
        match value {
            ast::Exp::Num(n) => Self::Num(n),
            ast::Exp::Lvalue(lval) => Self::Lvalue(lval),
            ast::Exp::Binop(left, binop, right) => Self::Binop {
                left: *left,
                elab_left: New,
                binop,
                right: *right,
                elab_right: New,
            },
            ast::Exp::Unop(op, exp) => Self::Unop {
                op,
                exp: *exp,
                elab_exp: New,
            },
            ast::Exp::True => Self::True,
            ast::Exp::False => Self::False,
            ast::Exp::Ternary {
                cond,
                branch_true,
                branch_false,
            } => Self::Ternary {
                cond: *cond,
                elab_cond: New,
                exp_true: *branch_true,
                elab_true: New,
                exp_false: *branch_false,
                elab_false: New,
            },
        }
    }
}

impl<'a> TryFrom<ast::Exp<'a>> for elab_ast::Exp<'a> {
    type Error = ();

    fn try_from(value: ast::Exp<'a>) -> Result<Self, Self::Error> {
        let mut stack: Vec<ExpFrame> = Vec::new();
        stack.push(value.into());

        // need to initialize, since Rust can't guarantee that this is valid.
        // I could use an option, but then I need to pattern match all over the place.
        let mut exp: elab_ast::Exp = elab_ast::Exp::default();

        while let Some(frame) = stack.pop() {
            match frame {
                ExpFrame::Num(ast::Num::DecNum(n)) => {
                    if i128::from(i32::MIN) <= n
                        && n <= i128::from(i32::MAX) + 1
                    {
                        exp = elab_ast::Exp::Num(n as i32);
                    } else {
                        return Err(());
                    }
                }
                ExpFrame::Num(ast::Num::HexNum(n)) => {
                    if n <= i128::from(u32::MAX) {
                        exp = elab_ast::Exp::Num(n as i32);
                    } else {
                        return Err(());
                    }
                }
                ExpFrame::Lvalue(ast::Lvalue::Ident(name)) => {
                    exp = elab_ast::Exp::Lvalue(elab_ast::Lvalue::Ident(name));
                }
                ExpFrame::Binop {
                    left,
                    elab_left,
                    binop,
                    right,
                    elab_right,
                } => match (elab_left, elab_right) {
                    (New, New) => {
                        stack.push(ExpFrame::Binop {
                            left: ast::Exp::default(),
                            elab_left: InProgress,
                            binop,
                            right,
                            elab_right: New,
                        });
                        stack.push(left.into());
                    }
                    (InProgress, New) => {
                        let mut stored = elab_ast::Exp::Num(0);
                        std::mem::swap(&mut exp, &mut stored);
                        stack.push(ExpFrame::Binop {
                            left,
                            elab_left: Done(stored),
                            binop,
                            right: ast::Exp::default(),
                            elab_right: InProgress,
                        });
                        stack.push(right.into());
                    }
                    (Done(elab_left), InProgress) => match binop {
                        ast::Binop::LogAnd => {
                            exp = elab_ast::Exp::Ternary {
                                cond: Box::new(elab_left),
                                exp_true: Box::new(exp),
                                exp_false: Box::new(elab_ast::Exp::False),
                            };
                        }
                        ast::Binop::LogOr => {
                            exp = elab_ast::Exp::Ternary {
                                cond: Box::new(elab_left),
                                exp_true: Box::new(elab_ast::Exp::True),
                                exp_false: Box::new(exp),
                            };
                        }
                        ast_binop => match ast_binop.try_into() {
                            Ok(elab_ast::Binop::Pure(op)) => {
                                exp = elab_ast::Exp::PureBinop(
                                    Box::new(elab_left),
                                    op,
                                    Box::new(exp),
                                )
                            }
                            Ok(elab_ast::Binop::Impure(op)) => {
                                exp = elab_ast::Exp::ImpureBinop(
                                    Box::new(elab_left),
                                    op,
                                    Box::new(exp),
                                )
                            }
                            _ => return Err(()),
                        },
                    },
                    _ => unreachable!(),
                },
                // special handling for negative decimals, no need for recursion
                ExpFrame::Unop {
                    op: ast::Unop::Negative,
                    exp: ast::Exp::Num(ast::Num::DecNum(n)),
                    elab_exp: _,
                } => {
                    if i128::from(i32::MIN) <= -n
                        && -n <= i128::from(i32::MAX) + 1
                    {
                        exp = elab_ast::Exp::Num((-n) as i32);
                    } else {
                        return Err(());
                    }
                }
                ExpFrame::Unop {
                    op,
                    exp: ast_exp,
                    elab_exp,
                } => {
                    // recursion
                    match elab_exp {
                        New => {
                            stack.push(ExpFrame::Unop {
                                op,
                                exp: ast::Exp::False,
                                elab_exp: InProgress,
                            });
                            stack.push(ast_exp.into());
                        }
                        InProgress => {
                            exp = elab_ast::Exp::Unop(op, Box::new(exp));
                        }
                        _ => unreachable!(),
                    }
                }
                ExpFrame::True => exp = elab_ast::Exp::True,
                ExpFrame::False => exp = elab_ast::Exp::False,
                ExpFrame::Ternary {
                    cond,
                    elab_cond,
                    exp_true,
                    elab_true,
                    exp_false,
                    elab_false,
                } => match (elab_cond, elab_true, elab_false) {
                    (New, New, New) => {
                        stack.push(ExpFrame::Ternary {
                            cond: ast::Exp::default(),
                            elab_cond: InProgress,
                            exp_true,
                            elab_true: New,
                            exp_false,
                            elab_false: New,
                        });
                        stack.push(cond.into());
                    }
                    (InProgress, New, New) => {
                        let mut stored = elab_ast::Exp::Num(0);
                        std::mem::swap(&mut exp, &mut stored);
                        stack.push(ExpFrame::Ternary {
                            cond,
                            elab_cond: Done(stored),
                            exp_true: ast::Exp::default(),
                            elab_true: InProgress,
                            exp_false,
                            elab_false: New,
                        });
                        stack.push(exp_true.into());
                    }
                    (Done(elab_cond), InProgress, New) => {
                        let mut stored = elab_ast::Exp::Num(0);
                        std::mem::swap(&mut exp, &mut stored);
                        stack.push(ExpFrame::Ternary {
                            cond,
                            elab_cond: Done(elab_cond),
                            exp_true,
                            elab_true: Done(stored),
                            exp_false: ast::Exp::default(),
                            elab_false: InProgress,
                        });
                        stack.push(exp_false.into());
                    }
                    (Done(elab_cond), Done(elab_true), InProgress) => {
                        exp = elab_ast::Exp::Ternary {
                            cond: Box::new(elab_cond),
                            exp_true: Box::new(elab_true),
                            exp_false: Box::new(exp),
                        };
                    }
                    _ => unreachable!(),
                },
            }
        }

        Ok(exp)
    }
}

fn extract_binop(asnop: ast::AsnOp) -> Result<elab_ast::Binop, ()> {
    match asnop {
        ast::AsnOp::Eq => Err(()),
        ast::AsnOp::PlusEq => Ok(elab_ast::PureBinop::Plus.into()),
        ast::AsnOp::MinusEq => Ok(elab_ast::PureBinop::Minus.into()),
        ast::AsnOp::TimesEq => Ok(elab_ast::PureBinop::Times.into()),
        ast::AsnOp::DivEq => Ok(elab_ast::ImpureBinop::Divide.into()),
        ast::AsnOp::ModEq => Ok(elab_ast::ImpureBinop::Modulo.into()),
        ast::AsnOp::AndEq => Ok(elab_ast::PureBinop::BitAnd.into()),
        ast::AsnOp::XorEq => Ok(elab_ast::PureBinop::BitXor.into()),
        ast::AsnOp::OrEq => Ok(elab_ast::PureBinop::BitOr.into()),
        ast::AsnOp::ShlEq => Ok(elab_ast::ImpureBinop::Shl.into()),
        ast::AsnOp::ShrEq => Ok(elab_ast::ImpureBinop::Shr.into()),
    }
}

/// Elaborates a single statement, but since `Declare` and `DeclareAssign`
/// have unique interactions with subsequent statements, these statements
/// cannot be evaluated individually using this function.
///
/// This function can fail if the passed `ast::Stmt` is a `Declare` variant,
/// or if there is an assignment to an invalid `Exp`, such as
/// ```c
/// int main() {
///     int x;
///     x + 1 = 2; // invalid
///     return x;
/// }
/// ```
fn elaborate_stmt<'input>(
    stmt: ast::Stmt<'input>,
) -> Result<elab_ast::Stmt<'input>, ()> {
    match stmt {
        ast::Stmt::Declare(_, _) => Err(()),
        ast::Stmt::DeclareAssign(_, _, _) => Err(()),
        ast::Stmt::Block(b) => elaborate_stmts(b),
        ast::Stmt::Assign(ast::Lvalue::Ident(name), asnop, exp) => {
            let lval: elab_ast::Lvalue = (*name).into();

            let elab_exp = match extract_binop(asnop) {
                Ok(elab_ast::Binop::Pure(op)) => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Lvalue(lval)),
                    op,
                    Box::new(exp.try_into()?),
                ),
                Ok(elab_ast::Binop::Impure(op)) => elab_ast::Exp::ImpureBinop(
                    Box::new(elab_ast::Exp::Lvalue(lval)),
                    op,
                    Box::new(exp.try_into()?),
                ),
                Err(()) => exp.try_into()?,
            };
            Ok(elab_ast::Stmt::Assign(lval, elab_exp))
        }
        ast::Stmt::PostOp(ast::Lvalue::Ident(var), ast::PostOp::PlusPlus) => {
            Ok(elab_ast::Stmt::Assign(
                (*var).into(),
                elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Lvalue((*var).into())),
                    elab_ast::PureBinop::Plus,
                    Box::new(elab_ast::Exp::Num(1)),
                ),
            ))
        }
        ast::Stmt::PostOp(ast::Lvalue::Ident(var), ast::PostOp::MinusMinus) => {
            Ok(elab_ast::Stmt::Assign(
                (*var).into(),
                elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::from(elab_ast::Lvalue::from(var))),
                    elab_ast::PureBinop::Minus,
                    Box::new(elab_ast::Exp::Num(1)),
                ),
            ))
        }
        ast::Stmt::Return(exp) => {
            Ok(elab_ast::Stmt::Return(exp.try_into()?))
        }
        ast::Stmt::While { cond, body } => Ok(elab_ast::Stmt::While {
            cond: cond.try_into()?,
            body: Box::new(elaborate_stmt(*body)?),
        }),
        ast::Stmt::For {
            init,
            cond,
            step,
            body,
        } => {
            let elab_cond = cond.try_into()?;
            let elab_body = elaborate_stmt(*body)?;

            let new_body: elab_ast::Stmt;

            // elaborate the step and put it after the body
            if let Some(step) = step {
                new_body = match (*step, elab_body) {
                    (ast::Stmt::Declare(_, _), _) => {
                        // panic!("step cannot be declaration in for loop")
                        return Err(());
                    }
                    (ast::Stmt::DeclareAssign(_, _, _), _) => {
                        // panic!("step cannot be declaration in for loop")
                        return Err(());
                    }
                    (step, elab_ast::Stmt::Seq(mut v)) => {
                        v.push_back(elaborate_stmt(step)?);
                        elab_ast::Stmt::Seq(v)
                    }
                    (step, body) => elab_ast::Stmt::Seq(
                        [body, elaborate_stmt(step)?].into(),
                    ),
                };
            } else {
                new_body = elab_body;
            }

            if let Some(init) = init {
                match *init {
                    ast::Stmt::Declare(name, t) => Ok(elab_ast::Stmt::Declare(
                        name,
                        t,
                        Box::new(elab_ast::Stmt::While {
                            cond: elab_cond,
                            body: Box::new(new_body),
                        }),
                    )),
                    ast::Stmt::DeclareAssign(name, t, exp) => {
                        Ok(elab_ast::Stmt::Declare(
                            name,
                            t,
                            Box::new(elab_ast::Stmt::Seq(
                                [
                                    elab_ast::Stmt::Assign(
                                        (*name).into(),
                                        exp.try_into()?,
                                    ),
                                    elab_ast::Stmt::While {
                                        cond: elab_cond,
                                        body: Box::new(new_body),
                                    },
                                ]
                                .into(),
                            )),
                        ))
                    }
                    _ => Ok(elab_ast::Stmt::Seq(
                        [
                            elaborate_stmt(*init)?,
                            elab_ast::Stmt::While {
                                cond: elab_cond,
                                body: Box::new(new_body),
                            },
                        ]
                        .into(),
                    )),
                }
            } else {
                // no init
                Ok(elab_ast::Stmt::While {
                    cond: elab_cond,
                    body: Box::new(new_body),
                })
            }
        }
        ast::Stmt::If {
            cond,
            branch_true,
            branch_false,
        } => {
            let elab_false = match branch_false {
                Some(branch_false) => elaborate_stmt(*branch_false)?,
                None => elab_ast::Stmt::Nop,
            };
            Ok(elab_ast::Stmt::If {
                cond: cond.try_into()?,
                stmt_true: Box::new(elaborate_stmt(*branch_true)?),
                stmt_false: Box::new(elab_false),
            })
        }
        ast::Stmt::Exp(exp) => Ok(elab_ast::Stmt::Exp(exp.try_into()?)),
    }
}

/// Elaborate a slice of basic statements into a single elaborated statement,
/// rewriting and removing "syntactic sugar".
///
/// This function needs to be written to avoid recursion, since input code
/// can require an unbounded number of recursive calls.
fn elaborate_stmts<'input>(
    mut stmts: Vec<ast::Stmt<'input>>,
) -> Result<elab_ast::Stmt<'input>, ()> {
    let mut elab_stmt = elab_ast::Stmt::Nop;
    while let Some(stmt) = stmts.pop() {
        // for stmt in stmts.into_iter().rev() {
        // only the statements that can't be appended to a sequence need special handling here, otherwise we can use
        // `elaborate_stmt`
        elab_stmt = match stmt {
            ast::Stmt::Declare(name, t) => {
                elab_ast::Stmt::Declare(name, t, Box::new(elab_stmt))
            }
            ast::Stmt::DeclareAssign(name, t, exp) => elab_ast::Stmt::Declare(
                name,
                t,
                Box::new(elab_ast::Stmt::Seq(VecDeque::from(vec![
                    elab_ast::Stmt::Assign(
                        elab_ast::Lvalue::Ident(name),
                        exp.try_into()?,
                    ),
                    elab_stmt,
                ]))),
            ),
            // We want to flatten out sequences, to avoid things like
            // `Seq(Seq(s1), Seq(s2))` in favor of `Seq(s1 @ s2)` where `@` denotes concatenation.
            // since scope information is stored via `Declare`.
            ast::Stmt::Block(mut b) => match elab_stmt {
                elab_ast::Stmt::Seq(_) => {
                    stmts.append(&mut b);
                    elab_stmt
                }
                _ => {
                    stmts.append(&mut b);
                    elab_ast::Stmt::Seq([elab_stmt].into())
                }
            },
            stmt => {
                let mut seq_rest = match elab_stmt {
                    elab_ast::Stmt::Seq(v) => v,
                    elab_ast::Stmt::Nop => VecDeque::new(),
                    s => VecDeque::from(vec![s]),
                };
                seq_rest.push_front(elaborate_stmt(stmt)?);
                elab_ast::Stmt::Seq(seq_rest)
            }
        }
    }
    Ok(elab_stmt)
}

pub fn elaborate(program: ast::Program) -> Result<elab_ast::Program, ()> {
    // assert that main function is identified as main
    if program.name != "main" {
        Err(())
    } else {
        elaborate_stmts(program.body).map(|s| s.into())
    }
}
