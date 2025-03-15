// Elaborates parsed syntax into the first AST with semantic information

use std::collections::VecDeque;

use super::ast;
use super::elab_ast;

use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

enum ExpFrame<'a> {
    Num(ast::Num),
    Lvalue(ast::Lvalue<'a>),
    Binop {
        left: FrameProgress<ast::Exp<'a>, elab_ast::Exp<'a>>,
        binop: ast::Binop,
        right: FrameProgress<ast::Exp<'a>, elab_ast::Exp<'a>>,
    },
    Unop {
        op: ast::Unop,
        exp: FrameProgress<ast::Exp<'a>, ()>,
    },
    True,
    False,
    Ternary {
        cond: FrameProgress<ast::Exp<'a>, elab_ast::Exp<'a>>,
        exp_true: FrameProgress<ast::Exp<'a>, elab_ast::Exp<'a>>,
        exp_false: FrameProgress<ast::Exp<'a>, elab_ast::Exp<'a>>,
    },
}

impl<'a> From<ast::Exp<'a>> for ExpFrame<'a> {
    fn from(value: ast::Exp<'a>) -> Self {
        match value {
            ast::Exp::Num(n) => Self::Num(n),
            ast::Exp::Lvalue(lval) => Self::Lvalue(lval),
            ast::Exp::Binop(left, binop, right) => Self::Binop {
                left: New(*left),
                binop,
                right: New(*right),
            },
            ast::Exp::Unop(op, exp) => Self::Unop { op, exp: New(*exp) },
            ast::Exp::True => Self::True,
            ast::Exp::False => Self::False,
            ast::Exp::Ternary {
                cond,
                branch_true,
                branch_false,
            } => Self::Ternary {
                cond: New(*cond),
                exp_true: New(*branch_true),
                exp_false: New(*branch_false),
            },
        }
    }
}

fn elaborate_exp_iterative<'input>(
    value: ast::Exp<'input>,
) -> Result<elab_ast::Exp<'input>, ()> {
    let mut stack: Vec<ExpFrame> = Vec::new();
    stack.push(value.into());

    let mut exp: Option<elab_ast::Exp> = None;

    while let Some(frame) = stack.pop() {
        match frame {
            ExpFrame::Num(ast::Num::DecNum(n)) => {
                if i128::from(i32::MIN) <= n && n <= i128::from(i32::MAX) + 1 {
                    exp = Some(elab_ast::Exp::Num(n as i32));
                } else {
                    return Err(());
                }
            }
            ExpFrame::Num(ast::Num::HexNum(n)) => {
                if n <= i128::from(u32::MAX) {
                    exp = Some(elab_ast::Exp::Num(n as i32));
                } else {
                    return Err(());
                }
            }
            ExpFrame::Lvalue(ast::Lvalue::Ident(name)) => {
                exp =
                    Some(elab_ast::Exp::Lvalue(elab_ast::Lvalue::Ident(name)));
            }
            ExpFrame::Binop { left, binop, right } => match (left, right) {
                (New(left), New(right)) => {
                    stack.push(ExpFrame::Binop {
                        left: InProgress,
                        binop,
                        right: New(right),
                    });
                    stack.push(left.into());
                }
                (InProgress, New(right)) => {
                    stack.push(ExpFrame::Binop {
                        left: Done(
                            std::mem::take(&mut exp).expect(
                                "binop left should not ever be base case",
                            ),
                        ),
                        binop,
                        right: InProgress,
                    });
                    stack.push(right.into());
                }
                (Done(elab_left), InProgress) => match binop {
                    ast::Binop::LogAnd => {
                        exp = Some(elab_ast::Exp::Ternary {
                                cond: Box::new(elab_left),
                                exp_true: Box::new(
                                    std::mem::take(&mut exp).expect(
                                        "binop (&&) right should not ever be base case",
                                    ),
                                ),
                                exp_false: Box::new(elab_ast::Exp::False),
                            });
                    }
                    ast::Binop::LogOr => {
                        exp = Some(elab_ast::Exp::Ternary {
                                cond: Box::new(elab_left),
                                exp_true: Box::new(elab_ast::Exp::True),
                                exp_false: Box::new(
                                    std::mem::take(&mut exp).expect(
                                        "binop (||) right should not ever be base case",
                                    ),
                                ),
                            });
                    }
                    ast_binop => match ast_binop.try_into() {
                        Ok(elab_ast::Binop::Pure(op)) => {
                            exp = Some(elab_ast::Exp::PureBinop(
                                Box::new(elab_left),
                                op,
                                Box::new(std::mem::take(&mut exp).expect(
                                    "binop right should not ever be base case",
                                )),
                            ))
                        }
                        Ok(elab_ast::Binop::Impure(op)) => {
                            exp = Some(elab_ast::Exp::ImpureBinop(
                                Box::new(elab_left),
                                op,
                                Box::new(std::mem::take(&mut exp).expect(
                                    "binop right should not ever be base case",
                                )),
                            ))
                        }
                        _ => return Err(()),
                    },
                },
                _ => unreachable!(),
            },
            // special handling for negative decimals, no need for recursion
            ExpFrame::Unop {
                op: ast::Unop::Negative,
                exp: New(ast::Exp::Num(ast::Num::DecNum(n))),
            } => {
                if i128::from(i32::MIN) <= -n && -n <= i128::from(i32::MAX) + 1
                {
                    exp = Some(elab_ast::Exp::Num((-n) as i32));
                } else {
                    return Err(());
                }
            }
            ExpFrame::Unop { op, exp: ast_exp } => {
                // recursion
                match ast_exp {
                    New(ast_exp) => {
                        stack.push(ExpFrame::Unop {
                            op,
                            exp: InProgress,
                        });
                        stack.push(ast_exp.into());
                    }
                    InProgress => {
                        exp =
                            Some(elab_ast::Exp::Unop(
                                op,
                                Box::new(std::mem::take(&mut exp).expect(
                                    "unop should not ever be base case",
                                )),
                            ));
                    }
                    _ => unreachable!(),
                }
            }
            ExpFrame::True => exp = Some(elab_ast::Exp::True),
            ExpFrame::False => exp = Some(elab_ast::Exp::False),
            ExpFrame::Ternary {
                cond,
                exp_true,
                exp_false,
            } => {
                match (cond, exp_true, exp_false) {
                    (New(cond), New(exp_true), New(exp_false)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: InProgress,
                            exp_true: New(exp_true),
                            exp_false: New(exp_false),
                        });
                        stack.push(cond.into());
                    }
                    (InProgress, New(exp_true), New(exp_false)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: Done(std::mem::take(&mut exp).expect(
                                "ternary condition should not ever be base case",
                            )),
                            exp_true: InProgress,
                            exp_false: New(exp_false),
                        });
                        stack.push(exp_true.into());
                    }
                    (Done(elab_cond), InProgress, New(exp_false)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: Done(elab_cond),
                            exp_true: Done(std::mem::take(&mut exp).expect(
                                "ternary true should not ever be base case",
                            )),
                            exp_false: InProgress,
                        });
                        stack.push(exp_false.into());
                    }
                    (Done(elab_cond), Done(elab_true), InProgress) => {
                        exp = Some(elab_ast::Exp::Ternary {
                            cond: Box::new(elab_cond),
                            exp_true: Box::new(elab_true),
                            exp_false: Box::new(std::mem::take(&mut exp).expect("ternary false should not ever be base case")),
                        });
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    Ok(exp.expect("expressions should never be empty"))
}

impl<'a> TryFrom<ast::Exp<'a>> for elab_ast::Exp<'a> {
    type Error = ();

    fn try_from(value: ast::Exp<'a>) -> Result<Self, Self::Error> {
        elaborate_exp_iterative(value)
    }
}

fn extract_binop(asnop: ast::AsnOp) -> Option<elab_ast::Binop> {
    match asnop {
        ast::AsnOp::Eq => None,
        ast::AsnOp::PlusEq => Some(elab_ast::PureBinop::Plus.into()),
        ast::AsnOp::MinusEq => Some(elab_ast::PureBinop::Minus.into()),
        ast::AsnOp::TimesEq => Some(elab_ast::PureBinop::Times.into()),
        ast::AsnOp::DivEq => Some(elab_ast::ImpureBinop::Divide.into()),
        ast::AsnOp::ModEq => Some(elab_ast::ImpureBinop::Modulo.into()),
        ast::AsnOp::AndEq => Some(elab_ast::PureBinop::BitAnd.into()),
        ast::AsnOp::XorEq => Some(elab_ast::PureBinop::BitXor.into()),
        ast::AsnOp::OrEq => Some(elab_ast::PureBinop::BitOr.into()),
        ast::AsnOp::ShlEq => Some(elab_ast::ImpureBinop::Shl.into()),
        ast::AsnOp::ShrEq => Some(elab_ast::ImpureBinop::Shr.into()),
    }
}

fn elaborate_stmt_recursive<'input>(
    value: ast::Stmt<'input>,
) -> Result<elab_ast::Stmt<'input>, ()> {
    match value {
        ast::Stmt::Declare(name, t) => Ok(elab_ast::Stmt::Declare(
            name,
            t,
            Box::new(elab_ast::Stmt::Nop),
        )),
        ast::Stmt::DeclareAssign(name, t, exp) => {
            let elab_exp = exp.try_into()?;
            let assign = elab_ast::Stmt::Assign(name.into(), elab_exp);
            Ok(elab_ast::Stmt::Declare(name, t, Box::new(assign)))
        }
        ast::Stmt::Block(b) => b.try_into(),
        ast::Stmt::Assign(ast::Lvalue::Ident(name), asnop, exp) => {
            let lval: elab_ast::Lvalue = (*name).into();

            let elab_exp = match extract_binop(asnop) {
                Some(elab_ast::Binop::Pure(op)) => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Lvalue(lval)),
                    op,
                    Box::new(exp.try_into()?),
                ),
                Some(elab_ast::Binop::Impure(op)) => {
                    elab_ast::Exp::ImpureBinop(
                        Box::new(elab_ast::Exp::Lvalue(lval)),
                        op,
                        Box::new(exp.try_into()?),
                    )
                }
                None => exp.try_into()?,
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
        ast::Stmt::Return(exp) => Ok(elab_ast::Stmt::Return(exp.try_into()?)),
        ast::Stmt::While { cond, body } => Ok(elab_ast::Stmt::While {
            cond: cond.try_into()?,
            body: Box::new(elaborate_stmt_recursive(*body)?),
        }),
        ast::Stmt::For {
            init,
            cond,
            step,
            body,
        } => {
            let elab_cond = cond.try_into()?;
            let elab_body = elaborate_stmt_recursive(*body)?;

            let new_body: elab_ast::Stmt;

            // elaborate the step and put it after the body
            if let Some(step) = step {
                let elab_step = elaborate_stmt_recursive(*step)?;
                new_body = match elab_body {
                    elab_ast::Stmt::Seq(mut v) => {
                        v.push_back(elab_step);
                        elab_ast::Stmt::Seq(v)
                    }
                    elab_body => {
                        elab_ast::Stmt::Seq([elab_body, elab_step].into())
                    }
                };
            } else {
                new_body = elab_body;
            }

            match init.map(|s| *s) {
                Some(ast::Stmt::Declare(name, t)) => {
                    let scope = elab_ast::Stmt::While {
                        cond: elab_cond,
                        body: Box::new(new_body),
                    };
                    Ok(elab_ast::Stmt::Declare(name, t, Box::new(scope)))
                }
                Some(ast::Stmt::DeclareAssign(name, t, exp)) => {
                    let assign =
                        elab_ast::Stmt::Assign((*name).into(), exp.try_into()?);
                    let while_ = elab_ast::Stmt::While {
                        cond: elab_cond,
                        body: Box::new(new_body),
                    };
                    let seq = elab_ast::Stmt::Seq([assign, while_].into());
                    Ok(elab_ast::Stmt::Declare(name, t, Box::new(seq)))
                }
                Some(init) => Ok(elab_ast::Stmt::Seq(
                    [
                        elaborate_stmt_recursive(init)?,
                        elab_ast::Stmt::While {
                            cond: elab_cond,
                            body: Box::new(new_body),
                        },
                    ]
                    .into(),
                )),
                None => Ok(elab_ast::Stmt::While {
                    cond: elab_cond,
                    body: Box::new(new_body),
                }),
            }
        }
        ast::Stmt::If {
            cond,
            branch_true,
            branch_false,
        } => {
            let elab_false = match branch_false {
                Some(branch_false) => elaborate_stmt_recursive(*branch_false)?,
                None => elab_ast::Stmt::Nop,
            };
            Ok(elab_ast::Stmt::If {
                cond: cond.try_into()?,
                stmt_true: Box::new(elaborate_stmt_recursive(*branch_true)?),
                stmt_false: Box::new(elab_false),
            })
        }
        ast::Stmt::Exp(exp) => Ok(elab_ast::Stmt::Exp(exp.try_into()?)),
    }
}

fn elaborate_stmts_recursive<'input>(
    mut value: Vec<ast::Stmt<'input>>,
) -> Result<elab_ast::Stmt<'input>, ()> {
    // let mut stack: Vec<_> = value.into_iter().map(|_| ()).collect();
    let mut elab_stmt = elab_ast::Stmt::Nop;
    while let Some(stmt) = value.pop() {
        elab_stmt = match stmt {
            ast::Stmt::Declare(name, t) => {
                elab_ast::Stmt::Declare(name, t, Box::new(elab_stmt))
            }
            ast::Stmt::DeclareAssign(name, t, exp) => {
                let assign = elab_ast::Stmt::Assign(
                    elab_ast::Lvalue::Ident(name),
                    exp.try_into()?,
                );
                let seq = elab_ast::Stmt::Seq([assign, elab_stmt].into());
                elab_ast::Stmt::Declare(name, t, Box::new(seq))
            }
            ast::Stmt::Block(b) if b.is_empty() => elab_stmt,
            ast::Stmt::Block(b)
                if b.len() == 1 && matches!(&b[0], ast::Stmt::Block(_)) =>
            {
                // statement looks like Block([Block([..])])
                // which can safely be converted into Block([..])
                let Some(s) = b.into_iter().next() else {
                    unreachable!()
                };

                value.push(s);
                elab_stmt
            }
            stmt => {
                let mut seq_rest = match elab_stmt {
                    elab_ast::Stmt::Seq(v) => v,
                    elab_ast::Stmt::Nop => VecDeque::new(),
                    s => VecDeque::from(vec![s]),
                };
                seq_rest.push_front(elaborate_stmt_recursive(stmt)?);
                elab_ast::Stmt::Seq(seq_rest)
            }
        }
    }
    Ok(elab_stmt)
}

enum StmtFrame<'input> {
    Declare {
        name: &'input str,
        t: ast::Type,
    },
    DeclareAssign {
        name: &'input str,
        t: ast::Type,
        exp: ast::Exp<'input>,
    },
    Assign {
        lval: ast::Lvalue<'input>,
        op: ast::AsnOp,
        exp: ast::Exp<'input>,
    },
    Block {
        // stores elaborated statement *before* recursive call
        // so the recursion can start from a fresh statement
        // and combine the two at the end
        old: FrameProgress<(), elab_ast::Stmt<'input>>,
        body: Vec<ast::Stmt<'input>>,
    },
    Return {
        exp: ast::Exp<'input>,
    },
    If {
        // stores elaborated statement *before* recursive call
        // so the recursion can start from a fresh statement
        // and combine the two at the end
        old: FrameProgress<(), elab_ast::Stmt<'input>>,
        cond: ast::Exp<'input>,
        branch_true: FrameProgress<ast::Stmt<'input>, elab_ast::Stmt<'input>>,
        branch_false:
            Option<FrameProgress<ast::Stmt<'input>, elab_ast::Stmt<'input>>>,
    },
    While {
        old: FrameProgress<(), elab_ast::Stmt<'input>>,
        cond: ast::Exp<'input>,
        body: FrameProgress<ast::Stmt<'input>, ()>,
    },
    For {
        old: FrameProgress<(), elab_ast::Stmt<'input>>,
        init: Option<FrameProgress<ast::Stmt<'input>, elab_ast::Stmt<'input>>>,
        cond: ast::Exp<'input>,
        step: Option<FrameProgress<ast::Stmt<'input>, elab_ast::Stmt<'input>>>,
        body: FrameProgress<ast::Stmt<'input>, elab_ast::Stmt<'input>>,
    },
    PostOp {
        lval: ast::Lvalue<'input>,
        op: ast::PostOp,
    },
    Exp {
        exp: ast::Exp<'input>,
    },
}

struct StmtContext<'input> {
    elab_stmt: elab_ast::Stmt<'input>,
    stack: Vec<StmtFrame<'input>>,
}

impl<'input> StmtFrame<'input> {
    fn new(value: ast::Stmt<'input>) -> Self {
        match value {
            ast::Stmt::Declare(name, t) => Self::Declare { name, t },
            ast::Stmt::DeclareAssign(name, t, exp) => {
                Self::DeclareAssign { name, t, exp }
            }
            ast::Stmt::Assign(lval, op, exp) => Self::Assign { lval, op, exp },
            ast::Stmt::Block(vec) => Self::Block {
                old: New(()),
                body: vec,
            },
            ast::Stmt::Return(exp) => Self::Return { exp },
            ast::Stmt::If {
                cond,
                branch_true,
                branch_false,
            } => Self::If {
                old: New(()),
                cond,
                branch_true: New(*branch_true),
                branch_false: branch_false.map(|s| New(*s)),
            },
            ast::Stmt::While { cond, body } => Self::While {
                old: New(()),
                cond,
                body: New(*body),
            },
            ast::Stmt::For {
                init,
                cond,
                step,
                body,
            } => Self::For {
                old: New(()),
                init: init.map(|s| New(*s)),
                cond,
                step: step.map(|s| New(*s)),
                body: New(*body),
            },
            ast::Stmt::PostOp(lval, op) => Self::PostOp { lval, op },
            ast::Stmt::Exp(exp) => Self::Exp { exp },
        }
    }
}

fn elaborate_stmts_iterative<'input>(
    stmts: Vec<ast::Stmt<'input>>,
) -> Result<elab_ast::Stmt<'input>, ()> {
    let mut ctx = StmtContext {
        elab_stmt: elab_ast::Stmt::Nop,
        stack: Vec::new(),
    };

    ctx.stack.push(StmtFrame::new(ast::Stmt::Block(stmts)));

    while let Some(frame) = ctx.stack.pop() {
        let prev_stmt = match frame {
            StmtFrame::Declare { name, t } => {
                let scope = std::mem::take(&mut ctx.elab_stmt);
                elab_ast::Stmt::Declare(name, t, Box::new(scope))
            }
            StmtFrame::DeclareAssign { name, t, exp } => {
                let assign = elab_ast::Stmt::Assign(
                    elab_ast::Lvalue::Ident(name),
                    exp.try_into()?,
                );
                let scope = std::mem::take(&mut ctx.elab_stmt);
                let seq = if scope == elab_ast::Stmt::Nop {
                    assign
                } else {
                    elab_ast::Stmt::Seq([assign, scope].into())
                };
                elab_ast::Stmt::Declare(name, t, Box::new(seq))
            }
            StmtFrame::Assign {
                lval: ast::Lvalue::Ident(name),
                op,
                exp,
            } => {
                let lval = elab_ast::Lvalue::Ident(name);
                let left = elab_ast::Exp::Lvalue(name.into());
                let elab_exp = match extract_binop(op) {
                    Some(elab_ast::Binop::Pure(op)) => {
                        elab_ast::Exp::PureBinop(
                            Box::new(left),
                            op,
                            Box::new(exp.try_into()?),
                        )
                    }
                    Some(elab_ast::Binop::Impure(op)) => {
                        elab_ast::Exp::ImpureBinop(
                            Box::new(left),
                            op,
                            Box::new(exp.try_into()?),
                        )
                    }
                    None => exp.try_into()?,
                };

                elab_ast::Stmt::Assign(lval, elab_exp)
            }
            StmtFrame::Block { old, mut body } => {
                // store current elaborated statements, start from scratch with default
                let old = match old {
                    New(()) => std::mem::take(&mut ctx.elab_stmt),
                    InProgress => unreachable!(),
                    Done(old) => old,
                };

                if let Some(to_process) = body.pop() {
                    ctx.stack.push(StmtFrame::Block {
                        old: Done(old),
                        body,
                    });
                    ctx.stack.push(StmtFrame::new(to_process));
                    continue;
                } else {
                    // entire body is elaborated into ctx.elab_stmt with old
                    match old {
                        elab_ast::Stmt::Seq(mut s) => {
                            if ctx.elab_stmt != elab_ast::Stmt::Nop {
                                s.push_back(std::mem::take(&mut ctx.elab_stmt));
                            }
                            elab_ast::Stmt::Seq(s)
                        }
                        body => {
                            if ctx.elab_stmt == elab_ast::Stmt::Nop {
                                body
                            } else if body == elab_ast::Stmt::Nop {
                                continue;
                            } else {
                                let rest = std::mem::take(&mut ctx.elab_stmt);
                                elab_ast::Stmt::Seq([body, rest].into())
                            }
                        }
                    }
                }
            }
            StmtFrame::Return { exp } => {
                elab_ast::Stmt::Return(exp.try_into()?)
            }
            StmtFrame::If {
                old,
                cond,
                branch_true,
                branch_false,
            } => {
                let old = match old {
                    New(()) => std::mem::take(&mut ctx.elab_stmt),
                    InProgress => unreachable!(),
                    Done(old) => old,
                };

                match (branch_true, branch_false) {
                    (New(branch_true), branch_false) => {
                        ctx.stack.push(StmtFrame::If {
                            old: Done(old),
                            cond,
                            branch_true: InProgress,
                            branch_false,
                        });
                        ctx.stack.push(StmtFrame::new(branch_true));
                        continue;
                    }
                    (InProgress, Some(New(branch_false))) => {
                        ctx.stack.push(StmtFrame::If {
                            old: Done(old),
                            cond,
                            branch_true: Done(std::mem::take(
                                &mut ctx.elab_stmt,
                            )),
                            branch_false: Some(InProgress),
                        });
                        ctx.stack.push(StmtFrame::new(branch_false));
                        continue;
                    }
                    (InProgress, None) => {
                        let branch_true =
                            std::mem::replace(&mut ctx.elab_stmt, old);
                        elab_ast::Stmt::If {
                            cond: cond.try_into()?,
                            stmt_true: Box::new(branch_true),
                            stmt_false: Box::new(elab_ast::Stmt::Nop),
                        }
                    }
                    (Done(branch_true), Some(InProgress)) => {
                        let branch_false =
                            std::mem::replace(&mut ctx.elab_stmt, old);
                        elab_ast::Stmt::If {
                            cond: cond.try_into()?,
                            stmt_true: Box::new(branch_true),
                            stmt_false: Box::new(branch_false),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            StmtFrame::While { old, cond, body } => {
                let old = match old {
                    New(()) => std::mem::take(&mut ctx.elab_stmt),
                    InProgress => unreachable!(),
                    Done(old) => old,
                };

                match body {
                    New(body) => {
                        ctx.stack.push(StmtFrame::While {
                            old: Done(old),
                            cond,
                            body: InProgress,
                        });
                        ctx.stack.push(StmtFrame::new(body));
                        continue;
                    }
                    InProgress => elab_ast::Stmt::While {
                        cond: cond.try_into()?,
                        body: Box::new(std::mem::replace(
                            &mut ctx.elab_stmt,
                            old,
                        )),
                    },
                    Done(()) => unreachable!(),
                }
            }
            StmtFrame::For {
                old,
                init,
                cond,
                step,
                body,
            } => {
                // store the current working elab_stmt,
                // reset to Nop for new recursive call.
                //
                // use `std::mem::replace(&mut ctx.elab_stmt, old)`
                // to get whatever's been computed, and
                // set `elab_stmt` back to what it was.
                let old = match old {
                    New(()) => std::mem::take(&mut ctx.elab_stmt),
                    InProgress => unreachable!(),
                    Done(old) => old,
                };

                match (body, step, init) {
                    (New(body), step, init) => {
                        ctx.stack.push(StmtFrame::For {
                            old: Done(old),
                            init,
                            cond,
                            step,
                            body: InProgress,
                        });
                        ctx.stack.push(StmtFrame::new(body));
                        continue;
                    }
                    (InProgress, None, None) => {
                        let body = std::mem::replace(&mut ctx.elab_stmt, old);
                        // no step, no init, just body
                        elab_ast::Stmt::While {
                            cond: cond.try_into()?,
                            body: Box::new(body),
                        }
                    }
                    (InProgress, Some(New(step)), init) => {
                        ctx.stack.push(StmtFrame::For {
                            old: Done(old),
                            init,
                            cond,
                            step: Some(InProgress),
                            body: Done(std::mem::take(&mut ctx.elab_stmt)),
                        });
                        ctx.stack.push(StmtFrame::new(step));
                        continue;
                    }
                    (InProgress, None, Some(New(init))) => match init {
                        ast::Stmt::Declare(name, t) => {
                            let body =
                                std::mem::replace(&mut ctx.elab_stmt, old);
                            let scope = elab_ast::Stmt::While {
                                cond: cond.try_into()?,
                                body: Box::new(body),
                            };
                            elab_ast::Stmt::Declare(name, t, Box::new(scope))
                        }
                        ast::Stmt::DeclareAssign(name, t, exp) => {
                            let body =
                                std::mem::replace(&mut ctx.elab_stmt, old);
                            let assign = elab_ast::Stmt::Assign(
                                (*name).into(),
                                exp.try_into()?,
                            );
                            let while_ = elab_ast::Stmt::While {
                                cond: cond.try_into()?,
                                body: Box::new(body),
                            };
                            let seq =
                                elab_ast::Stmt::Seq([assign, while_].into());
                            elab_ast::Stmt::Declare(name, t, Box::new(seq))
                        }
                        init => {
                            ctx.stack.push(StmtFrame::For {
                                old: Done(old),
                                init: Some(InProgress),
                                cond,
                                step: None,
                                body: Done(std::mem::take(&mut ctx.elab_stmt)),
                            });
                            ctx.stack.push(StmtFrame::new(init));
                            continue;
                        }
                    },
                    (Done(body), Some(InProgress), Some(New(init))) => {
                        match init {
                            ast::Stmt::Declare(name, t) => {
                                let step =
                                    std::mem::replace(&mut ctx.elab_stmt, old);
                                let body = match body {
                                    elab_ast::Stmt::Seq(mut v) => {
                                        if step != elab_ast::Stmt::Nop {
                                            v.push_back(step);
                                        }
                                        elab_ast::Stmt::Seq(v)
                                    }
                                    body => {
                                        // note: step is never nop by grammar
                                        elab_ast::Stmt::Seq([body, step].into())
                                    }
                                };
                                let scope = elab_ast::Stmt::While {
                                    cond: cond.try_into()?,
                                    body: Box::new(body),
                                };
                                elab_ast::Stmt::Declare(
                                    name,
                                    t,
                                    Box::new(scope),
                                )
                            }
                            ast::Stmt::DeclareAssign(name, t, exp) => {
                                let step =
                                    std::mem::replace(&mut ctx.elab_stmt, old);
                                let body = match body {
                                    elab_ast::Stmt::Seq(mut v) => {
                                        v.push_back(step);
                                        elab_ast::Stmt::Seq(v)
                                    }
                                    body => {
                                        elab_ast::Stmt::Seq([body, step].into())
                                    }
                                };
                                let assign = elab_ast::Stmt::Assign(
                                    (*name).into(),
                                    exp.try_into()?,
                                );
                                let while_ = elab_ast::Stmt::While {
                                    cond: cond.try_into()?,
                                    body: Box::new(body),
                                };
                                let seq = elab_ast::Stmt::Seq(
                                    [assign, while_].into(),
                                );
                                elab_ast::Stmt::Declare(name, t, Box::new(seq))
                            }
                            init => {
                                let step = std::mem::take(&mut ctx.elab_stmt);
                                ctx.stack.push(StmtFrame::For {
                                    old: Done(old),
                                    init: Some(InProgress),
                                    cond,
                                    step: Some(Done(step)),
                                    body: Done(body),
                                });
                                ctx.stack.push(StmtFrame::new(init));
                                continue;
                            }
                        }
                    }
                    (Done(body), Some(InProgress), None) => {
                        let step = std::mem::replace(&mut ctx.elab_stmt, old);
                        let body = match body {
                            elab_ast::Stmt::Seq(mut v) => {
                                v.push_back(step);
                                elab_ast::Stmt::Seq(v)
                            }
                            body => elab_ast::Stmt::Seq([body, step].into()),
                        };
                        elab_ast::Stmt::While {
                            cond: cond.try_into()?,
                            body: Box::new(body),
                        }
                    }
                    (Done(body), Some(Done(step)), Some(InProgress)) => {
                        // know that init is not a declare/declare+assign,
                        // since those would have been caught by prev
                        // match guards,
                        // so can just put init into seq with while
                        let init = std::mem::replace(&mut ctx.elab_stmt, old);
                        let body = match body {
                            elab_ast::Stmt::Seq(mut v) => {
                                v.push_back(step);
                                elab_ast::Stmt::Seq(v)
                            }
                            body => elab_ast::Stmt::Seq([body, step].into()),
                        };
                        let while_ = elab_ast::Stmt::While {
                            cond: cond.try_into()?,
                            body: Box::new(body),
                        };
                        elab_ast::Stmt::Seq([init, while_].into())
                    }
                    _ => unreachable!(),
                }
            }
            StmtFrame::PostOp {
                lval: ast::Lvalue::Ident(name),
                op: ast::PostOp::PlusPlus,
            } => {
                let lval = elab_ast::Exp::Lvalue(elab_ast::Lvalue::Ident(name));
                let exp = elab_ast::Exp::PureBinop(
                    Box::new(lval),
                    elab_ast::PureBinop::Plus,
                    Box::new(elab_ast::Exp::Num(1)),
                );
                elab_ast::Stmt::Assign(name.into(), exp)
            }
            StmtFrame::PostOp {
                lval: ast::Lvalue::Ident(name),
                op: ast::PostOp::MinusMinus,
            } => {
                let lval = elab_ast::Exp::Lvalue(elab_ast::Lvalue::Ident(name));
                let exp = elab_ast::Exp::PureBinop(
                    Box::new(lval),
                    elab_ast::PureBinop::Minus,
                    Box::new(elab_ast::Exp::Num(1)),
                );
                elab_ast::Stmt::Assign(name.into(), exp)
            }
            StmtFrame::Exp { exp } => elab_ast::Stmt::Exp(exp.try_into()?),
        };

        if prev_stmt == elab_ast::Stmt::Nop {
            continue;
        }

        // combine into elab_stmt
        ctx.elab_stmt = match ctx.elab_stmt {
            elab_ast::Stmt::Nop => prev_stmt,
            elab_ast::Stmt::Seq(mut v) => {
                v.push_front(prev_stmt);
                elab_ast::Stmt::Seq(v)
            }
            next_stmt => elab_ast::Stmt::Seq([prev_stmt, next_stmt].into()),
        };
    }

    Ok(ctx.elab_stmt)
}

impl<'input> TryFrom<Vec<ast::Stmt<'input>>> for elab_ast::Stmt<'input> {
    type Error = ();

    /// Elaborate a slice of basic statements into a single elaborated statement,
    /// rewriting and removing "syntactic sugar".
    ///
    /// This function needs to be written to avoid recursion, since input code
    /// can require an unbounded number of recursive calls.
    fn try_from(value: Vec<ast::Stmt<'input>>) -> Result<Self, Self::Error> {
        elaborate_stmts_iterative(value)
    }
}

pub fn elaborate(program: ast::Program) -> Result<elab_ast::Program, ()> {
    // assert that main function is identified as main
    if program.name != "main" {
        Err(())
    } else {
        program.body.try_into().map(|s: elab_ast::Stmt| s.into())
    }
}
