// Elaborates parsed syntax into the first AST with semantic information

use std::collections::VecDeque;

use super::ast;
use super::elab_ast;

fn elaborate_binop<'input>(
    eleft: &ast::Exp<'input>,
    binop: &ast::BinOp,
    eright: &ast::Exp<'input>,
) -> Result<elab_ast::Exp<'input>, ()> {
    let elab_left = Box::new(elaborate_exp(eleft)?);
    let elab_right = Box::new(elaborate_exp(eright)?);

    match binop {
        ast::BinOp::LogAnd => Ok(elab_ast::Exp::Ternary {
            cond: elab_left,
            exp_true: elab_right,
            exp_false: Box::new(elab_ast::Exp::False),
        }),
        ast::BinOp::LogOr => Ok(elab_ast::Exp::Ternary {
            cond: elab_left,
            exp_true: Box::new(elab_ast::Exp::True),
            exp_false: elab_right,
        }),
        ast_binop => match elab_ast::BinOp::try_from(*ast_binop) {
            Ok(elab_ast::BinOp::Pure(op)) => {
                Ok(elab_ast::Exp::PureBinop(elab_left, op, elab_right))
            }
            Ok(elab_ast::BinOp::Impure(op)) => {
                Ok(elab_ast::Exp::ImpureBinop(elab_left, op, elab_right))
            }
            _ => {
                eprintln!("elaborate_binop: attempted conversion of && or || into a single elaborated binop");
                Err(())
            }
        },
    }
}

fn elaborate_exp<'input>(
    exp: &ast::Exp<'input>,
) -> Result<elab_ast::Exp<'input>, ()> {
    match exp {
        ast::Exp::Num(ast::Num::DecNum(n)) => {
            // bounds check decimal literals
            if i128::from(i32::MIN) <= *n && *n <= i128::from(i32::MAX) + 1 {
                Ok(elab_ast::Exp::Num(*n as i32))
            } else {
                eprintln!(
                    "elaborate_exp: integer literal {n} failed bounds check"
                );
                Err(())
            }
        }
        ast::Exp::Num(ast::Num::HexNum(n)) => {
            // bounds check hex literals
            if *n <= i128::from(u32::MAX) {
                Ok(elab_ast::Exp::Num(*n as i32))
            } else {
                eprintln!(
                    "elaborate_exp: integer literal {n:#} failed bounds check"
                );
                Err(())
            }
        }
        ast::Exp::Lvalue(ast::Lvalue::Ident(name)) => {
            Ok(elab_ast::Exp::Lvalue((*name).into()))
        }
        ast::Exp::BinOp(e1, binop, e2) => elaborate_binop(e1, binop, e2),
        // extra handling for negative literals
        ast::Exp::UnOp(op, exp) => match (op, exp.as_ref()) {
            (ast::UnOp::Negative, ast::Exp::Num(ast::Num::DecNum(n))) => {
                // bounds check negative integer literals
                if i128::from(i32::MIN) <= -(*n)
                    && -(*n) <= i128::from(i32::MAX) + 1
                {
                    Ok(elab_ast::Exp::Num((-n) as i32))
                } else {
                    eprintln!(
                        "integer literal {n} failed negation bounds check"
                    );
                    Err(())
                }
            }
            _ => Ok(elab_ast::Exp::UnOp(*op, Box::new(elaborate_exp(exp)?))),
        },
        ast::Exp::True => Ok(elab_ast::Exp::True),
        ast::Exp::False => Ok(elab_ast::Exp::False),
        ast::Exp::Ternary {
            cond,
            branch_true,
            branch_false,
        } => Ok(elab_ast::Exp::Ternary {
            cond: Box::new(elaborate_exp(cond)?),
            exp_true: Box::new(elaborate_exp(branch_true)?),
            exp_false: Box::new(elaborate_exp(branch_false)?),
        }),
    }
}

fn extract_binop(asnop: ast::AsnOp) -> Result<elab_ast::BinOp, ()> {
    match asnop {
        ast::AsnOp::Eq => Err(()),
        ast::AsnOp::PlusEq => Ok(elab_ast::PureBinOp::Plus.into()),
        ast::AsnOp::MinusEq => Ok(elab_ast::PureBinOp::Minus.into()),
        ast::AsnOp::TimesEq => Ok(elab_ast::PureBinOp::Times.into()),
        ast::AsnOp::DivEq => Ok(elab_ast::ImpureBinOp::Divide.into()),
        ast::AsnOp::ModEq => Ok(elab_ast::ImpureBinOp::Modulo.into()),
        ast::AsnOp::AndEq => Ok(elab_ast::PureBinOp::BitAnd.into()),
        ast::AsnOp::XorEq => Ok(elab_ast::PureBinOp::BitXor.into()),
        ast::AsnOp::OrEq => Ok(elab_ast::PureBinOp::BitOr.into()),
        ast::AsnOp::ShlEq => Ok(elab_ast::ImpureBinOp::Shl.into()),
        ast::AsnOp::ShrEq => Ok(elab_ast::ImpureBinOp::Shr.into()),
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
    stmt: &ast::Stmt<'input>,
) -> Result<elab_ast::Stmt<'input>, ()> {
    match stmt {
        ast::Stmt::Declare(_, _) => Err(()),
        ast::Stmt::DeclareAssign(_, _, _) => Err(()),
        ast::Stmt::Block(b) => elaborate_stmts(b),
        ast::Stmt::Assign(ast::Lvalue::Ident(name), asnop, exp) => {
            let lval: elab_ast::Lvalue = (*name).into();

            let elab_exp = match extract_binop(*asnop) {
                Ok(elab_ast::BinOp::Pure(op)) => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Lvalue(lval)),
                    op,
                    Box::new(elaborate_exp(exp)?),
                ),
                Ok(elab_ast::BinOp::Impure(op)) => elab_ast::Exp::ImpureBinop(
                    Box::new(elab_ast::Exp::Lvalue(lval)),
                    op,
                    Box::new(elaborate_exp(exp)?),
                ),
                Err(()) => elaborate_exp(exp)?,
            };
            Ok(elab_ast::Stmt::Assign(lval, elab_exp))
        }
        ast::Stmt::PostOp(ast::Lvalue::Ident(var), ast::PostOp::PlusPlus) => {
            Ok(elab_ast::Stmt::Assign(
                (*var).into(),
                elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Lvalue((*var).into())),
                    elab_ast::PureBinOp::Plus,
                    Box::new(elab_ast::Exp::Num(1)),
                ),
            ))
        }
        ast::Stmt::PostOp(ast::Lvalue::Ident(var), ast::PostOp::MinusMinus) => {
            Ok(elab_ast::Stmt::Assign(
                (*var).into(),
                elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::from(elab_ast::Lvalue::from(*var))),
                    elab_ast::PureBinOp::Minus,
                    Box::new(elab_ast::Exp::Num(1)),
                ),
            ))
        }
        ast::Stmt::Return(exp) => {
            Ok(elab_ast::Stmt::Return(elaborate_exp(exp)?))
        }
        ast::Stmt::While { cond, body } => Ok(elab_ast::Stmt::While {
            cond: elaborate_exp(cond)?,
            body: Box::new(elaborate_stmt(body)?),
        }),
        ast::Stmt::For {
            init,
            cond,
            step,
            body,
        } => {
            let elab_cond = elaborate_exp(cond)?;
            let elab_body = elaborate_stmt(body)?;

            let new_body: elab_ast::Stmt;

            // elaborate the step and put it after the body
            if let Some(step) = step {
                new_body = match (step.as_ref(), elab_body) {
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
                match init.as_ref() {
                    ast::Stmt::Declare(name, t) => Ok(elab_ast::Stmt::Declare(
                        name,
                        *t,
                        Box::new(elab_ast::Stmt::While {
                            cond: elab_cond,
                            body: Box::new(new_body),
                        }),
                    )),
                    ast::Stmt::DeclareAssign(name, t, exp) => {
                        Ok(elab_ast::Stmt::Declare(
                            name,
                            *t,
                            Box::new(elab_ast::Stmt::Seq(
                                [
                                    elab_ast::Stmt::Assign(
                                        (*name).into(),
                                        elaborate_exp(exp)?,
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
                            elaborate_stmt(init)?,
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
                Some(branch_false) => elaborate_stmt(branch_false)?,
                None => elab_ast::Stmt::Nop,
            };
            Ok(elab_ast::Stmt::If {
                cond: elaborate_exp(cond)?,
                stmt_true: Box::new(elaborate_stmt(branch_true)?),
                stmt_false: Box::new(elab_false),
            })
        }
        ast::Stmt::Exp(exp) => Ok(elab_ast::Stmt::Exp(elaborate_exp(exp)?)),
    }
}

/// Elaborate a slice of basic statements into a single elaborated statement,
/// rewriting and removing "syntactic sugar".
///
/// This function needs to be written to avoid recursion, since input code
/// can require an unbounded number of recursive calls.
fn elaborate_stmts<'input>(
    stmts: &[ast::Stmt<'input>],
) -> Result<elab_ast::Stmt<'input>, ()> {
    let mut elab_stmt = elab_ast::Stmt::Nop;
    for stmt in stmts.iter().rev() {
        // only the statements that can't be appended to a sequence need special handling here, otherwise we can use
        // `elaborate_stmt`
        elab_stmt = match stmt {
            ast::Stmt::Declare(name, t) => {
                elab_ast::Stmt::Declare(name, *t, Box::new(elab_stmt))
            }
            ast::Stmt::DeclareAssign(name, t, exp) => elab_ast::Stmt::Declare(
                name,
                *t,
                Box::new(elab_ast::Stmt::Seq(VecDeque::from(vec![
                    elab_ast::Stmt::Assign(
                        elab_ast::Lvalue::Ident(name),
                        elaborate_exp(exp)?,
                    ),
                    elab_stmt,
                ]))),
            ),
            // this recursive call is not ideal, but removing it is difficult
            // since it means we need to keep a whole stack of elaborated statements in progress,
            // and not just the current right hand side.
            //
            // we also want to flatten out sequences, to avoid things like
            // ```
            // Seq(Seq(s1), Seq(s2))
            // ```
            // since scope information is stored via `Declare`.
            ast::Stmt::Block(b) => match (elab_stmt, elaborate_stmts(b)?) {
                (
                    elab_ast::Stmt::Seq(mut seq1),
                    elab_ast::Stmt::Seq(mut seq2),
                ) => {
                    seq1.append(&mut seq2);
                    elab_ast::Stmt::Seq(seq1)
                }
                (elab_ast::Stmt::Seq(mut seq1), stmt) => {
                    seq1.push_back(stmt);
                    elab_ast::Stmt::Seq(seq1)
                }
                (stmt, elab_ast::Stmt::Seq(mut seq2)) => {
                    seq2.push_front(stmt);
                    elab_ast::Stmt::Seq(seq2)
                }
                (head, tail) => {
                    let seq_rest = [head, tail].into();
                    elab_ast::Stmt::Seq(seq_rest)
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
        elaborate_stmts(&program.body).map(|s| s.into())
    }
}
