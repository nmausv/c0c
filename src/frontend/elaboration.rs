// Elaborates parsed syntax into the first AST with semantic information

use std::collections::VecDeque;

use super::ast;
use super::elab_ast;

fn elaborate_binop(
    eleft: &ast::Exp,
    binop: &ast::BinOp,
    eright: &ast::Exp,
) -> elab_ast::Exp {
    let elab_left = Box::new(elaborate_exp(eleft));
    let elab_right = Box::new(elaborate_exp(eright));

    match binop {
        ast::BinOp::LogAnd => elab_ast::Exp::Ternary {
            cond: elab_left,
            exp_true: elab_right,
            exp_false: Box::new(elab_ast::Exp::False),
        },
        ast::BinOp::LogOr => elab_ast::Exp::Ternary {
            cond: elab_left,
            exp_true: Box::new(elab_ast::Exp::True),
            exp_false: elab_right,
        },
        ast_binop => match elab_ast::BinOp::try_from(*ast_binop) {
            Ok(elab_ast::BinOp::Pure(op)) => {
                elab_ast::Exp::PureBinop(elab_left, op, elab_right)
            }
            Ok(elab_ast::BinOp::Impure(op)) => {
                elab_ast::Exp::ImpureBinop(elab_left, op, elab_right)
            }
            _ => panic!(
                "attempted conversion of && or || into single elaborated binop"
            ),
        },
    }
}

fn elaborate_exp(exp: &ast::Exp) -> elab_ast::Exp {
    match exp {
        ast::Exp::Num(n) => elab_ast::Exp::Num(*n),
        ast::Exp::Ident(name) => elab_ast::Exp::Ident(name.clone()),
        ast::Exp::BinOp(e1, binop, e2) => elaborate_binop(e1, binop, e2),
        ast::Exp::UnOp(op, exp) => {
            elab_ast::Exp::UnOp(*op, Box::new(elaborate_exp(exp)))
        }
        ast::Exp::True => elab_ast::Exp::True,
        ast::Exp::False => elab_ast::Exp::False,
        ast::Exp::Ternary {
            cond,
            branch_true,
            branch_false,
        } => elab_ast::Exp::Ternary {
            cond: Box::new(elaborate_exp(cond)),
            exp_true: Box::new(elaborate_exp(branch_true)),
            exp_false: Box::new(elaborate_exp(branch_false)),
        },
    }
}

/// elaborates a single statement, but since `Declare` and `DeclareAssign`
/// have unique interactions with subsequent statements, these statements
/// cannot be evaluated individually, using this function.
fn elaborate_stmt(stmt: &ast::Stmt) -> elab_ast::Stmt {
    match stmt {
        ast::Stmt::Declare(_, _) => panic!(),
        ast::Stmt::DeclareAssign(_, _, _) => panic!(),
        ast::Stmt::Block(b) => elaborate_stmts(b),
        ast::Stmt::Assign(name, asnop, exp) => {
            let elab_exp = match asnop {
                ast::AsnOp::Eq => elaborate_exp(exp),
                ast::AsnOp::PlusEq => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::PureBinOp::Plus,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::MinusEq => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::PureBinOp::Minus,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::TimesEq => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::PureBinOp::Times,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::DivEq => elab_ast::Exp::ImpureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::ImpureBinOp::Divide,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::ModEq => elab_ast::Exp::ImpureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::ImpureBinOp::Modulo,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::AndEq => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::PureBinOp::BitAnd,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::XorEq => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::PureBinOp::BitXor,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::OrEq => elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::PureBinOp::BitOr,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::ShlEq => elab_ast::Exp::ImpureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::ImpureBinOp::Shl,
                    Box::new(elaborate_exp(exp)),
                ),
                ast::AsnOp::ShrEq => elab_ast::Exp::ImpureBinop(
                    Box::new(elab_ast::Exp::Ident(name.clone())),
                    elab_ast::ImpureBinOp::Shr,
                    Box::new(elaborate_exp(exp)),
                ),
            };
            elab_ast::Stmt::Assign(name.clone(), elab_exp)
        }
        ast::Stmt::PostOp(var, postop) => match postop {
            ast::PostOp::PlusPlus => elab_ast::Stmt::Assign(
                var.clone(),
                elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(var.clone())),
                    elab_ast::PureBinOp::Plus,
                    Box::new(elab_ast::Exp::Num(1)),
                ),
            ),
            ast::PostOp::MinusMinus => elab_ast::Stmt::Assign(
                var.clone(),
                elab_ast::Exp::PureBinop(
                    Box::new(elab_ast::Exp::Ident(var.clone())),
                    elab_ast::PureBinOp::Minus,
                    Box::new(elab_ast::Exp::Num(1)),
                ),
            ),
        },
        ast::Stmt::Return(exp) => elab_ast::Stmt::Return(elaborate_exp(exp)),
        ast::Stmt::While { cond, body } => elab_ast::Stmt::While {
            cond: elaborate_exp(cond),
            body: Box::new(elaborate_stmt(body)),
        },
        ast::Stmt::For {
            init,
            cond,
            step,
            body,
        } => {
            let elab_cond = elaborate_exp(cond);
            let elab_body = elaborate_stmt(body);

            let new_body: elab_ast::Stmt;

            // elaborate the step and put it after the body
            if let Some(step) = step {
                new_body = match (&**step, elab_body) {
                    (ast::Stmt::Declare(_, _), _) => {
                        panic!("step cannot be declaration in for loop")
                    }
                    (ast::Stmt::DeclareAssign(_, _, _), _) => {
                        panic!("step cannot be declaration in for loop")
                    }
                    (step, elab_ast::Stmt::Seq(mut v)) => {
                        v.push_back(elaborate_stmt(step));
                        elab_ast::Stmt::Seq(v)
                    }
                    (step, body) => {
                        elab_ast::Stmt::Seq([body, elaborate_stmt(step)].into())
                    }
                };
            } else {
                new_body = elab_body;
            }

            if let Some(init) = init {
                match &**init {
                    ast::Stmt::Declare(name, t) => elab_ast::Stmt::Declare(
                        name.clone(),
                        *t,
                        Box::new(elab_ast::Stmt::While {
                            cond: elab_cond,
                            body: Box::new(new_body),
                        }),
                    ),
                    ast::Stmt::DeclareAssign(name, t, exp) => {
                        elab_ast::Stmt::Declare(
                            name.clone(),
                            *t,
                            Box::new(elab_ast::Stmt::Seq(
                                [
                                    elab_ast::Stmt::Assign(
                                        name.clone(),
                                        elaborate_exp(exp),
                                    ),
                                    elab_ast::Stmt::While {
                                        cond: elab_cond,
                                        body: Box::new(new_body),
                                    },
                                ]
                                .into(),
                            )),
                        )
                    }
                    _ => elab_ast::Stmt::Seq(
                        [
                            elaborate_stmt(init),
                            elab_ast::Stmt::While {
                                cond: elab_cond,
                                body: Box::new(new_body),
                            },
                        ]
                        .into(),
                    ),
                }
            } else {
                match step {
                    Some(_) => todo!(),
                    None => todo!(),
                }
            }
        }
        ast::Stmt::If {
            cond,
            branch_true,
            branch_false,
        } => {
            let elab_false = match branch_false {
                Some(branch_false) => elaborate_stmt(branch_false),
                None => elab_ast::Stmt::Nop,
            };
            elab_ast::Stmt::If {
                cond: elaborate_exp(cond),
                stmt_true: Box::new(elaborate_stmt(branch_true)),
                stmt_false: Box::new(elab_false),
            }
        }
        ast::Stmt::Exp(exp) => elab_ast::Stmt::Exp(elaborate_exp(exp)),
    }
}

fn elaborate_stmts(stmts: &[ast::Stmt]) -> elab_ast::Stmt {
    if stmts.is_empty() {
        return elab_ast::Stmt::Nop;
    }

    let (first, rest) = match stmts.split_first() {
        Some((first, rest)) => (first, rest),
        None => return elab_ast::Stmt::Nop,
    };

    // both declares and blocks don't (necessarily) want a sequence of the
    // elaborated tail, so we defer that elaboration

    if let ast::Stmt::Declare(name, t) = first {
        return elab_ast::Stmt::Declare(
            name.clone(),
            *t,
            Box::new(elaborate_stmts(rest)),
        );
    }

    if let ast::Stmt::Block(b) = first {
        return match (elaborate_stmts(b), elaborate_stmts(rest)) {
            (elab_ast::Stmt::Seq(mut seq1), elab_ast::Stmt::Seq(mut seq2)) => {
                seq1.append(&mut seq2);
                elab_ast::Stmt::Seq(seq1)
            }
            (elab_ast::Stmt::Seq(mut seq1), stm) => {
                seq1.push_back(stm);
                elab_ast::Stmt::Seq(seq1)
            }
            (stm, elab_ast::Stmt::Seq(mut seq2)) => {
                seq2.push_front(stm);
                elab_ast::Stmt::Seq(seq2)
            }
            (head, tail) => {
                let seq_rest = [head, tail].into();
                elab_ast::Stmt::Seq(seq_rest)
            }
        };
    }

    let mut seq_rest = match elaborate_stmts(rest) {
        elab_ast::Stmt::Seq(v) => v,
        elab_ast::Stmt::Nop => VecDeque::new(),
        s => VecDeque::from(vec![s]),
    };

    if let ast::Stmt::DeclareAssign(name, t, exp) = first {
        seq_rest.push_front(elab_ast::Stmt::Assign(
            name.clone(),
            elaborate_exp(exp),
        ));
        return elab_ast::Stmt::Declare(
            name.clone(),
            *t,
            Box::new(elab_ast::Stmt::Seq(seq_rest)),
        );
    }

    // note: by now we know that first is not a Declare, Block, or DeclareAssign
    // since those branches all returned early
    seq_rest.push_front(elaborate_stmt(first));

    elab_ast::Stmt::Seq(seq_rest)
}

pub fn elaborate(program: ast::Program) -> elab_ast::Program {
    // assert that main function is identified as main
    assert!(program.name == "main");

    elaborate_stmts(&program.body).into()
}
