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
        ast::BinOp::Plus => elab_ast::Exp::PureBinop(
            elab_left,
            elab_ast::PureBinOp::Plus,
            elab_right,
        ),
        ast::BinOp::Minus => elab_ast::Exp::PureBinop(
            elab_left,
            elab_ast::PureBinOp::Minus,
            elab_right,
        ),
        ast::BinOp::Times => elab_ast::Exp::PureBinop(
            elab_left,
            elab_ast::PureBinOp::Times,
            elab_right,
        ),
        ast::BinOp::Divide => elab_ast::Exp::ImpureBinop(
            elab_left,
            elab_ast::ImpureBinOp::Divide,
            elab_right,
        ),
        ast::BinOp::Modulo => elab_ast::Exp::ImpureBinop(
            elab_left,
            elab_ast::ImpureBinOp::Modulo,
            elab_right,
        ),
    }
}

fn elaborate_exp(exp: &ast::Exp) -> elab_ast::Exp {
    match exp {
        ast::Exp::Num(n) => elab_ast::Exp::Num(*n),
        ast::Exp::Ident(name) => elab_ast::Exp::Ident(name.clone()),
        ast::Exp::BinOp(e1, binop, e2) => elaborate_binop(e1, binop, e2),
    }
}

fn elaborate_stmts(stmts: &[ast::Stmt]) -> elab_ast::Stmt {
    let (first, rest) = match stmts.split_first() {
        None => return elab_ast::Stmt::Nop,
        Some(x) => x,
    };

    let elab_rest = elaborate_stmts(rest);

    match first {
        ast::Stmt::Declare(name, t) => {
            elab_ast::Stmt::Declare(name.clone(), *t, Box::new(elab_rest))
        }
        ast::Stmt::DeclareAssign(name, t, exp) => {
            let mut seq_rest = match elab_rest {
                elab_ast::Stmt::Seq(v) => v,
                elab_ast::Stmt::Nop => VecDeque::new(),
                s => {
                    let mut new_rest = VecDeque::new();
                    new_rest.push_back(s);
                    new_rest
                }
            };
            seq_rest.push_front(elab_ast::Stmt::Assign(
                name.clone(),
                elaborate_exp(exp),
            ));
            elab_ast::Stmt::Declare(
                name.clone(),
                *t,
                Box::new(elab_ast::Stmt::Seq(seq_rest)),
            )
        }
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
            };
            let mut seq_rest = match elab_rest {
                elab_ast::Stmt::Seq(v) => v,
                elab_ast::Stmt::Nop => VecDeque::new(),
                s => {
                    let mut new_rest = VecDeque::new();
                    new_rest.push_back(s);
                    new_rest
                }
            };
            seq_rest.push_front(elab_ast::Stmt::Assign(name.clone(), elab_exp));
            elab_ast::Stmt::Seq(seq_rest)
        }
        ast::Stmt::Block(b) => {
            let new_first = elaborate_stmts(b);
            match (new_first, elab_rest) {
                (
                    elab_ast::Stmt::Seq(mut seq1),
                    elab_ast::Stmt::Seq(mut seq2),
                ) => {
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
                    let mut seq_rest = VecDeque::new();
                    seq_rest.push_back(head);
                    seq_rest.push_back(tail);
                    elab_ast::Stmt::Seq(seq_rest)
                }
            }
        }
        ast::Stmt::Return(exp) => {
            let mut seq_rest = match elab_rest {
                elab_ast::Stmt::Seq(v) => v,
                elab_ast::Stmt::Nop => VecDeque::new(),
                s => {
                    let mut new_rest = VecDeque::new();
                    new_rest.push_back(s);
                    new_rest
                }
            };
            seq_rest.push_front(elab_ast::Stmt::Return(elaborate_exp(exp)));
            elab_ast::Stmt::Seq(seq_rest)
        }
    }
}

pub fn elaborate(program: ast::Program) -> elab_ast::Stmt {
    // assert that main function is identified as main
    assert!(program.name == "main");

    elaborate_stmts(&program.body)
}
