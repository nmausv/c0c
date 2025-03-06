// Translates elaborated AST into IR

pub mod tree;

use std::collections::VecDeque;

use crate::frontend::elab_ast::{self, OpType};
use crate::temps::{Label, TempFactory};

use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

fn translate_bool<'input>(
    tf: &'input mut TempFactory,
    boolexp: &'input elab_ast::Exp<'input>,
    branch_true: Label,
    branch_false: Label,
) -> Vec<tree::Command> {
    match boolexp {
        elab_ast::Exp::Num(1) => vec![tree::Command::Goto(branch_true)],
        elab_ast::Exp::Num(0) => vec![tree::Command::Goto(branch_false)],
        elab_ast::Exp::Unop(elab_ast::Unop::LogNegate, exp) => {
            translate_bool(tf, exp.as_ref(), branch_false, branch_true)
        }
        elab_ast::Exp::PureBinop(e1, op, e2)
            if op.signature() != OpType::Arithmetic =>
        {
            let (mut c1, p1) = translate_exp(tf, e1.as_ref());
            let (mut c2, p2) = translate_exp(tf, e2.as_ref());
            c1.append(&mut c2);
            c1.push(tree::Command::If {
                left: p1,
                comp: (*op).into(),
                right: p2,
                branch_true,
                branch_false,
            });
            c1
        }
        elab_ast::Exp::Ternary {
            cond,
            exp_true,
            exp_false,
        } => {
            // check cond, if true goto ter_true else goto ter_false
            // ter_true:
            // check exp_true, if true goto branch_true else goto branch_false
            // ter_false:
            // check exp_false, if true goto branch_true else goto branch_false

            let ter_branch_true = tf.make_label();
            let ter_branch_false = tf.make_label();

            let mut commands = translate_bool(
                tf,
                cond.as_ref(),
                ter_branch_true.clone(),
                ter_branch_false.clone(),
            );

            let mut ter_true = translate_bool(
                tf,
                exp_true.as_ref(),
                branch_true.clone(),
                branch_false.clone(),
            );

            let mut ter_false = translate_bool(
                tf,
                exp_false.as_ref(),
                branch_true,
                branch_false,
            );

            commands.push(tree::Command::Label(ter_branch_true));
            commands.append(&mut ter_true);

            commands.push(tree::Command::Label(ter_branch_false));
            commands.append(&mut ter_false);

            commands
        }
        exp => {
            let (mut commands, pure) = translate_exp(tf, exp);
            // if pure != 0
            // then goto branch_true
            // else goto branch_false
            commands.push(tree::Command::If {
                left: pure,
                comp: elab_ast::Binop::Pure(elab_ast::PureBinop::NotEq),
                right: tree::PureExp::Num(0),
                branch_true,
                branch_false,
            });
            commands
        }
    }
}

fn translate_exp<'input>(
    tf: &mut crate::temps::TempFactory,
    exp: &elab_ast::Exp<'input>,
) -> (Vec<tree::Command>, tree::PureExp) {
    match exp {
        elab_ast::Exp::Num(n) => (vec![], tree::PureExp::Num(*n)),
        elab_ast::Exp::True => (vec![], tree::PureExp::Num(1)),
        elab_ast::Exp::False => (vec![], tree::PureExp::Num(0)),
        elab_ast::Exp::Lvalue(elab_ast::Lvalue::Ident(x)) => {
            (vec![], tree::PureExp::Ident((*x).into()))
        }
        // Note that logical operations like (a && b) require short circuit
        // evaluation, so they cannot be translated like arithmetic operations
        // like (a + b), which always require computing both a and b
        elab_ast::Exp::PureBinop(e1, binop, e2)
            if binop.signature() == OpType::Arithmetic =>
        {
            let (mut c1, p1) = translate_exp(tf, e1.as_ref());
            let (mut c2, p2) = translate_exp(tf, e2.as_ref());
            c1.append(&mut c2);
            (
                c1,
                tree::PureExp::PureBinop(Box::new(p1), *binop, Box::new(p2)),
            )
        }
        elab_ast::Exp::Unop(op, exp)
            if op.signature() == OpType::Arithmetic =>
        {
            let (commands, pure) = translate_exp(tf, exp.as_ref());
            (commands, tree::PureExp::Unop(*op, Box::new(pure)))
        }
        elab_ast::Exp::ImpureBinop(e1, binop, e2) => {
            let (mut c1, p1) = translate_exp(tf, e1.as_ref());
            let (mut c2, p2) = translate_exp(tf, e2.as_ref());
            c1.append(&mut c2);
            let t1 = tf.make_temp();
            c1.push(tree::Command::StoreImpureBinop {
                dest: t1.clone().into(),
                left: p1,
                op: *binop,
                right: p2,
            });
            (c1, tree::PureExp::Ident(t1.into()))
        }
        elab_ast::Exp::Ternary {
            cond,
            exp_true,
            exp_false,
        } => {
            let branch_true = tf.make_label();
            let branch_false = tf.make_label();
            let branch_done = tf.make_label();

            let result = tf.make_temp();

            let mut commands = translate_bool(
                tf,
                cond.as_ref(),
                branch_true.clone(),
                branch_false.clone(),
            );

            let (mut c1, p1) = translate_exp(tf, exp_true.as_ref());
            let (mut c2, p2) = translate_exp(tf, exp_false.as_ref());

            commands.push(tree::Command::Label(branch_true));
            commands.append(&mut c1);
            commands.push(tree::Command::Store(result.clone().into(), p1));
            commands.push(tree::Command::Goto(branch_done.clone()));

            commands.push(tree::Command::Label(branch_false));
            commands.append(&mut c2);
            commands.push(tree::Command::Store(result.clone().into(), p2));
            commands.push(tree::Command::Goto(branch_done.clone()));

            commands.push(tree::Command::Label(branch_done));

            (commands, tree::PureExp::Ident(result.into()))
        }
        exp => {
            let branch_true = tf.make_label();
            let branch_false = tf.make_label();
            let branch_done = tf.make_label();

            let result = tf.make_temp();

            let mut commands = translate_bool(
                tf,
                exp,
                branch_true.clone(),
                branch_false.clone(),
            );

            // branch_true:
            // compute p1
            // result = p1
            // goto branch_done
            commands.push(tree::Command::Label(branch_true));
            commands.push(tree::Command::Store(
                result.clone().into(),
                tree::PureExp::Num(1),
            ));
            commands.push(tree::Command::Goto(branch_done.clone()));

            // branch_false:
            // compute p2
            // result = p2
            // goto branch_done
            commands.push(tree::Command::Label(branch_false));
            commands.push(tree::Command::Store(
                result.clone().into(),
                tree::PureExp::Num(0),
            ));
            commands.push(tree::Command::Goto(branch_done.clone()));

            // branch_done:
            commands.push(tree::Command::Label(branch_done));

            (commands, tree::PureExp::Ident(result.into()))
        }
    }
}

enum ExpFrame<'input> {
    Num(elab_ast::Num),
    True,
    False,
    Lvalue(elab_ast::Lvalue<'input>),
    PureBinop {
        binop: elab_ast::PureBinop,
        left: elab_ast::Exp<'input>,
        trans_left: FrameProgress<(Vec<tree::Command>, tree::PureExp)>,
        right: elab_ast::Exp<'input>,
        trans_right: FrameProgress<(Vec<tree::Command>, tree::PureExp)>,
    },
    ImpureBinop {
        binop: elab_ast::PureBinop,
        left: elab_ast::Exp<'input>,
        trans_left: FrameProgress<(Vec<tree::Command>, tree::PureExp)>,
        right: elab_ast::Exp<'input>,
        trans_right: FrameProgress<(Vec<tree::Command>, tree::PureExp)>,
    },
    Unop {},
    Ternary {},
}

enum StmtFrame<'input> {
    Seq {
        list: VecDeque<elab_ast::Stmt<'input>>,
        next: usize,
    },
    If {
        label_true: Label,
        body_true: elab_ast::Stmt<'input>,
        translated_true: FrameProgress<()>,

        label_false: Label,
        body_false: elab_ast::Stmt<'input>,
        translated_false: FrameProgress<()>,

        label_done: Label,
    },
    While {
        label_cond: Label,
        cond: elab_ast::Exp<'input>,
        label_body: Label,
        translated_body: FrameProgress<()>,
        body: elab_ast::Stmt<'input>,
        done: Label,
    },
    Nop,
    Assign(elab_ast::Lvalue<'input>, elab_ast::Exp<'input>),
    Return(elab_ast::Exp<'input>),
    Declare(elab_ast::Stmt<'input>),
    Exp(elab_ast::Exp<'input>),
}

impl<'input> StmtFrame<'input> {
    fn new(
        tf: &mut crate::temps::TempFactory,
        value: elab_ast::Stmt<'input>,
    ) -> Self {
        match value {
            elab_ast::Stmt::Declare(_, _, scope) => Self::Declare(*scope),
            elab_ast::Stmt::Assign(elab_ast::Lvalue::Ident(var), exp) => {
                Self::Assign((*var).into(), exp)
            }
            elab_ast::Stmt::Return(exp) => Self::Return(exp),
            elab_ast::Stmt::Seq(v) => Self::Seq { list: v, next: 0 },
            elab_ast::Stmt::Nop => Self::Nop,
            elab_ast::Stmt::If {
                cond: _,
                stmt_true,
                stmt_false,
            } => Self::If {
                label_true: tf.make_label(),
                body_true: *stmt_true,
                translated_true: New,
                label_false: tf.make_label(),
                body_false: *stmt_false,
                translated_false: New,
                label_done: tf.make_label(),
            },
            elab_ast::Stmt::While { cond, body } => Self::While {
                label_cond: tf.make_label(),
                cond,
                label_body: tf.make_label(),
                body: *body,
                translated_body: New,
                done: tf.make_label(),
            },
            elab_ast::Stmt::Exp(exp) => Self::Exp(exp),
        }
    }
}

impl<'input> elab_ast::Stmt<'input> {
    fn translate_iterative(
        self: elab_ast::Stmt<'input>,
        tf: &mut crate::temps::TempFactory,
    ) -> Vec<tree::Command> {
        let mut commands = Vec::new();
        let mut stack: Vec<StmtFrame> = Vec::new();
        stack.push(StmtFrame::new(tf, self));

        while let Some(frame) = stack.pop() {
            match frame {
                StmtFrame::Seq { mut list, next } => {
                    if next < list.len() {
                        // mem::take here to get ownership of the statement, so
                        // the statement isn't freed again when the Seq frame
                        // is dropped
                        let child_frame =
                            StmtFrame::new(tf, std::mem::take(&mut list[next]));
                        stack.push(StmtFrame::Seq {
                            list,
                            next: next + 1,
                        });
                        stack.push(child_frame);
                    }
                }
                StmtFrame::If {
                    label_true,
                    body_true,
                    translated_true,
                    label_false,
                    body_false,
                    translated_false,
                    label_done,
                } => match (translated_true, translated_false) {
                    (New, New) => {
                        let child_frame = StmtFrame::new(tf, body_true);
                        stack.push(StmtFrame::If {
                            label_true: label_true.clone(),
                            body_true: elab_ast::Stmt::default(),
                            translated_true: InProgress,
                            label_false,
                            body_false,
                            translated_false: New,
                            label_done,
                        });
                        stack.push(child_frame);
                        commands.push(tree::Command::Label(label_true));
                    }
                    (InProgress, New) => {
                        let parent_frame = StmtFrame::If {
                            label_true,
                            body_true,
                            translated_true: Done(()),
                            label_false: label_false.clone(),
                            body_false: elab_ast::Stmt::default(),
                            translated_false: InProgress,
                            label_done: label_done.clone(),
                        };
                        stack.push(parent_frame);

                        let child_frame = StmtFrame::new(tf, body_false);
                        stack.push(child_frame);
                        commands.push(tree::Command::Goto(label_done.clone()));
                        commands.push(tree::Command::Label(label_false));
                    }
                    (Done(_), InProgress) => {
                        commands.push(tree::Command::Goto(label_done.clone()));
                        commands.push(tree::Command::Label(label_done));
                    }
                    _ => unreachable!(),
                },
                StmtFrame::While {
                    label_cond,
                    cond,
                    label_body,
                    body,
                    translated_body,
                    done,
                } => match translated_body {
                    New => {
                        commands.push(tree::Command::Label(label_cond.clone()));
                        commands.append(&mut translate_bool(
                            tf,
                            &cond,
                            label_body.clone(),
                            done.clone(),
                        ));
                        commands.push(tree::Command::Label(label_body.clone()));

                        stack.push(StmtFrame::While {
                            label_cond: label_cond.clone(),
                            cond,
                            label_body: label_body.clone(),
                            translated_body: InProgress,
                            body: elab_ast::Stmt::default(),
                            done,
                        });

                        let child_frame = StmtFrame::new(tf, body);
                        stack.push(child_frame);
                    }
                    InProgress => unreachable!(),
                    Done(_) => {
                        commands.push(tree::Command::Goto(label_cond));
                        commands.push(tree::Command::Label(done));
                    }
                },
                StmtFrame::Nop => (),
                StmtFrame::Assign(elab_ast::Lvalue::Ident(var), exp) => {
                    let (mut edown, eup) = translate_exp(tf, &exp);
                    commands.append(&mut edown);
                    commands.push(tree::Command::Store((*var).into(), eup));
                }
                StmtFrame::Return(exp) => {
                    let (mut edown, eup) = translate_exp(tf, &exp);
                    commands.append(&mut edown);
                    commands.push(tree::Command::Return(eup));
                }
                StmtFrame::Declare(scope) => {
                    let child_frame = StmtFrame::new(tf, scope);
                    stack.push(child_frame);
                }
                StmtFrame::Exp(exp) => {
                    let (mut edown, _) = translate_exp(tf, &exp);
                    commands.append(&mut edown);
                }
            }
        }

        commands
    }

    fn translate_recursive(
        self: &elab_ast::Stmt<'input>,
        tf: &mut crate::temps::TempFactory,
    ) -> Vec<tree::Command> {
        match self {
            elab_ast::Stmt::Nop => vec![],
            elab_ast::Stmt::Seq(block) => block
                .iter()
                .flat_map(|s| s.translate_recursive(tf))
                .collect(),
            elab_ast::Stmt::Assign(elab_ast::Lvalue::Ident(var), e) => {
                let (mut edown, eup) = translate_exp(tf, e);
                edown.push(tree::Command::Store((*var).into(), eup));
                edown
            }
            elab_ast::Stmt::Return(e) => {
                let (mut edown, eup) = translate_exp(tf, e);
                edown.push(tree::Command::Return(eup));
                edown
            }
            elab_ast::Stmt::Declare(_, _, scope) => {
                scope.translate_recursive(tf)
            }
            elab_ast::Stmt::If {
                cond,
                stmt_true,
                stmt_false,
            } => {
                let branch_true = tf.make_label();
                let branch_false = tf.make_label();
                let branch_done = tf.make_label();

                let mut commands = translate_bool(
                    tf,
                    cond,
                    branch_true.clone(),
                    branch_false.clone(),
                );

                let mut translated_true = stmt_true.translate_recursive(tf);
                let mut translated_false = stmt_false.translate_recursive(tf);

                commands.push(tree::Command::Label(branch_true));
                commands.append(&mut translated_true);
                commands.push(tree::Command::Goto(branch_done.clone()));

                commands.push(tree::Command::Label(branch_false));
                commands.append(&mut translated_false);
                commands.push(tree::Command::Goto(branch_done.clone()));

                commands.push(tree::Command::Label(branch_done));

                commands
            }
            elab_ast::Stmt::While { cond, body } => {
                let cond_body = tf.make_label();
                let while_body = tf.make_label();
                let done = tf.make_label();

                // cond_body:
                // translate bool with label_true = while_body, label_false = done
                // while_body:
                // ...
                // goto cond_body
                // done:

                let mut commands =
                    vec![tree::Command::Label(cond_body.clone())];
                commands.append(&mut translate_bool(
                    tf,
                    cond,
                    while_body.clone(),
                    done.clone(),
                ));

                commands.push(tree::Command::Label(while_body));
                commands.append(&mut body.translate_recursive(tf));
                commands.push(tree::Command::Goto(cond_body));
                commands.push(tree::Command::Label(done));

                commands
            }
            elab_ast::Stmt::Exp(exp) => {
                // pure expression guaranteed to have no effects, can omit
                let (edown, _) = translate_exp(tf, exp);
                edown
            }
        }
    }
}

pub fn translate<'input>(
    elab: elab_ast::Program<'input>,
    tf: &mut crate::temps::TempFactory,
) -> tree::Program {
    let stmt: elab_ast::Stmt = elab.into();
    stmt.translate_iterative(tf).into()
}
