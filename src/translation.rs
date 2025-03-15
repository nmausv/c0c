// Translates elaborated AST into IR

pub mod tree;

use std::collections::VecDeque;

use crate::frontend::elab_ast::{self, OpType};
use crate::temps::{Label, Temp, TempFactory};

use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

struct ExpContext<'input> {
    tf: &'input mut TempFactory,
    stack: Vec<ExpFrame<'input>>,
    commands: Vec<tree::Command>,
    pure_exp: Option<tree::PureExp>,
}

#[derive(Debug)]
enum ArithExpFrame<'input> {
    Num(elab_ast::Num),
    True,
    False,
    Lvalue(elab_ast::Lvalue<'input>),
    PureBinop {
        binop: elab_ast::PureBinop,
        left: FrameProgress<elab_ast::Exp<'input>, tree::PureExp>,
        right: FrameProgress<elab_ast::Exp<'input>, tree::PureExp>,
    },
    ImpureBinop {
        binop: elab_ast::ImpureBinop,
        left: FrameProgress<elab_ast::Exp<'input>, tree::PureExp>,
        right: FrameProgress<elab_ast::Exp<'input>, tree::PureExp>,
    },
    Unop {
        op: elab_ast::Unop,
        exp: FrameProgress<elab_ast::Exp<'input>, ()>,
    },
    Ternary {
        cond_progress: FrameProgress<elab_ast::Exp<'input>, ()>,
        true_progress: FrameProgress<elab_ast::Exp<'input>, ()>,
        false_progress: FrameProgress<elab_ast::Exp<'input>, ()>,

        label_true: Label,
        label_false: Label,
        label_done: Label,
        value: Temp,
    },
}

impl<'input> ArithExpFrame<'input> {
    fn new(tf: &mut TempFactory, value: elab_ast::Exp<'input>) -> Self {
        match value {
            elab_ast::Exp::Num(n) => Self::Num(n),
            elab_ast::Exp::Lvalue(lvalue) => Self::Lvalue(lvalue),
            elab_ast::Exp::PureBinop(left, pure_binop, right) => {
                Self::PureBinop {
                    binop: pure_binop,
                    left: New(*left),
                    right: New(*right),
                }
            }
            elab_ast::Exp::ImpureBinop(left, impure_binop, right) => {
                Self::ImpureBinop {
                    binop: impure_binop,
                    left: New(*left),
                    right: New(*right),
                }
            }
            elab_ast::Exp::Unop(unop, exp) => Self::Unop {
                op: unop,
                exp: New(*exp),
            },
            elab_ast::Exp::True => Self::True,
            elab_ast::Exp::False => Self::False,
            elab_ast::Exp::Ternary {
                cond,
                exp_true,
                exp_false,
            } => Self::Ternary {
                cond_progress: New(*cond),
                true_progress: New(*exp_true),
                false_progress: New(*exp_false),
                label_true: tf.make_label(),
                label_false: tf.make_label(),
                label_done: tf.make_label(),
                value: tf.make_temp(),
            },
        }
    }

    #[allow(unused_variables)]
    fn recurse(self, ctx: &mut ExpContext<'input>) {
        match self {
            ArithExpFrame::Num(n) => ctx.pure_exp = Some(tree::PureExp::Num(n)),
            ArithExpFrame::True => ctx.pure_exp = Some(tree::PureExp::Num(1)),
            ArithExpFrame::False => ctx.pure_exp = Some(tree::PureExp::Num(0)),
            ArithExpFrame::Lvalue(lval) => {
                let elab_ast::Lvalue::Ident(name) = lval;
                ctx.pure_exp = Some(tree::PureExp::Ident(name.into()))
            }
            ArithExpFrame::PureBinop { binop, left, right }
                if binop.signature() == OpType::Arithmetic =>
            {
                match (left, right) {
                    (New(left), New(right)) => {
                        ctx.stack.push(ExpFrame::Arith(
                            ArithExpFrame::PureBinop {
                                binop,
                                left: InProgress,
                                right: New(right),
                            },
                        ));
                        ctx.stack.push(ExpFrame::new_arith(ctx.tf, left));
                    }
                    (InProgress, New(right)) => {
                        ctx.stack.push(ExpFrame::Arith(
                            ArithExpFrame::PureBinop {
                                binop,
                                left: Done(ctx.pure_exp.take().expect(
                                    "binop left should not ever be a base case",
                                )),
                                right: InProgress,
                            },
                        ));
                        ctx.stack.push(ExpFrame::new_arith(ctx.tf, right));
                    }
                    (Done(left), InProgress) => {
                        ctx.pure_exp = Some(tree::PureExp::PureBinop(
                            Box::new(left),
                            binop,
                            Box::new(ctx.pure_exp.take().expect(
                                "binop right should never be a base case",
                            )),
                        ));
                    }
                    _ => unreachable!(),
                }
            }
            ArithExpFrame::PureBinop { .. } => {
                unreachable!("possibly reachable?")
            }
            ArithExpFrame::ImpureBinop { binop, left, right } => {
                match (left, right) {
                    (New(left), New(right)) => {
                        ctx.stack.push(ExpFrame::Arith(
                            ArithExpFrame::ImpureBinop {
                                binop,
                                left: InProgress,
                                right: New(right),
                            },
                        ));
                        ctx.stack.push(ExpFrame::new_arith(ctx.tf, left));
                    }
                    (InProgress, New(right)) => {
                        ctx.stack.push(ExpFrame::Arith(ArithExpFrame::ImpureBinop {
                                    binop,
                                    left: Done(ctx.pure_exp.take().expect("impure binop left should not ever be a base case")),
                                    right: InProgress,
                                }));
                        ctx.stack.push(ExpFrame::new_arith(ctx.tf, right));
                    }
                    (Done(left), InProgress) => {
                        let temp = ctx.tf.make_temp();
                        ctx.commands.push(tree::Command::StoreImpureBinop {
                            dest: temp.clone().into(),
                            left,
                            op: binop,
                            right: ctx.pure_exp.take().expect("impure binop right should not ever be a base case"),
                        });
                        ctx.pure_exp = Some(tree::PureExp::Ident(temp.into()));
                    }
                    _ => unreachable!(),
                }
            }
            ArithExpFrame::Unop { op, exp }
                if op.signature() == OpType::Arithmetic =>
            {
                match exp {
                    New(exp) => {
                        ctx.stack.push(ExpFrame::Arith(ArithExpFrame::Unop {
                            op,
                            exp: Done(()),
                        }));
                        ctx.stack.push(ExpFrame::new_arith(ctx.tf, exp));
                    }
                    InProgress => unreachable!(),
                    Done(()) => {
                        ctx.pure_exp =
                            Some(tree::PureExp::Unop(
                                op,
                                Box::new(ctx.pure_exp.take().expect(
                                    "unop should never be a base case",
                                )),
                            ));
                    }
                }
            }
            ArithExpFrame::Unop { op, exp } => todo!(),
            ArithExpFrame::Ternary {
                cond_progress,
                true_progress,
                false_progress,
                label_true,
                label_false,
                label_done,
                value,
            } => match (cond_progress, true_progress, false_progress) {
                (New(cond), New(true_exp), New(false_exp)) => {
                    ctx.stack.push(ExpFrame::Arith(ArithExpFrame::Ternary {
                        cond_progress: InProgress,
                        true_progress: New(true_exp),
                        false_progress: New(false_exp),
                        label_true: label_true.clone(),
                        label_false: label_false.clone(),
                        label_done,
                        value,
                    }));
                    ctx.stack.push(ExpFrame::new_bool(
                                ctx.tf,
                                cond,
                                label_true,
                                label_false,
                            ).expect("non booleans should have been caught in type check for use in conditional"));
                }
                (InProgress, New(true_exp), New(false_exp)) => {
                    // commands has computations which jump to `label_true` if `cond` is true,
                    // and jumps to `label_false` if `cond` is false
                    ctx.stack.push(ExpFrame::Arith(ArithExpFrame::Ternary {
                        cond_progress: Done(()),
                        true_progress: InProgress,
                        false_progress: New(false_exp),
                        label_true: label_true.clone(),
                        label_false,
                        label_done,
                        value,
                    }));
                    ctx.commands.push(tree::Command::Label(label_true));
                    ctx.stack.push(ExpFrame::new_arith(ctx.tf, true_exp));
                }
                (Done(cond), InProgress, New(false_exp)) => {
                    // finish the true branch
                    ctx.commands.push(tree::Command::Store(
                        value.clone().into(),
                        ctx.pure_exp
                            .take()
                            .expect("ternary true should never be base case"),
                    ));
                    ctx.commands.push(tree::Command::Goto(label_done.clone()));
                    // start the false branch
                    ctx.commands
                        .push(tree::Command::Label(label_false.clone()));
                    ctx.stack.push(ExpFrame::Arith(ArithExpFrame::Ternary {
                        cond_progress: Done(cond),
                        true_progress: Done(()),
                        false_progress: InProgress,
                        label_true,
                        label_false,
                        label_done,
                        value,
                    }));
                    ctx.stack.push(ExpFrame::new_arith(ctx.tf, false_exp));
                }
                (Done(()), Done(()), InProgress) => {
                    // finish the false branch
                    ctx.commands.push(tree::Command::Store(
                        value.into(),
                        ctx.pure_exp
                            .take()
                            .expect("ternary false should never be base case"),
                    ));
                    ctx.commands.push(tree::Command::Goto(label_done.clone()));
                    // set the done label
                    ctx.commands
                        .push(tree::Command::Label(label_false.clone()));
                }
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug)]
enum BoolExpFrame<'input> {
    True {
        label_true: Label,
        label_false: Label,
    },
    False {
        label_true: Label,
        label_false: Label,
    },
    Unop {
        label_true: Label,
        label_false: Label,
        exp: elab_ast::Exp<'input>,
    },
    PureBinop {
        label_true: Label,
        label_false: Label,
        left: FrameProgress<elab_ast::Exp<'input>, ()>,
        op: elab_ast::PureBinop,
        right: FrameProgress<elab_ast::Exp<'input>, ()>,
    },
    Ternary {
        label_true: Label,
        label_false: Label,
        ter_true: Label,
        ter_false: Label,
        cond: FrameProgress<elab_ast::Exp<'input>, ()>,
        true_exp: FrameProgress<elab_ast::Exp<'input>, ()>,
        false_exp: FrameProgress<elab_ast::Exp<'input>, ()>,
    },
    Lvalue {
        label_true: Label,
        label_false: Label,
        progress: FrameProgress<elab_ast::Lvalue<'input>, ()>,
    },
}

impl<'input> BoolExpFrame<'input> {
    fn new(
        tf: &mut TempFactory,
        value: elab_ast::Exp<'input>,
        label_true: Label,
        label_false: Label,
    ) -> Result<Self, ()> {
        match value {
            elab_ast::Exp::Lvalue(lvalue) => Ok(Self::Lvalue {
                label_true,
                label_false,
                progress: New(lvalue),
            }),
            elab_ast::Exp::PureBinop(left, op, right)
                if op.signature() != OpType::Arithmetic =>
            {
                Ok(Self::PureBinop {
                    label_true,
                    label_false,
                    left: New(*left),
                    op,
                    right: New(*right),
                })
            }
            elab_ast::Exp::Unop(elab_ast::Unop::LogNegate, exp) => {
                Ok(Self::Unop {
                    label_true,
                    label_false,
                    exp: *exp,
                })
            }
            elab_ast::Exp::True => Ok(Self::True {
                label_true,
                label_false,
            }),
            elab_ast::Exp::False => Ok(Self::False {
                label_true,
                label_false,
            }),
            elab_ast::Exp::Ternary {
                cond,
                exp_true,
                exp_false,
            } => Ok(Self::Ternary {
                label_true,
                label_false,
                ter_true: tf.make_label(),
                ter_false: tf.make_label(),
                cond: New(*cond),
                true_exp: New(*exp_true),
                false_exp: New(*exp_false),
            }),
            elab_ast::Exp::Num(..) => Err(()),
            elab_ast::Exp::Unop(..) => Err(()),
            elab_ast::Exp::PureBinop(..) => Err(()),
            elab_ast::Exp::ImpureBinop(..) => Err(()),
        }
    }

    #[allow(unused_variables)]
    fn recurse(self, ctx: &mut ExpContext<'input>) {
        match self {
            BoolExpFrame::True {
                label_true,
                label_false: _,
            } => ctx.commands.push(tree::Command::Goto(label_true)),
            BoolExpFrame::False {
                label_true: _,
                label_false,
            } => ctx.commands.push(tree::Command::Goto(label_false)),
            BoolExpFrame::Unop {
                label_true,
                label_false,
                exp,
            } => {
                // note the labels switched
                ctx.stack.push(ExpFrame::Bool(
                    BoolExpFrame::new(ctx.tf, exp, label_false, label_true)
                        .expect("non boolean expression should never be logically negated"),
                ));
            }
            BoolExpFrame::PureBinop {
                label_true,
                label_false,
                left,
                op,
                right,
            } => todo!(),
            BoolExpFrame::Ternary {
                label_true,
                label_false,
                ter_true,
                ter_false,
                cond,
                true_exp,
                false_exp,
            } => todo!(),
            BoolExpFrame::Lvalue {
                label_true,
                label_false,
                progress,
            } => todo!(),
        }
    }
}

enum ExpFrame<'input> {
    Arith(ArithExpFrame<'input>),
    Bool(BoolExpFrame<'input>),
}

impl<'input> ExpFrame<'input> {
    fn new_arith(tf: &mut TempFactory, exp: elab_ast::Exp<'input>) -> Self {
        Self::Arith(ArithExpFrame::new(tf, exp))
    }

    fn new_bool(
        tf: &mut TempFactory,
        exp: elab_ast::Exp<'input>,
        label_true: Label,
        label_false: Label,
    ) -> Result<Self, ()> {
        Ok(Self::Bool(BoolExpFrame::new(
            tf,
            exp,
            label_true,
            label_false,
        )?))
    }

    fn recurse(self, ctx: &mut ExpContext<'input>) {
        match self {
            ExpFrame::Arith(frame) => frame.recurse(ctx),
            ExpFrame::Bool(frame) => frame.recurse(ctx),
        }
    }
}

impl<'input> elab_ast::Exp<'input> {
    fn translate_bool_recursive(
        self,
        tf: &mut TempFactory,
        branch_true: Label,
        branch_false: Label,
    ) -> Vec<tree::Command> {
        match self {
            elab_ast::Exp::True => vec![tree::Command::Goto(branch_true)],
            elab_ast::Exp::False => vec![tree::Command::Goto(branch_false)],
            elab_ast::Exp::Unop(elab_ast::Unop::LogNegate, exp) => {
                exp.translate_bool_recursive(tf, branch_false, branch_true)
            }
            elab_ast::Exp::PureBinop(e1, op, e2)
                if op.signature() != OpType::Arithmetic =>
            {
                let (mut c1, p1) = e1.translate_recursive(tf);
                let (mut c2, p2) = e2.translate_recursive(tf);
                c1.append(&mut c2);
                c1.push(tree::Command::If {
                    left: p1,
                    comp: op.into(),
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

                let mut commands = cond.translate_bool_recursive(
                    tf,
                    ter_branch_true.clone(),
                    ter_branch_false.clone(),
                );

                let mut ter_true = exp_true.translate_bool_recursive(
                    tf,
                    branch_true.clone(),
                    branch_false.clone(),
                );

                let mut ter_false = exp_false.translate_bool_recursive(
                    tf,
                    branch_true,
                    branch_false,
                );

                commands.push(tree::Command::Label(ter_branch_true));
                commands.append(&mut ter_true);

                commands.push(tree::Command::Label(ter_branch_false));
                commands.append(&mut ter_false);

                commands
            }
            elab_ast::Exp::Lvalue(lval) => {
                let (mut commands, pure) =
                    elab_ast::Exp::Lvalue(lval).translate_recursive(tf);
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
            elab_ast::Exp::Unop(..) => unreachable!(),
            elab_ast::Exp::ImpureBinop(..) => unreachable!(),
            elab_ast::Exp::Num(..) => unreachable!(),
            elab_ast::Exp::PureBinop(..) => unreachable!(),
        }
    }

    fn translate_recursive(
        self,
        tf: &mut TempFactory,
    ) -> (Vec<tree::Command>, tree::PureExp) {
        match self {
            elab_ast::Exp::Num(n) => (vec![], tree::PureExp::Num(n)),
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
                let (mut c1, p1) = e1.translate_recursive(tf);
                let (mut c2, p2) = e2.translate_recursive(tf);
                c1.append(&mut c2);
                (
                    c1,
                    tree::PureExp::PureBinop(Box::new(p1), binop, Box::new(p2)),
                )
            }
            elab_ast::Exp::Unop(op, exp)
                if op.signature() == OpType::Arithmetic =>
            {
                let (commands, pure) = exp.translate_recursive(tf);
                (commands, tree::PureExp::Unop(op, Box::new(pure)))
            }
            elab_ast::Exp::ImpureBinop(e1, binop, e2) => {
                let (mut c1, p1) = e1.translate_recursive(tf);
                let (mut c2, p2) = e2.translate_recursive(tf);
                c1.append(&mut c2);
                let t1 = tf.make_temp();
                c1.push(tree::Command::StoreImpureBinop {
                    dest: t1.clone().into(),
                    left: p1,
                    op: binop,
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

                let mut commands = cond.translate_bool_recursive(
                    tf,
                    branch_true.clone(),
                    branch_false.clone(),
                );

                let (mut c1, p1) = exp_true.translate_recursive(tf);
                let (mut c2, p2) = exp_false.translate_recursive(tf);

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

                let mut commands = exp.translate_bool_recursive(
                    tf,
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

    fn translate_iterative(
        self,
        tf: &mut TempFactory,
    ) -> (Vec<tree::Command>, tree::PureExp) {
        let mut ctx = ExpContext {
            tf,
            stack: Vec::new(),
            commands: Vec::new(),
            pure_exp: None,
        };

        ctx.stack.push(ExpFrame::new_arith(ctx.tf, self));

        while let Some(frame) = ctx.stack.pop() {
            frame.recurse(&mut ctx);
        }

        (
            ctx.commands,
            ctx.pure_exp.expect("cannot translate empty expression"),
        )
    }

    fn translate(
        self,
        tf: &mut TempFactory,
    ) -> (Vec<tree::Command>, tree::PureExp) {
        self.translate_iterative(tf)
    }
}

enum StmtFrame<'input> {
    Seq {
        list: VecDeque<elab_ast::Stmt<'input>>,
        next: usize,
    },
    If {
        label_true: Label,
        body_true: FrameProgress<elab_ast::Stmt<'input>, ()>,

        label_false: Label,
        body_false: FrameProgress<elab_ast::Stmt<'input>, ()>,

        label_done: Label,
    },
    While {
        label_cond: Label,
        cond: FrameProgress<elab_ast::Exp<'input>, ()>,

        label_body: Label,
        body: FrameProgress<elab_ast::Stmt<'input>, ()>,

        done: Label,
    },
    Nop,
    Assign(elab_ast::Lvalue<'input>, elab_ast::Exp<'input>),
    Return(elab_ast::Exp<'input>),
    Declare(elab_ast::Stmt<'input>),
    Exp(elab_ast::Exp<'input>),
}

impl<'input> StmtFrame<'input> {
    fn new(tf: &mut TempFactory, value: elab_ast::Stmt<'input>) -> Self {
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
                body_true: New(*stmt_true),
                label_false: tf.make_label(),
                body_false: New(*stmt_false),
                label_done: tf.make_label(),
            },
            elab_ast::Stmt::While { cond, body } => Self::While {
                label_cond: tf.make_label(),
                cond: New(cond),
                label_body: tf.make_label(),
                body: New(*body),
                done: tf.make_label(),
            },
            elab_ast::Stmt::Exp(exp) => Self::Exp(exp),
        }
    }
}

impl<'input> elab_ast::Stmt<'input> {
    fn translate_iterative(
        self: elab_ast::Stmt<'input>,
        tf: &mut TempFactory,
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
                    label_false,
                    body_false,
                    label_done,
                } => match (body_true, body_false) {
                    (New(body_true), New(body_false)) => {
                        let child_frame = StmtFrame::new(tf, body_true);
                        stack.push(StmtFrame::If {
                            label_true: label_true.clone(),
                            body_true: InProgress,
                            label_false,
                            body_false: New(body_false),
                            label_done,
                        });
                        stack.push(child_frame);
                        commands.push(tree::Command::Label(label_true));
                    }
                    (InProgress, New(body_false)) => {
                        let parent_frame = StmtFrame::If {
                            label_true,
                            body_true: Done(()),
                            label_false: label_false.clone(),
                            body_false: InProgress,
                            label_done: label_done.clone(),
                        };
                        stack.push(parent_frame);

                        let child_frame = StmtFrame::new(tf, body_false);
                        stack.push(child_frame);
                        commands.push(tree::Command::Goto(label_done.clone()));
                        commands.push(tree::Command::Label(label_false));
                    }
                    (Done(()), InProgress) => {
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
                    done,
                } => match (cond, body) {
                    (New(cond), New(body)) => {
                        commands.push(tree::Command::Label(label_cond.clone()));
                        commands.append(&mut cond.translate_bool_recursive(
                            tf,
                            label_body.clone(),
                            done.clone(),
                        ));
                        commands.push(tree::Command::Label(label_body.clone()));

                        stack.push(StmtFrame::While {
                            label_cond: label_cond.clone(),
                            cond: Done(()),
                            label_body: label_body.clone(),
                            body: InProgress,
                            done,
                        });

                        let child_frame = StmtFrame::new(tf, body);
                        stack.push(child_frame);
                    }
                    (Done(()), InProgress) => {
                        commands.push(tree::Command::Goto(label_cond));
                        commands.push(tree::Command::Label(done));
                    }
                    _ => unreachable!(),
                },
                StmtFrame::Nop => (),
                StmtFrame::Assign(elab_ast::Lvalue::Ident(var), exp) => {
                    let (mut edown, eup) = exp.translate(tf);
                    commands.append(&mut edown);
                    commands.push(tree::Command::Store((*var).into(), eup));
                }
                StmtFrame::Return(exp) => {
                    let (mut edown, eup) = exp.translate(tf);
                    commands.append(&mut edown);
                    commands.push(tree::Command::Return(eup));
                }
                StmtFrame::Declare(scope) => {
                    let child_frame = StmtFrame::new(tf, scope);
                    stack.push(child_frame);
                }
                StmtFrame::Exp(exp) => {
                    let (mut edown, _) = exp.translate(tf);
                    commands.append(&mut edown);
                }
            }
        }

        commands
    }

    fn translate_recursive(
        self: elab_ast::Stmt<'input>,
        tf: &mut TempFactory,
    ) -> Vec<tree::Command> {
        match self {
            elab_ast::Stmt::Nop => vec![],
            elab_ast::Stmt::Seq(block) => block
                .into_iter()
                .flat_map(|s| s.translate_recursive(tf))
                .collect(),
            elab_ast::Stmt::Assign(elab_ast::Lvalue::Ident(var), e) => {
                let (mut edown, eup) = e.translate(tf);
                edown.push(tree::Command::Store((*var).into(), eup));
                edown
            }
            elab_ast::Stmt::Return(e) => {
                let (mut edown, eup) = e.translate(tf);
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

                let mut commands = cond.translate_bool_recursive(
                    tf,
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

                commands.append(&mut cond.translate_bool_recursive(
                    tf,
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
                let (edown, _) = exp.translate(tf);
                edown
            }
        }
    }
}

pub fn translate<'input>(
    elab: elab_ast::Program<'input>,
    tf: &mut TempFactory,
) -> tree::Program {
    let stmt: elab_ast::Stmt = elab.into();
    stmt.translate_iterative(tf).into()
}
