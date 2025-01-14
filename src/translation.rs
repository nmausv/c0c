// Translates elaborated AST into IR

pub mod tree;

use crate::frontend::elab_ast::{self, OpType};
use crate::temps::{Label, TempFactory};

fn translate_bool(
    tf: &mut TempFactory,
    boolexp: elab_ast::Exp,
    branch_true: Label,
    branch_false: Label,
) -> Vec<tree::Command> {
    match boolexp {
        elab_ast::Exp::Num(1) => vec![tree::Command::Goto(branch_true)],
        elab_ast::Exp::Num(0) => vec![tree::Command::Goto(branch_false)],
        elab_ast::Exp::UnOp(elab_ast::UnOp::LogNegate, exp) => {
            translate_bool(tf, *exp, branch_false, branch_true)
        }
        elab_ast::Exp::PureBinop(e1, op, e2)
            if op.signature() != OpType::Arithmetic =>
        {
            let (mut c1, p1) = translate_exp(tf, *e1);
            let (mut c2, p2) = translate_exp(tf, *e2);
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

            let mut commands = translate_bool(
                tf,
                *cond,
                ter_branch_true.clone(),
                ter_branch_false.clone(),
            );

            let mut ter_true = translate_bool(
                tf,
                *exp_true,
                branch_true.clone(),
                branch_false.clone(),
            );

            let mut ter_false =
                translate_bool(tf, *exp_false, branch_true, branch_false);

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
                comp: elab_ast::BinOp::Pure(elab_ast::PureBinOp::NotEq),
                right: tree::PureExp::Num(0),
                branch_true,
                branch_false,
            });
            commands
        }
    }
}

fn translate_exp(
    tf: &mut crate::temps::TempFactory,
    exp: elab_ast::Exp,
) -> (Vec<tree::Command>, tree::PureExp) {
    match exp {
        elab_ast::Exp::Num(n) => (vec![], tree::PureExp::Num(n)),
        elab_ast::Exp::True => (vec![], tree::PureExp::Num(1)),
        elab_ast::Exp::False => (vec![], tree::PureExp::Num(0)),
        elab_ast::Exp::Ident(x) => (vec![], tree::PureExp::Ident(x)),
        // Note that logical operations like (a && b) require possibly short circuit
        // evaluation, so they cannot be translated like arithmetic operations
        // like (a + b), which always require computing both a and b
        elab_ast::Exp::PureBinop(e1, binop, e2)
            if binop.signature() == OpType::Arithmetic =>
        {
            let (mut c1, p1) = translate_exp(tf, *e1);
            let (mut c2, p2) = translate_exp(tf, *e2);
            c1.append(&mut c2);
            (
                c1,
                tree::PureExp::PureBinOp(Box::new(p1), binop, Box::new(p2)),
            )
        }
        elab_ast::Exp::UnOp(op, exp)
            if op.signature() == OpType::Arithmetic =>
        {
            let (commands, pure) = translate_exp(tf, *exp);
            (commands, tree::PureExp::UnOp(op, Box::new(pure)))
        }
        elab_ast::Exp::ImpureBinop(e1, binop, e2) => {
            let (mut c1, p1) = translate_exp(tf, *e1);
            let (mut c2, p2) = translate_exp(tf, *e2);
            c1.append(&mut c2);
            let t1 = tf.make_temp();
            c1.push(tree::Command::StoreImpureBinOp {
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

            let mut commands = translate_bool(
                tf,
                *cond,
                branch_true.clone(),
                branch_false.clone(),
            );

            let (mut c1, p1) = translate_exp(tf, *exp_true);
            let (mut c2, p2) = translate_exp(tf, *exp_false);

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

fn translate_stmt(
    tf: &mut crate::temps::TempFactory,
    s: elab_ast::Stmt,
) -> Vec<tree::Command> {
    match s {
        elab_ast::Stmt::Nop => vec![],
        elab_ast::Stmt::Seq(block) => block
            .into_iter()
            .flat_map(|s| translate_stmt(tf, s))
            .collect(),
        elab_ast::Stmt::Assign(var, e) => {
            let (mut edown, eup) = translate_exp(tf, e);
            edown.push(tree::Command::Store(var, eup));
            edown
        }
        elab_ast::Stmt::Return(e) => {
            let (mut edown, eup) = translate_exp(tf, e);
            edown.push(tree::Command::Return(eup));
            edown
        }
        elab_ast::Stmt::Declare(_, _, scope) => translate_stmt(tf, *scope),
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

            let mut translated_true = translate_stmt(tf, *stmt_true);
            let mut translated_false = translate_stmt(tf, *stmt_false);

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

            let mut commands = vec![tree::Command::Label(cond_body.clone())];
            commands.append(&mut translate_bool(
                tf,
                cond,
                while_body.clone(),
                done.clone(),
            ));

            commands.push(tree::Command::Label(while_body));
            commands.append(&mut translate_stmt(tf, *body));
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

pub fn translate(
    elab: elab_ast::Program,
    tf: &mut crate::temps::TempFactory,
) -> tree::Program {
    translate_stmt(tf, elab.into()).into()
}
