// Translates elaborated AST into IR

pub mod tree;

use crate::frontend::elab_ast;

fn translate_exp(
    tf: &mut crate::temps::TempFactory,
    exp: elab_ast::Exp,
) -> (Vec<tree::Command>, tree::PureExp) {
    match exp {
        elab_ast::Exp::Num(n) => (vec![], tree::PureExp::Num(n)),
        elab_ast::Exp::Ident(x) => (vec![], tree::PureExp::Ident(x)),
        elab_ast::Exp::PureBinop(e1, binop, e2) => {
            let (mut c1, p1) = translate_exp(tf, *e1);
            let (mut c2, p2) = translate_exp(tf, *e2);
            c1.append(&mut c2);
            (
                c1,
                tree::PureExp::PureBinOp(Box::new(p1), binop, Box::new(p2)),
            )
        }
        elab_ast::Exp::ImpureBinop(e1, binop, e2) => {
            let (mut c1, p1) = translate_exp(tf, *e1);
            let (mut c2, p2) = translate_exp(tf, *e2);
            c1.append(&mut c2);
            let t1 = tf.make_temp();
            c1.push(tree::Command::StoreImpureBinOp(t1.clone(), p1, binop, p2));
            (c1, tree::PureExp::Ident(t1))
        }
        _ => todo!(),
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
        _ => todo!(),
    }
}

pub fn translate(
    elab: elab_ast::Program,
    tf: &mut crate::temps::TempFactory,
) -> tree::Program {
    tree::Program(translate_stmt(tf, elab.into()))
}
