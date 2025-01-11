use crate::frontend::elab_ast::{Program, Stmt};

fn return_check_statement(s: &Stmt) -> bool {
    match s {
        Stmt::Declare(_, _, scope) => return_check_statement(scope),
        Stmt::Assign(_, _) => false,
        Stmt::Nop => false,
        Stmt::Seq(v) => v.iter().any(return_check_statement),
        Stmt::Exp(_) => false,
        Stmt::While { .. } => false,
        Stmt::If {
            branch_true,
            branch_false,
            ..
        } => {
            return_check_statement(branch_true)
                && return_check_statement(branch_false)
        }
        Stmt::Return(_) => true,
    }
}

pub fn return_check(program: &Program) -> bool {
    return_check_statement(program.as_ref())
}
