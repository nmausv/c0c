mod initialization;
mod returns;
mod types;

use crate::frontend::elab_ast;
use initialization::initialization_check;
use returns::return_check;
use types::typecheck;

pub fn check(elab_program: &elab_ast::Program) -> bool {
    if initialization_check(elab_program).is_err() {
        return false;
    }

    if return_check(elab_program).is_err() {
        return false;
    }

    if typecheck(elab_program).is_err() {
        return false;
    }

    true
}
