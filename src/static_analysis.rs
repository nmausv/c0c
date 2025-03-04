mod initialization;
mod returns;
mod types;

use crate::frontend::elab_ast;

pub fn check(elab_program: &elab_ast::Program) -> bool {
    if elab_program.initialization_check().is_err() {
        return false;
    }

    if elab_program.return_check().is_err() {
        return false;
    }

    if elab_program.typecheck().is_err() {
        return false;
    }

    true
}
