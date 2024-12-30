mod initialization;
mod types;

use crate::frontend::elab_ast;
use crate::static_analysis::initialization::initialization_check;
use crate::static_analysis::types::typecheck;

pub fn check(elab_program: &elab_ast::Stmt) -> bool {
    if !initialization_check(elab_program) {
        return false;
    }

    if !typecheck(elab_program) {
        return false;
    }

    return true;
}
