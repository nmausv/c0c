mod initialization;
mod returns;
mod types;

use crate::frontend::elab_ast;
use initialization::initialization_check;
use returns::return_check;
use types::typecheck;

pub fn check(elab_program: &elab_ast::Program) -> bool {
    if !initialization_check(elab_program) {
        return false;
    }
    println!("program passed initialization check");

    if !return_check(elab_program) {
        return false;
    }
    println!("program passed return check");

    if !typecheck(elab_program) {
        return false;
    }
    println!("program passed type check");


    return true;
}
