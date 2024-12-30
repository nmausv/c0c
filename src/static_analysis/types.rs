use std::collections::HashMap;

use crate::frontend::elab_ast::{Exp, Stmt, Type};

type TypeMap = HashMap<String, Type>;

fn synthesize_exp(types: &TypeMap, exp: &Exp) -> Option<Type> {
    match exp {
        Exp::Num(_) => Some(Type::Int),
        Exp::Ident(x) => types.get(x).copied(),
        Exp::PureBinop(e1, _, e2) => {
            if synthesize_exp(types, e1) == Some(Type::Int)
                && synthesize_exp(types, e2) == Some(Type::Int)
            {
                Some(Type::Int)
            } else {
                None
            }
        }
        Exp::ImpureBinop(e1, _, e2) => {
            if synthesize_exp(types, e1) == Some(Type::Int)
                && synthesize_exp(types, e2) == Some(Type::Int)
            {
                Some(Type::Int)
            } else {
                None
            }
        }
    }
}

fn check_stmt(types: &mut TypeMap, s: &Stmt, t: Type) -> bool {
    match s {
        Stmt::Return(e) => synthesize_exp(types, &e) == Some(t),
        Stmt::Assign(var, e) => {
            let exp_type = synthesize_exp(types, e);
            let var_type = types.get(var);

            match (exp_type, var_type) {
                (Some(t1), Some(t2)) => t1 == *t2,
                _ => false,
            }
        }
        Stmt::Seq(block) => block.iter().all(|s| check_stmt(types, s, t)),
        Stmt::Nop => true,
        Stmt::Declare(var, var_type, scope) => {
            // disallow shadowing
            if types.contains_key(var) {
                return false;
            }
            types.insert(var.clone(), *var_type);
            let result = check_stmt(types, scope, t);
            types.remove(var);
            result
        }
    }
}

pub fn typecheck(s: &Stmt) -> bool {
    let mut types = HashMap::new();
    check_stmt(&mut types, s, Type::Int)
}
