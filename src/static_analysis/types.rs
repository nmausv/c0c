use std::collections::HashMap;

use crate::frontend::elab_ast::{Exp, OpType, Program, Stmt, Type};

type TypeMap = HashMap<String, Type>;

impl Exp {
    fn synthesize(&self, types: &TypeMap) -> Option<Type> {
        match self {
            Exp::Num(_) => Some(Type::Int),
            Exp::True => Some(Type::Bool),
            Exp::False => Some(Type::Bool),
            Exp::Ident(x) => types.get(x).copied(),
            Exp::PureBinop(e1, eq, e2)
                if eq.signature() == OpType::Equality =>
            {
                if e1.synthesize(types)? == e2.synthesize(types)? {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            Exp::PureBinop(e1, op, e2) if op.signature() == OpType::Logical => {
                if e1.synthesize(types)? == Type::Bool
                    && e2.synthesize(types)? == Type::Bool
                {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            Exp::PureBinop(e1, op, e2)
                if op.signature() == OpType::Relational =>
            {
                if e1.synthesize(types)? == Type::Int
                    && e2.synthesize(types)? == Type::Int
                {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            Exp::PureBinop(e1, op, e2)
                if op.signature() == OpType::Arithmetic =>
            {
                if e1.synthesize(types)? == Type::Int
                    && e2.synthesize(types)? == Type::Int
                {
                    Some(Type::Int)
                } else {
                    None
                }
            }
            Exp::ImpureBinop(e1, _, e2) => {
                if e1.synthesize(types)? == Type::Int
                    && e2.synthesize(types)? == Type::Int
                {
                    Some(Type::Int)
                } else {
                    None
                }
            }
            Exp::UnOp(op, e) if op.signature() == OpType::Logical => {
                if e.synthesize(types)? == Type::Bool {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            Exp::UnOp(op, e) if op.signature() == OpType::Arithmetic => {
                if e.synthesize(types)? == Type::Int {
                    Some(Type::Int)
                } else {
                    None
                }
            }
            Exp::Ternary {
                cond,
                exp_true,
                exp_false,
            } => {
                let cond_type = cond.synthesize(types)?;
                if cond_type != Type::Bool {
                    return None;
                }

                let branch_type = exp_true.synthesize(types)?;

                if exp_false.synthesize(types)? == branch_type {
                    Some(branch_type)
                } else {
                    None
                }
            }
            exp => {
                panic!("unexpected expression in typecheck: {:?}", exp)
            }
        }
    }
}

fn check_stmt(types: &mut TypeMap, s: &Stmt, t: Type) -> bool {
    match s {
        Stmt::Return(e) => e.synthesize(types) == Some(t),
        Stmt::Assign(var, e) => {
            let exp_type = e.synthesize(types);
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
        Stmt::Exp(e) => e.synthesize(types).is_some(),
        Stmt::If {
            cond,
            stmt_true,
            stmt_false,
        } => {
            if cond.synthesize(types) == Some(Type::Bool) {
                check_stmt(types, stmt_true, t)
                    && check_stmt(types, stmt_false, t)
            } else {
                false
            }
        }
        Stmt::While { cond, body } => {
            if cond.synthesize(types) == Some(Type::Bool) {
                check_stmt(types, body, t)
            } else {
                false
            }
        }
    }
}

pub fn typecheck(s: &Program) -> bool {
    let mut types = HashMap::new();
    check_stmt(&mut types, s.as_ref(), Type::Int)
}
