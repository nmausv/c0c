use std::collections::HashMap;

use crate::frontend::elab_ast::{Exp, Lvalue, OpType, Program, Stmt, Type};

type TypeMap<'input> = HashMap<&'input str, Type>;

impl Exp<'_> {
    fn synthesize(&self, types: &TypeMap) -> Result<Type, ()> {
        match self {
            Exp::Num(_) => Ok(Type::Int),
            Exp::True => Ok(Type::Bool),
            Exp::False => Ok(Type::Bool),
            Exp::Lvalue(Lvalue::Ident(x)) => types.get(x).copied().ok_or(()),
            Exp::PureBinop(e1, eq, e2)
                if eq.signature() == OpType::Equality =>
            {
                if e1.synthesize(types)? == e2.synthesize(types)? {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::PureBinop(e1, op, e2) if op.signature() == OpType::Logical => {
                if e1.synthesize(types)? == Type::Bool
                    && e2.synthesize(types)? == Type::Bool
                {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::PureBinop(e1, op, e2)
                if op.signature() == OpType::Relational =>
            {
                if e1.synthesize(types)? == Type::Int
                    && e2.synthesize(types)? == Type::Int
                {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::PureBinop(e1, op, e2)
                if op.signature() == OpType::Arithmetic =>
            {
                if e1.synthesize(types)? == Type::Int
                    && e2.synthesize(types)? == Type::Int
                {
                    Ok(Type::Int)
                } else {
                    Err(())
                }
            }
            Exp::ImpureBinop(e1, _, e2) => {
                if e1.synthesize(types)? == Type::Int
                    && e2.synthesize(types)? == Type::Int
                {
                    Ok(Type::Int)
                } else {
                    Err(())
                }
            }
            Exp::UnOp(op, e) if op.signature() == OpType::Logical => {
                if e.synthesize(types)? == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::UnOp(op, e) if op.signature() == OpType::Arithmetic => {
                if e.synthesize(types)? == Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(())
                }
            }
            Exp::Ternary {
                cond,
                exp_true,
                exp_false,
            } => {
                let cond_type = cond.synthesize(types)?;
                if cond_type != Type::Bool {
                    return Err(());
                }

                let branch_type = exp_true.synthesize(types)?;

                if exp_false.synthesize(types)? == branch_type {
                    Ok(branch_type)
                } else {
                    Err(())
                }
            }
            exp => {
                panic!("unexpected expression in typecheck: {:?}", exp)
            }
        }
    }
}

fn check_stmt<'input>(
    types: &mut TypeMap<'input>,
    s: &'input Stmt,
    t: Type,
) -> Result<(), ()> {
    match s {
        Stmt::Return(e) => match e.synthesize(types) == Ok(t) {
            true => Ok(()),
            false => Err(()),
        },
        Stmt::Assign(Lvalue::Ident(var), e) => {
            let exp_type = e.synthesize(types);
            let var_type = types.get(var);

            match (exp_type, var_type) {
                (Ok(t1), Some(t2)) if t1 == *t2 => Ok(()),
                _ => Err(()),
            }
        }
        Stmt::Seq(block) => {
            block.iter().try_for_each(|s| check_stmt(types, s, t))
        }
        Stmt::Nop => Ok(()),
        Stmt::Declare(var, var_type, scope) => {
            // disallow shadowing
            if types.contains_key(var) {
                return Err(());
            }
            types.insert(var, *var_type);
            let result = check_stmt(types, scope, t);
            types.remove(var);
            result
        }
        Stmt::Exp(e) => e.synthesize(types).map(|_| ()),
        Stmt::If {
            cond,
            stmt_true,
            stmt_false,
        } => {
            if cond.synthesize(types) == Ok(Type::Bool) {
                check_stmt(types, stmt_true, t)
                    .and(check_stmt(types, stmt_false, t))
            } else {
                Err(())
            }
        }
        Stmt::While { cond, body } => {
            if cond.synthesize(types) == Ok(Type::Bool) {
                check_stmt(types, body, t)
            } else {
                Err(())
            }
        }
    }
}

pub fn typecheck(s: &Program) -> Result<(), ()> {
    let mut types = HashMap::new();
    check_stmt(&mut types, s.as_ref(), Type::Int)
}
