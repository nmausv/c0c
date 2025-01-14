use std::collections::{HashMap, HashSet};

use crate::frontend::elab_ast::{Exp, Ident, Program, Stmt, Type};

type VarSet = HashSet<Ident>;
type TypeMap = HashMap<Ident, Type>;

// true if and only if the expression uses only variables in the set
fn exp_uses_only(exp: &Exp, initialized: &VarSet) -> bool {
    match exp {
        Exp::Num(_) => true,
        Exp::True => true,
        Exp::False => true,
        Exp::Ident(x) => initialized.contains(x),
        Exp::PureBinop(e1, _, e2) => {
            exp_uses_only(e1, initialized) && exp_uses_only(e2, initialized)
        }
        Exp::ImpureBinop(e1, _, e2) => {
            exp_uses_only(e1, initialized) && exp_uses_only(e2, initialized)
        }
        Exp::UnOp(_, e) => exp_uses_only(e, initialized),
        Exp::Ternary {
            cond,
            exp_true,
            exp_false,
        } => {
            exp_uses_only(cond, initialized)
                && exp_uses_only(exp_true, initialized)
                && exp_uses_only(exp_false, initialized)
        }
    }
}

#[derive(Clone)]
struct StmtEnv {
    initialized: VarSet,
    inscope: TypeMap,
}

/// Given an environment `env` and a statment `s`, returns a new environment `s'`
/// if `s` assigns only to variables in the scope of `env`, and uses only variables initialized in `env`,
/// leaving a resulting environment of `s'`.
fn stmt_initializes(mut env: StmtEnv, s: &Stmt) -> Option<StmtEnv> {
    match s {
        Stmt::Nop => Some(env),
        Stmt::Seq(v) => {
            let mut intermediary = env;
            for s in v {
                intermediary = stmt_initializes(intermediary, s)?;
            }
            Some(intermediary)
        }
        Stmt::Assign(x, e) => {
            if env.inscope.contains_key(x) && exp_uses_only(e, &env.initialized)
            {
                env.initialized.insert(x.clone());
                Some(env)
            } else {
                if !env.inscope.contains_key(x) {
                    println!("assignment to variable not in scope");
                } else {
                    println!("assignment uses variables not yet initialized");
                }
                None
            }
        }
        Stmt::Return(e) => {
            if exp_uses_only(e, &env.initialized) {
                env.initialized.drain();
                for key in env.inscope.keys() {
                    env.initialized.insert(key.clone());
                }
                Some(env)
            } else {
                None
            }
        }
        Stmt::Declare(x, t, scope) => {
            // since double declares are not allowed, check that x is not already in scope
            if env.inscope.contains_key(x) {
                println!("double declaration");
                return None;
            }
            env.inscope.insert(x.clone(), *t);
            let mut new_env = stmt_initializes(env, scope)?;
            new_env.inscope.remove(x);
            new_env.initialized.remove(x);
            Some(new_env)
        }
        Stmt::Exp(_) => Some(env),
        Stmt::If {
            cond,
            stmt_true,
            stmt_false,
        } => {
            if !exp_uses_only(cond, &env.initialized) {
                return None;
            }

            // at least one clone is necessary since if bodies may mutate the environment
            // if we only use one clone, we need to clone the previous inscope anyway
            // because we need to make sure that variables declared in the branches
            // aren't valid in scope outside those branches
            let env_true = stmt_initializes(env.clone(), stmt_true)?;
            let env_false = stmt_initializes(env.clone(), stmt_false)?;

            // note:
            // right now this code is obvious in what it does, at the cost of some efficiency.
            // cloning the intersection is possibly slow, especially when the two environments
            // are about to go out of scope and be dropped anyway

            Some(StmtEnv {
                initialized: env_true
                    .initialized
                    .intersection(&env_false.initialized)
                    .cloned()
                    .collect(),
                inscope: env.inscope,
            })
        }
        Stmt::While { cond, body } => {
            if !exp_uses_only(cond, &env.initialized) {
                return None;
            }

            // clone is necessary since the while body may mutate the environment,
            // but we only want it to mutate the local environment since the body
            // may not be called
            let _ = stmt_initializes(env.clone(), body)?;

            Some(env)
        }
    }
}

/// Checks that all variables are initialized before being used.
/// A `return` initializes all variables currently in scope, so programs like
/// ```c
/// int main() {
///   int x; // (1)
///   return 0;
///   int y = x + 1; // (2)
/// }
/// ```
/// are valid, even though `x` is used before being initialized at (2).
/// Note that variables not in scope are not initialized, so removing line (1) is not valid.
pub fn initialization_check(elab_program: &Program) -> bool {
    let env = StmtEnv {
        inscope: HashMap::new(),
        initialized: HashSet::new(),
    };

    match stmt_initializes(env, elab_program.as_ref()) {
        None => false,
        Some(_) => true,
    }
}

#[cfg(test)]
mod init_tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::frontend::elab_ast::*;

    #[test]
    fn empty_main() {
        let program: Program = Stmt::Return(Exp::Num(0)).into();
        assert!(initialization_check(&program));
    }

    #[test]
    fn declare_only() {
        let program: Program = Stmt::Declare(
            String::from("x"),
            Type::Int,
            Box::new(Stmt::Return(Exp::Num(0))),
        )
        .into();
        assert!(initialization_check(&program));
    }

    #[test]
    fn use_before_declare() {
        let program: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
            Stmt::Return(Exp::Num(0)),
        ]))
        .into();
        assert!(!initialization_check(&program));
    }

    #[test]
    fn use_out_of_scope() {
        let program: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Declare(String::from("x"), Type::Int, Box::new(Stmt::Nop)),
            Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
        ]))
        .into();
        assert!(!initialization_check(&program));
    }

    #[test]
    fn use_after_return() {
        let program: Program = Stmt::Declare(
            String::from("x"),
            Type::Int,
            Box::new(Stmt::Seq(VecDeque::from(vec![
                Stmt::Return(Exp::Num(0)),
                Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
            ]))),
        )
        .into();
        assert!(initialization_check(&program));
    }

    #[test]
    fn use_with_declare_after_return() {
        let program: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Return(Exp::Num(0)),
            Stmt::Declare(
                String::from("x"),
                Type::Int,
                Box::new(Stmt::Seq(VecDeque::from(vec![
                    Stmt::Assign(String::from("x"), Exp::Num(0)),
                    Stmt::Assign(
                        String::from("x"),
                        Exp::Ident(String::from("x")),
                    ),
                ]))),
            ),
        ]))
        .into();
        assert!(initialization_check(&program));
    }

    #[test]
    fn double_declare() {
        // NOT VALID
        // int main() {int x = 0; int x = 1; x = x;}
        let program_noscope: Program = Stmt::Declare(
            String::from("x"),
            Type::Int,
            Box::new(Stmt::Seq(VecDeque::from(vec![
                Stmt::Assign(String::from("x"), Exp::Num(0)),
                Stmt::Declare(
                    String::from("x"),
                    Type::Int,
                    Box::new(Stmt::Assign(String::from("x"), Exp::Num(1))),
                ),
                Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
            ]))),
        )
        .into();
        assert!(!initialization_check(&program_noscope));

        // VALID
        // int main() {{int x = 0;} int x = 1; x = x;}
        let program_scope: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Declare(
                String::from("x"),
                Type::Int,
                Box::new(Stmt::Assign(String::from("x"), Exp::Num(0))),
            ),
            Stmt::Declare(
                String::from("x"),
                Type::Int,
                Box::new(Stmt::Seq(VecDeque::from(vec![
                    Stmt::Assign(String::from("x"), Exp::Num(1)),
                    Stmt::Return(Exp::Ident(String::from("x"))),
                ]))),
            ),
        ]))
        .into();
        assert!(initialization_check(&program_scope));
    }
}
