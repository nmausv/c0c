use std::collections::{HashMap, HashSet};

use crate::frontend::elab_ast;

type VarSet = HashSet<elab_ast::Ident>;
type TypeMap = HashMap<elab_ast::Ident, elab_ast::Type>;

// true if and only if the expression uses only variables in the set
fn exp_uses(exp: &elab_ast::Exp, defined: &VarSet) -> bool {
    match exp {
        elab_ast::Exp::Num(_) => true,
        elab_ast::Exp::Ident(x) => defined.contains(x),
        elab_ast::Exp::PureBinop(e1, _, e2) => {
            exp_uses(e1, defined) && exp_uses(e2, defined)
        }
        elab_ast::Exp::ImpureBinop(e1, _, e2) => {
            exp_uses(e1, defined) && exp_uses(e2, defined)
        }
    }
}

#[derive(Clone)]
struct StmtEnv {
    defined: VarSet,
    types: TypeMap,
}

/// Given an environment `env` and a statment `s`, returns a new environment `s'`
/// if `s` assigns only to variables in the scope of `env`, and uses only variables defined in `env`,
/// leaving a resulting environment of `s'`.
fn stmt_initializes(mut env: StmtEnv, s: &elab_ast::Stmt) -> Option<StmtEnv> {
    match s {
        elab_ast::Stmt::Nop => Some(env),
        elab_ast::Stmt::Seq(v) => {
            let mut intermediary = env;
            for s in v {
                intermediary = stmt_initializes(intermediary, s)?;
            }
            Some(intermediary)
        }
        elab_ast::Stmt::Assign(x, e) => {
            if env.types.contains_key(x) && exp_uses(e, &env.defined) {
                env.defined.insert(x.clone());
                Some(env)
            } else {
                if !env.types.contains_key(x) {
                    println!("assignment to variable not in scope");
                } else {
                    println!("assignment uses variables not yet defined");
                }
                None
            }
        }
        elab_ast::Stmt::Return(e) => {
            if exp_uses(e, &env.defined) {
                env.defined.drain();
                for key in env.types.keys() {
                    env.defined.insert(key.clone());
                }
                Some(env)
            } else {
                None
            }
        }
        elab_ast::Stmt::Declare(x, t, scope) => {
            // since double declares are not allowed, check that x is not already in scope
            if env.types.contains_key(x) {
                println!("double declaration");
                return None;
            }
            env.types.insert(x.clone(), *t);
            let mut new_env = stmt_initializes(env, scope)?;
            new_env.types.remove(x);
            new_env.defined.remove(x);
            Some(new_env)
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
pub fn initialization_check(elab_program: &elab_ast::Stmt) -> bool {
    let env = StmtEnv {
        types: HashMap::new(),
        defined: HashSet::new(),
    };

    match stmt_initializes(env, elab_program) {
        None => false,
        Some(_) => true,
    }
}

#[cfg(test)]
mod init_tests {
    use std::collections::VecDeque;

    use super::*;
    use elab_ast::*;

    #[test]
    fn empty_main() {
        let program = Stmt::Return(Exp::Num(0));
        assert!(initialization_check(&program));
    }

    #[test]
    fn declare_only() {
        let program = Stmt::Declare(
            String::from("x"),
            Type::Int,
            Box::new(Stmt::Return(Exp::Num(0))),
        );
        assert!(initialization_check(&program));
    }

    #[test]
    fn use_before_declare() {
        let program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
            Stmt::Return(Exp::Num(0)),
        ]));
        assert!(!initialization_check(&program));
    }

    #[test]
    fn use_out_of_scope() {
        let program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Declare(String::from("x"), Type::Int, Box::new(Stmt::Nop)),
            Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
        ]));
        assert!(!initialization_check(&program));
    }

    #[test]
    fn use_after_return() {
        let program = Stmt::Declare(
            String::from("x"),
            Type::Int,
            Box::new(Stmt::Seq(VecDeque::from(vec![
                Stmt::Return(Exp::Num(0)),
                Stmt::Assign(String::from("x"), Exp::Ident(String::from("x"))),
            ]))),
        );
        assert!(initialization_check(&program));
    }

    #[test]
    fn use_with_declare_after_return() {
        let program = Stmt::Seq(VecDeque::from(vec![
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
        ]));
        assert!(initialization_check(&program));
    }

    #[test]
    fn double_declare() {
        // NOT VALID
        // int main() {int x = 0; int x = 1; x = x;}
        let program_noscope = Stmt::Declare(
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
        );
        assert!(!initialization_check(&program_noscope));

        // VALID
        // int main() {{int x = 0;} int x = 1; x = x;}
        let program_scope = Stmt::Seq(VecDeque::from(vec![
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
        ]));
        assert!(initialization_check(&program_scope));
    }
}
