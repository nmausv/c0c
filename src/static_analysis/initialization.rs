use std::collections::{HashMap, HashSet, VecDeque};

use crate::frontend::elab_ast::{Exp, Ident, Lvalue, Program, Stmt, Type};

use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

type VarSet<'input> = HashSet<Ident<'input>>;
type TypeMap<'input> = HashMap<Ident<'input>, Type>;

// true if and only if the expression uses only variables in the initialized set
fn exp_uses_only(exp: &Exp, initialized: &VarSet) -> bool {
    match exp {
        Exp::Num(_) | Exp::True | Exp::False => true,
        Exp::Lvalue(Lvalue::Ident(x)) => initialized.contains(x),
        Exp::PureBinop(e1, _, e2) | Exp::ImpureBinop(e1, _, e2) => {
            exp_uses_only(e1, initialized) && exp_uses_only(e2, initialized)
        }
        Exp::Unop(_, e) => exp_uses_only(e, initialized),
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
struct StmtEnv<'input> {
    initialized: VarSet<'input>,
    inscope: TypeMap<'input>,
}

/// Given an environment `env` and a statment `s`, returns a new environment `s'`
/// if `s` assigns only to variables in the scope of `env`, and uses only variables initialized in `env`,
/// leaving a resulting environment of `s'`.
fn stmt_initializes<'input>(
    mut env: StmtEnv<'input>,
    s: &Stmt<'input>,
) -> Option<StmtEnv<'input>> {
    match s {
        Stmt::Nop => Some(env),
        Stmt::Seq(v) => {
            let mut intermediary = env;
            for s in v {
                intermediary = stmt_initializes(intermediary, s)?;
            }
            Some(intermediary)
        }
        Stmt::Assign(Lvalue::Ident(x), e) => {
            if env.inscope.contains_key(x) && exp_uses_only(e, &env.initialized)
            {
                env.initialized.insert(x);
                Some(env)
            } else {
                //TODO: add verbose mode?
                // more likely just add proper error types
                /*
                if !env.inscope.contains_key(x) {
                    println!("assignment to variable not in scope");
                } else {
                    println!("assignment uses variables not yet initialized");
                }
                */
                None
            }
        }
        Stmt::Return(e) => {
            if exp_uses_only(e, &env.initialized) {
                env.initialized.drain();
                for key in env.inscope.keys() {
                    env.initialized.insert(key);
                }
                Some(env)
            } else {
                None
            }
        }
        Stmt::Declare(x, t, scope) => {
            // since double declares are not allowed, check that x is not already in scope
            if env.inscope.contains_key(x) {
                // eprintln!("double declaration");
                return None;
            }
            env.inscope.insert(x, *t);
            let mut new_env = stmt_initializes(env, scope)?;
            new_env.inscope.remove(x);
            new_env.initialized.remove(x);
            Some(new_env)
        }
        Stmt::Exp(exp) => {
            if exp_uses_only(exp, &env.initialized) {
                Some(env)
            } else {
                None
            }
        }
        Stmt::If {
            cond,
            stmt_true,
            stmt_false,
        } => {
            if !exp_uses_only(cond, &env.initialized) {
                return None;
            }

            // at least one clone is necessary since "if" bodies may mutate the environment

            // if we only use one clone, we need to clone the previous inscope anyway
            // because we need to make sure that variables declared in the branches
            // aren't in scope outside those branches
            let env_true = stmt_initializes(env.clone(), stmt_true)?;
            let env_false = stmt_initializes(env.clone(), stmt_false)?;

            // note:
            // right now code below is obvious in what it does, at the cost of some efficiency.
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

enum FrameKind<'input> {
    Seq {
        list: &'input VecDeque<Stmt<'input>>,
        next: usize,
    },
    Declare {
        progress: FrameProgress<()>,
    },
    If {
        environ: StmtEnv<'input>,
        if_env: FrameProgress<Box<StmtEnv<'input>>>,
        else_env: FrameProgress<Box<StmtEnv<'input>>>,
    },
    While {
        progress: FrameProgress<()>,
        environ: StmtEnv<'input>,
    },
    Assign,
    Return,
    Nop,
    Exp,
}

struct Frame<'input, 'frame> {
    statement: &'frame Stmt<'input>,
    kind: FrameKind<'input>,
}

fn make_frame<'input, 'parent, 'frame>(
    s: &'parent Stmt<'input>,
    env: &StmtEnv<'input>,
) -> Frame<'frame, 'input>
where
    'input: 'parent,
    'parent: 'frame,
{
    match s {
        Stmt::Declare(_, _, _) => Frame {
            statement: s,
            kind: FrameKind::Declare { progress: New },
        },
        Stmt::Assign(_, _) => Frame {
            statement: s,
            kind: FrameKind::Assign,
        },
        Stmt::Return(_) => Frame {
            statement: s,
            kind: FrameKind::Return,
        },
        Stmt::Seq(seq) => Frame {
            statement: s,
            kind: FrameKind::Seq { list: seq, next: 0 },
        },
        Stmt::Nop => Frame {
            statement: s,
            kind: FrameKind::Nop,
        },
        Stmt::If { .. } => Frame {
            statement: s,
            kind: FrameKind::If {
                environ: env.clone(),
                if_env: FrameProgress::New,
                else_env: FrameProgress::New,
            },
        },
        Stmt::While { .. } => Frame {
            statement: s,
            kind: FrameKind::While {
                progress: New,
                environ: env.clone(),
            },
        },
        Stmt::Exp(_) => Frame {
            statement: s,
            kind: FrameKind::Exp,
        },
    }
}

impl<'a> Program<'a> {
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
    ///
    /// This function used to be recursive, but for very long programs, that could overflow
    /// the stack.
    /// Now this function uses heap space to keep track of `Frame`s, and manually recurses to
    /// save on stack space.
    pub fn initialization_check(self: &'a Program<'a>) -> Result<(), ()> {
        let mut env = StmtEnv {
            initialized: HashSet::new(),
            inscope: HashMap::new(),
        };
        let mut stack: Vec<Frame> = Vec::new();

        stack.push(make_frame(self.as_ref(), &env));

        while let Some(mut frame) = stack.pop() {
            match frame.kind {
                FrameKind::Seq { list, next } => {
                    if next < list.len() {
                        // get child statement
                        let child_stmt = &list[next];
                        // modify parent
                        frame.kind = FrameKind::Seq {
                            list,
                            next: next + 1,
                        };
                        // push modified parent frame
                        stack.push(frame);
                        // push child frame
                        let child_frame = make_frame(child_stmt, &env);
                        stack.push(child_frame);
                    }
                }
                FrameKind::Declare { progress } => {
                    let Stmt::Declare(name, t, scope) = frame.statement else {
                        unreachable!()
                    };
                    match progress {
                        New => {
                            // check double declare
                            if env.inscope.contains_key(name) {
                                return Err(());
                            }
                            env.inscope.insert(name, *t);

                            // mark as done
                            frame.kind =
                                FrameKind::Declare { progress: Done(()) };

                            // push modified parent
                            stack.push(frame);

                            // push child
                            let scope_frame = make_frame(scope, &env);
                            stack.push(scope_frame);
                        }
                        InProgress => {
                            unreachable!()
                        }
                        Done(()) => {
                            env.inscope.remove(name);
                            env.initialized.remove(name);
                        }
                    }
                }
                FrameKind::If {
                    environ,
                    if_env,
                    else_env,
                } => {
                    let Stmt::If {
                        cond,
                        stmt_true,
                        stmt_false,
                    } = frame.statement
                    else {
                        unreachable!()
                    };
                    match (if_env, else_env) {
                        (New, New) => {
                            // check condition
                            if !exp_uses_only(cond, &env.initialized) {
                                return Err(());
                            }

                            // create if child frame, mark as in progress
                            frame.kind = FrameKind::If {
                                environ,
                                if_env: InProgress,
                                else_env: New,
                            };
                            let if_frame = make_frame(stmt_true, &env);

                            // push modified parent
                            stack.push(frame);

                            // push child frame to stack
                            stack.push(if_frame);
                        }
                        (InProgress, New) => {
                            // store if environment, mark as done
                            // mark else frame as in progress
                            frame.kind = FrameKind::If {
                                environ,
                                if_env: Done(Box::new(env.clone())),
                                else_env: New,
                            };

                            // make child frame
                            let else_frame = make_frame(stmt_false, &env);

                            // push modified parent
                            stack.push(frame);

                            // push child frame
                            stack.push(else_frame);
                        }
                        (Done(if_env), InProgress) => {
                            env.initialized = if_env
                                .initialized
                                .intersection(&env.initialized)
                                .cloned()
                                .collect();
                        }
                        _ => unreachable!(),
                    }
                }
                FrameKind::While { environ, progress } => {
                    let Stmt::While { cond, body } = frame.statement else {
                        unreachable!()
                    };
                    match progress {
                        New => {
                            if !exp_uses_only(cond, &env.initialized) {
                                return Err(());
                            }

                            // mark as Done
                            frame.kind = FrameKind::While {
                                progress: Done(()),
                                environ,
                            };

                            // push child
                            stack.push(make_frame(body, &env));
                        }
                        InProgress => unreachable!(),
                        Done(()) => {
                            env = environ;
                        }
                    }
                }
                FrameKind::Exp => {
                    let Stmt::Exp(exp) = frame.statement else {
                        unreachable!()
                    };
                    if !exp_uses_only(exp, &env.initialized) {
                        return Err(());
                    }
                }
                FrameKind::Nop => (),
                FrameKind::Assign => {
                    let Stmt::Assign(Lvalue::Ident(name), exp) =
                        frame.statement
                    else {
                        unreachable!()
                    };
                    if env.inscope.contains_key(name)
                        && exp_uses_only(exp, &env.initialized)
                    {
                        env.initialized.insert(name);
                    } else {
                        return Err(());
                    }
                }
                FrameKind::Return => {
                    let Stmt::Return(exp) = frame.statement else {
                        unreachable!()
                    };
                    if exp_uses_only(exp, &env.initialized) {
                        env.initialized.drain();
                        for key in env.inscope.keys() {
                            env.initialized.insert(key);
                        }
                    } else {
                        return Err(());
                    }
                }
            };
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::frontend::elab_ast::*;

    #[test]
    fn empty_main() {
        let program: Program = Stmt::Return(Exp::Num(0)).into();
        assert!(program.initialization_check().is_ok());
    }

    #[test]
    fn declare_only() {
        let program: Program =
            Stmt::Declare("x", Type::Int, Box::new(Stmt::Return(Exp::Num(0))))
                .into();
        assert!(program.initialization_check().is_ok());
    }

    #[test]
    fn use_before_declare() {
        let program: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Assign("x".into(), Exp::Lvalue("x".into())),
            Stmt::Return(Exp::Num(0)),
        ]))
        .into();
        assert!(program.initialization_check().is_err());
    }

    #[test]
    fn use_out_of_scope() {
        let program: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Declare("x", Type::Int, Box::new(Stmt::Nop)),
            Stmt::Assign("x".into(), Exp::Lvalue("x".into())),
        ]))
        .into();
        assert!(program.initialization_check().is_err());
    }

    #[test]
    fn use_after_return() {
        let program: Program = Stmt::Declare(
            "x",
            Type::Int,
            Box::new(Stmt::Seq(VecDeque::from(vec![
                Stmt::Return(Exp::Num(0)),
                Stmt::Assign("x".into(), Exp::Lvalue("x".into())),
            ]))),
        )
        .into();
        assert!(program.initialization_check().is_ok());
    }

    #[test]
    fn declare_after_return() {
        let program: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Return(Exp::Num(0)),
            Stmt::Declare(
                "x",
                Type::Int,
                Box::new(Stmt::Seq(VecDeque::from(vec![
                    Stmt::Assign("x".into(), Exp::Num(0)),
                    Stmt::Assign("x".into(), Exp::Lvalue("x".into())),
                ]))),
            ),
        ]))
        .into();
        assert!(program.initialization_check().is_ok());
    }

    #[test]
    fn double_declare() {
        // NOT VALID
        // int main() {int x = 0; int x = 1; x = x;}
        let program_noscope: Program = Stmt::Declare(
            "x",
            Type::Int,
            Box::new(Stmt::Seq(VecDeque::from(vec![
                Stmt::Assign("x".into(), Exp::Num(0)),
                Stmt::Declare(
                    "x",
                    Type::Int,
                    Box::new(Stmt::Assign("x".into(), Exp::Num(1))),
                ),
                Stmt::Assign("x".into(), Exp::Lvalue("x".into())),
            ]))),
        )
        .into();
        assert!(program_noscope.initialization_check().is_err());

        // VALID
        // int main() {{int x = 0;} int x = 1; x = x;}
        let program_scope: Program = Stmt::Seq(VecDeque::from(vec![
            Stmt::Declare(
                "x",
                Type::Int,
                Box::new(Stmt::Assign("x".into(), Exp::Num(0))),
            ),
            Stmt::Declare(
                "x",
                Type::Int,
                Box::new(Stmt::Seq(VecDeque::from(vec![
                    Stmt::Assign("x".into(), Exp::Num(1)),
                    Stmt::Return(Exp::Lvalue("x".into())),
                ]))),
            ),
        ]))
        .into();
        assert!(program_scope.initialization_check().is_ok());
    }
}
