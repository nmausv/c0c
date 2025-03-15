use std::collections::{HashMap, HashSet, VecDeque};

use crate::frontend::elab_ast::{Exp, Ident, Lvalue, Program, Stmt, Type};

use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

type VarSet<'input> = HashSet<Ident<'input>>;
type TypeMap<'input> = HashMap<Ident<'input>, Type>;

// true if and only if the expression uses only variables in the initialized set

enum ExpFrame<'a> {
    Num,
    True,
    False,
    Lvalue(Lvalue<'a>),
    Binop {
        left: FrameProgress<&'a Exp<'a>, ()>,
        right: FrameProgress<&'a Exp<'a>, ()>,
    },
    Unop {
        exp: &'a Exp<'a>,
    },
    Ternary {
        cond: FrameProgress<&'a Exp<'a>, ()>,
        true_exp: FrameProgress<&'a Exp<'a>, ()>,
        false_exp: FrameProgress<&'a Exp<'a>, ()>,
    },
}

impl<'a> From<&'a Exp<'a>> for ExpFrame<'a> {
    fn from(value: &'a Exp<'a>) -> Self {
        match value {
            Exp::Num(_) => Self::Num,
            Exp::Lvalue(lvalue) => Self::Lvalue(*lvalue),
            Exp::PureBinop(left, _, right)
            | Exp::ImpureBinop(left, _, right) => Self::Binop {
                left: New(left),
                right: New(right),
            },
            Exp::Unop(_, exp) => Self::Unop { exp },
            Exp::True => Self::True,
            Exp::False => Self::False,
            Exp::Ternary {
                cond,
                exp_true,
                exp_false,
            } => Self::Ternary {
                cond: New(cond),
                true_exp: New(exp_true),
                false_exp: New(exp_false),
            },
        }
    }
}

impl<'input> Exp<'input> {
    fn uses_only_iterative(&self, initialized: &VarSet) -> bool {
        let mut stack = Vec::new();
        stack.push(self.into());

        while let Some(frame) = stack.pop() {
            match frame {
                ExpFrame::Num | ExpFrame::True | ExpFrame::False => (),
                ExpFrame::Lvalue(Lvalue::Ident(name)) => {
                    if !initialized.contains(name) {
                        return false;
                    }
                }
                ExpFrame::Unop { exp } => {
                    stack.push(exp.into());
                }
                ExpFrame::Binop { left, right } => match (left, right) {
                    (New(left), New(right)) => {
                        stack.push(ExpFrame::Binop {
                            left: InProgress,
                            right: New(right),
                        });
                        stack.push(left.into());
                    }
                    (InProgress, New(right)) => {
                        stack.push(ExpFrame::Binop {
                            left: Done(()),
                            right: InProgress,
                        });
                        stack.push(right.into());
                    }
                    (Done(()), InProgress) => {}
                    _ => unreachable!(),
                },
                ExpFrame::Ternary {
                    cond,
                    true_exp,
                    false_exp,
                } => match (cond, true_exp, false_exp) {
                    (New(cond), New(true_exp), New(false_exp)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: InProgress,
                            true_exp: New(true_exp),
                            false_exp: New(false_exp),
                        });
                        stack.push(cond.into());
                    }
                    (InProgress, New(true_exp), New(false_exp)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: Done(()),
                            true_exp: InProgress,
                            false_exp: New(false_exp),
                        });
                        stack.push(true_exp.into());
                    }
                    (Done(()), InProgress, New(false_exp)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: Done(()),
                            true_exp: Done(()),
                            false_exp: InProgress,
                        });
                        stack.push(false_exp.into());
                    }
                    (Done(()), Done(()), InProgress) => {}
                    _ => unreachable!(),
                },
            }
        }

        true
    }

    fn uses_only_recursive(&self, initialized: &VarSet) -> bool {
        match self {
            Exp::Num(_) | Exp::True | Exp::False => true,
            Exp::Lvalue(Lvalue::Ident(x)) => initialized.contains(x),
            Exp::PureBinop(e1, _, e2) | Exp::ImpureBinop(e1, _, e2) => {
                e1.uses_only_recursive(initialized)
                    && e2.uses_only_recursive(initialized)
            }
            Exp::Unop(_, e) => e.uses_only_recursive(initialized),
            Exp::Ternary {
                cond,
                exp_true,
                exp_false,
            } => {
                cond.uses_only_recursive(initialized)
                    && exp_true.uses_only_recursive(initialized)
                    && exp_false.uses_only_recursive(initialized)
            }
        }
    }

    fn uses_only(&self, initialized: &VarSet) -> bool {
        self.uses_only_iterative(initialized)
    }
}

#[derive(Clone, Debug)]
struct StmtEnv<'input> {
    initialized: VarSet<'input>,
    inscope: TypeMap<'input>,
}

impl<'input> Stmt<'input> {
    /// Given an environment `env` and a statment `s`, returns a new environment `s'`
    /// if `s` assigns only to variables in the scope of `env`, and uses only variables initialized in `env`,
    /// leaving a resulting environment of `s'`.
    fn initializes(&self, mut env: StmtEnv<'input>) -> Option<StmtEnv<'input>> {
        match self {
            Stmt::Nop => Some(env),
            Stmt::Seq(v) => {
                let mut intermediary = env;
                for s in v {
                    intermediary = s.initializes(intermediary)?;
                }
                Some(intermediary)
            }
            Stmt::Assign(Lvalue::Ident(x), e) => {
                if env.inscope.contains_key(x) && e.uses_only(&env.initialized)
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
                if e.uses_only(&env.initialized) {
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
                let mut new_env = scope.initializes(env)?;
                new_env.inscope.remove(x);
                new_env.initialized.remove(x);
                Some(new_env)
            }
            Stmt::Exp(exp) => {
                if exp.uses_only(&env.initialized) {
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
                if !cond.uses_only(&env.initialized) {
                    return None;
                }

                // at least one clone is necessary since "if" bodies may mutate the environment

                // if we only use one clone, we need to clone the previous inscope anyway
                // because we need to make sure that variables declared in the branches
                // aren't in scope outside those branches
                let env_true = stmt_true.initializes(env.clone())?;
                let env_false = stmt_false.initializes(env.clone())?;

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
                if !cond.uses_only(&env.initialized) {
                    return None;
                }

                // clone is necessary since the while body may mutate the environment,
                // but we only want it to mutate the local environment since the body
                // may not be called
                let _ = body.initializes(env.clone())?;

                Some(env)
            }
        }
    }
}

#[derive(Debug)]
enum Frame<'input, 'frame> {
    Seq {
        list: &'input VecDeque<Stmt<'input>>,
        next: usize,
    },
    Declare {
        name: Ident<'input>,
        t: Type,
        scope: FrameProgress<&'frame Stmt<'input>, ()>,
    },
    If {
        environ: StmtEnv<'input>,
        cond: &'frame Exp<'input>,
        if_branch: FrameProgress<&'frame Stmt<'input>, Box<StmtEnv<'input>>>,
        else_branch: FrameProgress<&'frame Stmt<'input>, Box<StmtEnv<'input>>>,
    },
    While {
        cond: &'frame Exp<'input>,
        body: FrameProgress<&'frame Stmt<'input>, ()>,
        environ: StmtEnv<'input>,
    },
    Assign {
        lval: &'frame Lvalue<'input>,
        exp: &'frame Exp<'input>,
    },
    Return {
        exp: &'frame Exp<'input>,
    },
    Nop,
    Exp {
        exp: &'frame Exp<'input>,
    },
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
        Stmt::Declare(name, t, scope) => Frame::Declare {
            name,
            t: *t,
            scope: New(scope),
        },
        Stmt::Assign(lval, exp) => Frame::Assign { lval, exp },
        Stmt::Return(exp) => Frame::Return { exp },
        Stmt::Seq(seq) => Frame::Seq { list: seq, next: 0 },
        Stmt::Nop => Frame::Nop,
        Stmt::If {
            cond,
            stmt_true,
            stmt_false,
        } => Frame::If {
            environ: env.clone(),
            cond,
            if_branch: New(stmt_true),
            else_branch: New(stmt_false),
        },
        Stmt::While { cond, body } => Frame::While {
            cond,
            body: New(body),
            environ: env.clone(),
        },
        Stmt::Exp(exp) => Frame::Exp { exp },
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
    fn initialization_check_iterative(self: &'a Program<'a>) -> Result<(), ()> {
        let mut env = StmtEnv {
            initialized: HashSet::new(),
            inscope: HashMap::new(),
        };
        let mut stack: Vec<Frame> = Vec::new();

        stack.push(make_frame(self.as_ref(), &env));

        while let Some(mut frame) = stack.pop() {
            match frame {
                Frame::Seq { list, next } => {
                    if next < list.len() {
                        // get child statement
                        let child_stmt = &list[next];
                        // modify parent
                        frame = Frame::Seq {
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
                Frame::Declare { name, t, scope } => {
                    match scope {
                        New(scope) => {
                            // check double declare
                            if env.inscope.contains_key(name) {
                                return Err(());
                            }
                            env.inscope.insert(name, t);

                            // mark as done
                            frame = Frame::Declare {
                                name,
                                t,
                                scope: Done(()),
                            };

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
                Frame::If {
                    environ,
                    cond,
                    if_branch,
                    else_branch,
                } => {
                    match (if_branch, else_branch) {
                        (New(if_branch), New(else_branch)) => {
                            // check condition
                            if !cond.uses_only(&env.initialized) {
                                return Err(());
                            }

                            // create if child frame, mark as in progress
                            frame = Frame::If {
                                environ,
                                cond,
                                if_branch: InProgress,
                                else_branch: New(else_branch),
                            };
                            let if_frame = make_frame(if_branch, &env);

                            // push modified parent
                            stack.push(frame);

                            // push child frame to stack
                            stack.push(if_frame);
                        }
                        (InProgress, New(else_branch)) => {
                            // store if environment, mark as done
                            // mark else frame as in progress
                            frame = Frame::If {
                                environ,
                                cond,
                                if_branch: Done(Box::new(env.clone())),
                                else_branch: InProgress,
                            };

                            // make child frame
                            let else_frame = make_frame(else_branch, &env);

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
                Frame::While {
                    cond,
                    body,
                    environ,
                } => {
                    match body {
                        New(body) => {
                            if !cond.uses_only(&env.initialized) {
                                return Err(());
                            }

                            // mark as Done
                            frame = Frame::While {
                                cond,
                                body: InProgress,
                                environ,
                            };
                            stack.push(frame);

                            // push child
                            stack.push(make_frame(body, &env));
                        }
                        InProgress => unreachable!(),
                        Done(()) => {
                            env = environ;
                        }
                    }
                }
                Frame::Exp { exp } => {
                    if !exp.uses_only(&env.initialized) {
                        return Err(());
                    }
                }
                Frame::Nop => (),
                Frame::Assign { lval, exp } => {
                    let Lvalue::Ident(name) = lval;
                    if env.inscope.contains_key(name)
                        && exp.uses_only(&env.initialized)
                    {
                        env.initialized.insert(name);
                    } else {
                        return Err(());
                    }
                }
                Frame::Return { exp } => {
                    if exp.uses_only(&env.initialized) {
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

    fn initialization_check_recursive(self: &'a Program<'a>) -> Result<(), ()> {
        let env: StmtEnv = StmtEnv {
            initialized: HashSet::new(),
            inscope: HashMap::new(),
        };
        let stmt: &Stmt = self.as_ref();
        match stmt.initializes(env) {
            Some(_) => Ok(()),
            None => Err(()),
        }
    }

    pub fn initialization_check(self: &'a Program<'a>) -> Result<(), ()> {
        self.initialization_check_iterative()
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
