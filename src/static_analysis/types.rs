use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};
use std::collections::{HashMap, VecDeque};

use crate::frontend::{
    ast::Unop,
    elab_ast::{
        Exp, ImpureBinop, Lvalue, OpType, Program, PureBinop, Stmt, Type,
    },
};

type TypeMap<'a> = HashMap<&'a str, Type>;

enum ExpFrame<'a> {
    Num,
    True,
    False,
    Lvalue(Lvalue<'a>),
    PureBinop {
        left: FrameProgress<&'a Exp<'a>, Type>,
        op: PureBinop,
        right: FrameProgress<&'a Exp<'a>, Type>,
    },
    ImpureBinop {
        left: FrameProgress<&'a Exp<'a>, Type>,
        op: ImpureBinop,
        right: FrameProgress<&'a Exp<'a>, Type>,
    },
    Unop {
        op: Unop,
        exp: FrameProgress<&'a Exp<'a>, ()>,
    },
    Ternary {
        cond: FrameProgress<&'a Exp<'a>, ()>,
        true_exp: FrameProgress<&'a Exp<'a>, Type>,
        false_exp: FrameProgress<&'a Exp<'a>, Type>,
    },
}

impl<'a> From<&'a Exp<'a>> for ExpFrame<'a> {
    fn from(exp: &'a Exp<'a>) -> Self {
        match exp {
            Exp::Num(_) => Self::Num,
            Exp::Lvalue(lvalue) => Self::Lvalue(*lvalue),
            Exp::PureBinop(left, pure_binop, right) => Self::PureBinop {
                left: New(left),
                op: *pure_binop,
                right: New(right),
            },
            Exp::ImpureBinop(left, impure_binop, right) => Self::ImpureBinop {
                left: New(left),
                op: *impure_binop,
                right: New(right),
            },
            Exp::Unop(unop, exp) => Self::Unop {
                op: *unop,
                exp: New(exp),
            },
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

impl Exp<'_> {
    fn synthesize_recursive(&self, types: &TypeMap) -> Result<Type, ()> {
        match self {
            Exp::Num(_) => Ok(Type::Int),
            Exp::True => Ok(Type::Bool),
            Exp::False => Ok(Type::Bool),
            Exp::Lvalue(Lvalue::Ident(x)) => types.get(x).copied().ok_or(()),
            Exp::PureBinop(e1, eq, e2)
                if eq.signature() == OpType::Equality =>
            {
                if e1.synthesize_recursive(types)?
                    == e2.synthesize_recursive(types)?
                {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::PureBinop(e1, op, e2) if op.signature() == OpType::Logical => {
                if e1.synthesize_recursive(types)? == Type::Bool
                    && e2.synthesize_recursive(types)? == Type::Bool
                {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::PureBinop(e1, op, e2)
                if op.signature() == OpType::Relational =>
            {
                if e1.synthesize_recursive(types)? == Type::Int
                    && e2.synthesize_recursive(types)? == Type::Int
                {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::PureBinop(e1, op, e2)
                if op.signature() == OpType::Arithmetic =>
            {
                if e1.synthesize_recursive(types)? == Type::Int
                    && e2.synthesize_recursive(types)? == Type::Int
                {
                    Ok(Type::Int)
                } else {
                    Err(())
                }
            }
            Exp::ImpureBinop(e1, _, e2) => {
                if e1.synthesize_recursive(types)? == Type::Int
                    && e2.synthesize_recursive(types)? == Type::Int
                {
                    Ok(Type::Int)
                } else {
                    Err(())
                }
            }
            Exp::Unop(op, e) if op.signature() == OpType::Logical => {
                if e.synthesize_recursive(types)? == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(())
                }
            }
            Exp::Unop(op, e) if op.signature() == OpType::Arithmetic => {
                if e.synthesize_recursive(types)? == Type::Int {
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
                let cond_type = cond.synthesize_recursive(types)?;
                if cond_type != Type::Bool {
                    return Err(());
                }

                let branch_type = exp_true.synthesize_recursive(types)?;

                if exp_false.synthesize_recursive(types)? == branch_type {
                    Ok(branch_type)
                } else {
                    Err(())
                }
            }
            exp => {
                panic!("unexpected expression in type_check: {:?}", exp)
            }
        }
    }

    fn synthesize_iterative(&self, types: &TypeMap) -> Result<Type, ()> {
        let mut stack: Vec<ExpFrame> = Vec::new();
        stack.push(ExpFrame::from(self));

        // it is plausible to the Rust compiler that we attempt
        // to read from `t` before we assign to it, even if
        // we know that this cannot happen by the structure
        // of our induction.
        // Thus we need a default type, which does not matter.
        let mut t: Type = Type::Int;

        while let Some(frame) = stack.pop() {
            match frame {
                ExpFrame::Num => t = Type::Int,
                ExpFrame::True => t = Type::Bool,
                ExpFrame::False => t = Type::Bool,
                ExpFrame::Lvalue(Lvalue::Ident(name)) => {
                    match types.get(name) {
                        Some(name_type) => t = *name_type,
                        None => return Err(()),
                    }
                }
                ExpFrame::PureBinop { left, op, right } => {
                    match (left, right) {
                        (New(left), New(right)) => {
                            stack.push(ExpFrame::PureBinop {
                                left: InProgress,
                                op,
                                right: New(right),
                            });
                            stack.push(ExpFrame::from(left));
                        }
                        (InProgress, New(right)) => {
                            stack.push(ExpFrame::PureBinop {
                                left: Done(t),
                                op,
                                right: InProgress,
                            });
                            stack.push(ExpFrame::from(right));
                        }
                        (Done(left_type), InProgress) => match op.signature() {
                            OpType::Relational => {
                                if left_type == Type::Int && t == Type::Int {
                                    t = Type::Bool;
                                } else {
                                    return Err(());
                                }
                            }
                            OpType::Equality => {
                                if left_type == t {
                                    t = Type::Bool;
                                } else {
                                    return Err(());
                                }
                            }
                            OpType::Logical => {
                                if left_type == Type::Bool && t == Type::Bool {
                                    t = Type::Bool;
                                } else {
                                    return Err(());
                                }
                            }
                            OpType::Arithmetic => {
                                if left_type == Type::Int && t == Type::Int {
                                    t = Type::Int;
                                } else {
                                    return Err(());
                                }
                            }
                        },
                        _ => unreachable!(),
                    }
                }
                ExpFrame::ImpureBinop { left, op, right } => {
                    match (left, right) {
                        (New(left), New(right)) => {
                            stack.push(ExpFrame::ImpureBinop {
                                left: InProgress,
                                op,
                                right: New(right),
                            });
                            stack.push(ExpFrame::from(left));
                        }
                        (InProgress, New(right)) => {
                            stack.push(ExpFrame::ImpureBinop {
                                left: Done(t),
                                op,
                                right: InProgress,
                            });
                            stack.push(ExpFrame::from(right));
                        }
                        (Done(left_type), InProgress) => {
                            if left_type == Type::Int && t == Type::Int {
                                t = Type::Int;
                            } else {
                                return Err(());
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                ExpFrame::Unop { op, exp } => match exp {
                    New(exp) => {
                        stack.push(ExpFrame::Unop {
                            op,
                            exp: InProgress,
                        });
                        stack.push(ExpFrame::from(exp));
                    }
                    InProgress => match op.signature() {
                        OpType::Relational => unreachable!(),
                        OpType::Equality => unreachable!(),
                        OpType::Logical => {
                            if t != Type::Bool {
                                return Err(());
                            }
                        }
                        OpType::Arithmetic => {
                            if t != Type::Int {
                                return Err(());
                            }
                        }
                    },
                    Done(()) => unreachable!(),
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
                        stack.push(ExpFrame::from(cond));
                    }
                    (InProgress, New(true_exp), New(false_exp)) => {
                        if t != Type::Bool {
                            return Err(());
                        }
                        stack.push(ExpFrame::Ternary {
                            cond: Done(()),
                            true_exp: InProgress,
                            false_exp: New(false_exp),
                        });
                        stack.push(ExpFrame::from(true_exp));
                    }
                    (Done(()), InProgress, New(false_exp)) => {
                        stack.push(ExpFrame::Ternary {
                            cond: Done(()),
                            true_exp: Done(t),
                            false_exp: InProgress,
                        });
                        stack.push(ExpFrame::from(false_exp));
                    }
                    (Done(()), Done(true_type), InProgress) => {
                        if true_type != t {
                            return Err(());
                        }
                    }
                    _ => unreachable!(),
                },
            }
        }

        Ok(t)
    }

    fn synthesize(&self, types: &TypeMap) -> Result<Type, ()> {
        self.synthesize_iterative(types)
    }
}

enum StmtFrameKind<'a> {
    Return,
    Assign,
    Seq {
        list: &'a VecDeque<Stmt<'a>>,
        next: usize,
    },
    Nop,
    Declare(FrameProgress<(), ()>),
    Exp,
    If {
        true_check: FrameProgress<(), ()>,
        false_check: FrameProgress<(), ()>,
    },
    While,
}

struct StmtFrame<'a> {
    kind: StmtFrameKind<'a>,
    statement: &'a Stmt<'a>,
}

impl<'a> From<&'a Stmt<'a>> for StmtFrame<'a> {
    fn from(s: &'a Stmt<'a>) -> Self {
        match s {
            Stmt::Declare(_, _, _) => Self {
                kind: StmtFrameKind::Declare(New(())),
                statement: s,
            },
            Stmt::Assign(_, _) => Self {
                kind: StmtFrameKind::Assign,
                statement: s,
            },
            Stmt::Return(_) => Self {
                kind: StmtFrameKind::Return,
                statement: s,
            },
            Stmt::Seq(list) => Self {
                kind: StmtFrameKind::Seq { list, next: 0 },
                statement: s,
            },
            Stmt::Nop => Self {
                kind: StmtFrameKind::Nop,
                statement: s,
            },
            Stmt::If { .. } => Self {
                kind: StmtFrameKind::If {
                    true_check: New(()),
                    false_check: New(()),
                },
                statement: s,
            },
            Stmt::While { .. } => Self {
                kind: StmtFrameKind::While,
                statement: s,
            },
            Stmt::Exp(_) => Self {
                kind: StmtFrameKind::Exp,
                statement: s,
            },
        }
    }
}

impl<'a> Stmt<'a> {
    /// Checks whether the statement, provided the current type information
    /// in `types`, is valid and returns a type `t`, if it returns at all.
    /// If it does not return, then any return type is valid.
    fn type_check_recursive(
        self: &'a Stmt<'a>,
        types: &mut TypeMap<'a>,
        t: Type,
    ) -> Result<(), ()> {
        match self {
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
            Stmt::Seq(block) => block
                .iter()
                .try_for_each(|s| s.type_check_recursive(types, t)),
            Stmt::Nop => Ok(()),
            Stmt::Declare(var, var_type, scope) => {
                // disallow shadowing
                if types.contains_key(var) {
                    return Err(());
                }
                types.insert(var, *var_type);
                let result = scope.type_check_recursive(types, t);
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
                    stmt_true
                        .type_check_recursive(types, t)
                        .and(stmt_false.type_check_recursive(types, t))
                } else {
                    Err(())
                }
            }
            Stmt::While { cond, body } => {
                if cond.synthesize(types) == Ok(Type::Bool) {
                    body.type_check_recursive(types, t)
                } else {
                    Err(())
                }
            }
        }
    }

    fn type_check_iterative(
        self: &'a Stmt<'a>,
        types: &mut TypeMap<'a>,
        t: Type,
    ) -> Result<(), ()> {
        let mut stack: Vec<StmtFrame> = Vec::new();
        stack.push(StmtFrame::from(self));

        while let Some(frame) = stack.pop() {
            match frame.kind {
                StmtFrameKind::Return => {
                    let Stmt::Return(exp) = frame.statement else {
                        unreachable!()
                    };
                    match exp.synthesize(types) {
                        Ok(exp_type) if exp_type == t => (),
                        Ok(_) => return Err(()),
                        Err(_) => return Err(()),
                    };
                }
                StmtFrameKind::Assign => {
                    let Stmt::Assign(Lvalue::Ident(var), exp) = frame.statement
                    else {
                        unreachable!()
                    };
                    match (exp.synthesize(types), types.get(var)) {
                        (Ok(t1), Some(t2)) if t1 == *t2 => (),
                        (Ok(_), Some(_)) => return Err(()),
                        (Ok(_), None) => return Err(()),
                        (Err(_), _) => return Err(()),
                    }
                }
                StmtFrameKind::Seq { list, next } => {
                    if next >= list.len() {
                        continue;
                    }
                    stack.push(StmtFrame {
                        kind: StmtFrameKind::Seq {
                            list,
                            next: next + 1,
                        },
                        statement: frame.statement,
                    });
                    stack.push(StmtFrame::from(&list[next]));
                }
                StmtFrameKind::Nop => (),
                StmtFrameKind::Declare(frame_progress) => {
                    let Stmt::Declare(var, var_type, scope) = frame.statement
                    else {
                        unreachable!()
                    };
                    match frame_progress {
                        New(()) => {
                            // disallow shadowing
                            if types.insert(*var, *var_type).is_some() {
                                return Err(());
                            }
                            stack.push(StmtFrame {
                                kind: StmtFrameKind::Declare(Done(())),
                                statement: frame.statement,
                            });
                            stack.push(StmtFrame::from(scope.as_ref()));
                        }
                        InProgress => unreachable!(),
                        Done(()) => {
                            types.remove(var);
                        }
                    }
                }
                StmtFrameKind::Exp => {
                    let Stmt::Exp(exp) = frame.statement else {
                        unreachable!()
                    };
                    let Ok(_) = exp.synthesize(types) else {
                        return Err(());
                    };
                }
                StmtFrameKind::If {
                    true_check,
                    false_check,
                } => {
                    let Stmt::If {
                        cond,
                        stmt_true,
                        stmt_false,
                    } = frame.statement
                    else {
                        unreachable!()
                    };
                    match (true_check, false_check) {
                        (New(()), New(())) => {
                            if cond.synthesize(types) != Ok(Type::Bool) {
                                return Err(());
                            }

                            stack.push(StmtFrame {
                                kind: StmtFrameKind::If {
                                    true_check: InProgress,
                                    false_check: New(()),
                                },
                                statement: frame.statement,
                            });
                            stack.push(StmtFrame::from(stmt_true.as_ref()));
                        }
                        (InProgress, New(())) => {
                            stack.push(StmtFrame {
                                kind: StmtFrameKind::If {
                                    true_check: Done(()),
                                    false_check: InProgress,
                                },
                                statement: frame.statement,
                            });
                            stack.push(StmtFrame::from(stmt_false.as_ref()));
                        }
                        (Done(()), InProgress) => (),
                        _ => unreachable!(),
                    }
                }
                StmtFrameKind::While => {
                    let Stmt::While { cond, body } = frame.statement else {
                        unreachable!()
                    };
                    if cond.synthesize(types) != Ok(Type::Bool) {
                        return Err(());
                    }

                    stack.push(StmtFrame::from(body.as_ref()));
                }
            }
        }

        Ok(())
    }

    fn type_check(
        self: &'a Stmt<'a>,
        types: &mut TypeMap<'a>,
        t: Type,
    ) -> Result<(), ()> {
        self.type_check_iterative(types, t)
    }
}

impl<'a> Program<'a> {
    pub fn type_check(self: &'a Program<'a>) -> Result<(), ()> {
        let mut types = HashMap::new();
        let inner = self.as_ref();
        inner.type_check(&mut types, Type::Int)
    }
}
