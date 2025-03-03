use std::collections::VecDeque;

use crate::frontend::elab_ast::{Program, Stmt};

#[derive(Debug)]
enum FrameProgress<T> {
    New,
    InProgress,
    Done(T),
}

use FrameProgress::*;

enum Frame<'a> {
    Declare(&'a Stmt<'a>),
    Assign,
    Nop,
    Seq {
        list: &'a VecDeque<Stmt<'a>>,
        next: usize,
        any_returns: bool,
    },
    Exp,
    While,
    If {
        stmt_true: &'a Stmt<'a>,
        return_true: FrameProgress<bool>,
        stmt_false: &'a Stmt<'a>,
        return_false: FrameProgress<bool>,
    },
    Return,
}

impl<'a> std::fmt::Debug for Frame<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Frame::Declare(_) => write!(f, "[Declare {{scope}}]"),
            Frame::Assign => write!(f, "[Assign]"),
            Frame::Nop => write!(f, "[Nop]"),
            Frame::Seq {
                list: _,
                next,
                any_returns,
            } => write!(
                f,
                "[Seq {{next: {next}}} {{any_returns: {any_returns}}}]"
            ),
            Frame::Exp => write!(f, "[Exp]"),
            Frame::While => write!(f, "[While]"),
            Frame::If {
                stmt_true: _,
                return_true,
                stmt_false: _,
                return_false,
            } => {
                write!(f, "[If {{return_true: {return_true:?}}} {{return_false: {return_false:?}}}]")
            }
            Frame::Return => write!(f, "[Return]"),
        }
    }
}

fn make_frame<'a>(s: &'a Stmt<'a>) -> Frame<'a> {
    match s {
        Stmt::Declare(_, _, scope) => Frame::Declare(scope),
        Stmt::Assign(_, _) => Frame::Assign,
        Stmt::Return(_) => Frame::Return,
        Stmt::Seq(list) => Frame::Seq {
            list,
            next: 0,
            any_returns: false,
        },
        Stmt::Nop => Frame::Nop,
        Stmt::If {
            cond: _,
            stmt_true,
            stmt_false,
        } => Frame::If {
            stmt_true,
            return_true: New,
            stmt_false,
            return_false: New,
        },
        Stmt::While { .. } => Frame::While,
        Stmt::Exp(_) => Frame::Exp,
    }
}

fn return_check_statement(s: &Stmt) -> bool {
    let mut stack: Vec<Frame> = Vec::new();
    let mut returns: bool = false;

    stack.push(make_frame(s));

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Declare(scope) => stack.push(make_frame(scope)),
            Frame::Assign => returns = false,
            Frame::Nop => returns = false,
            Frame::Seq {
                list,
                next,
                any_returns,
            } => {
                if next < list.len() {
                    stack.push(Frame::Seq {
                        list,
                        next: next + 1,
                        any_returns: any_returns || returns,
                    });
                    stack.push(make_frame(&list[next]));
                } else {
                    returns |= any_returns;
                }
            }
            Frame::Exp => returns = false,
            Frame::While => returns = false,
            Frame::If {
                stmt_true,
                return_true,
                stmt_false,
                return_false,
            } => match (return_true, return_false) {
                (New, New) => {
                    stack.push(Frame::If {
                        stmt_true,
                        return_true: InProgress,
                        stmt_false,
                        return_false: New,
                    });
                    stack.push(make_frame(stmt_true));
                }
                (InProgress, New) => {
                    stack.push(Frame::If {
                        stmt_true,
                        return_true: Done(returns),
                        stmt_false,
                        return_false: InProgress,
                    });
                    stack.push(make_frame(stmt_false));
                }
                (Done(if_returns), InProgress) => {
                    returns &= if_returns;
                }
                _ => unreachable!(),
            },
            Frame::Return => returns = true,
        }
    }

    returns
}

pub fn return_check(program: &Program) -> Result<(), ()> {
    if return_check_statement(program.as_ref()) {
        Ok(())
    } else {
        Err(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::{frontend::elab_ast, static_analysis::returns::return_check};

    #[test]
    fn empty() {
        let program: elab_ast::Program = elab_ast::Stmt::Nop.into();
        assert!(return_check(&program).is_err());
    }

    #[test]
    fn sequence() {
        let mut seq = VecDeque::from(vec![
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
            elab_ast::Stmt::Exp(elab_ast::Exp::Num(0)),
        ]);
        let program: elab_ast::Program =
            elab_ast::Stmt::Seq(seq.clone()).into();

        assert!(return_check(&program).is_err());

        seq.push_back(elab_ast::Stmt::Return(elab_ast::Exp::Num(0)));
        let program = elab_ast::Stmt::Seq(seq).into();

        assert!(return_check(&program).is_ok());
    }
}
