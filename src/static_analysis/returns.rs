use std::collections::VecDeque;

use crate::frontend::elab_ast::{Program, Stmt};
use crate::heap_recursion::FrameProgress::{self, Done, InProgress, New};

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
        stmt_true: FrameProgress<&'a Stmt<'a>, bool>,
        stmt_false: FrameProgress<&'a Stmt<'a>, bool>,
    },
    Return,
}

impl<'a> Frame<'a> {
    fn new(s: &'a Stmt<'a>) -> Self {
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
                stmt_true: New(stmt_true),
                stmt_false: New(stmt_false),
            },
            Stmt::While { .. } => Frame::While,
            Stmt::Exp(_) => Frame::Exp,
        }
    }
}

impl<'a> Stmt<'a> {
    fn return_check_recursive(self: &'a Stmt<'a>) -> bool {
        match self {
            Stmt::Declare(_, _, stmt) => stmt.return_check_recursive(),
            Stmt::Assign(_, _) => false,
            Stmt::Return(_) => true,
            Stmt::Seq(seq) => seq.iter().any(|s| s.return_check_recursive()),
            Stmt::Nop => false,
            Stmt::If {
                cond: _,
                stmt_true,
                stmt_false,
            } => {
                stmt_true.return_check_recursive()
                    && stmt_false.return_check_recursive()
            }
            Stmt::While { .. } => false,
            Stmt::Exp(_) => false,
        }
    }

    fn return_check_iterative(self: &'a Stmt<'a>) -> bool {
        let mut stack: Vec<Frame> = Vec::new();
        let mut returns: bool = false;

        stack.push(Frame::new(self));

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Declare(scope) => stack.push(Frame::new(scope)),
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
                        stack.push(Frame::new(&list[next]));
                    } else {
                        returns |= any_returns;
                    }
                }
                Frame::Exp => returns = false,
                Frame::While => returns = false,
                Frame::If {
                    stmt_true,
                    stmt_false,
                } => match (stmt_true, stmt_false) {
                    (New(stmt_true), New(stmt_false)) => {
                        stack.push(Frame::If {
                            stmt_true: InProgress,
                            stmt_false: New(stmt_false),
                        });
                        stack.push(Frame::new(stmt_true));
                    }
                    (InProgress, New(stmt_false)) => {
                        stack.push(Frame::If {
                            stmt_true: Done(returns),
                            stmt_false: InProgress,
                        });
                        stack.push(Frame::new(stmt_false));
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
}

impl<'a> Program<'a> {
    /// Checks whether every possible control path through the given program
    /// returns.
    ///
    /// Note that code like
    /// ```c
    /// int main() {
    ///     while (true) {
    ///         return 0;
    ///     }
    /// }
    /// ```
    /// will be recorded as not necessarily returning, since in general
    /// `while` bodies may not be executed, and compile time evaluation
    /// of the loop guard is not yet implemented.
    ///
    /// Thus, the above code can be amended to
    /// ```c
    /// int main() {
    ///     while (true) {
    ///         return 0;
    ///     }
    ///
    ///     return -1;
    /// }
    /// ```
    /// to maintain the same behaviour while respecting the current
    /// return checker.
    pub fn return_check(self: &'a Program<'a>) -> Result<(), ()> {
        if self.as_ref().return_check_iterative() {
            Ok(())
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::frontend::elab_ast;

    #[test]
    fn empty() {
        let program: elab_ast::Program = elab_ast::Stmt::Nop.into();
        assert!(program.return_check().is_err());
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

        assert!(program.return_check().is_err());

        seq.push_back(elab_ast::Stmt::Return(elab_ast::Exp::Num(0)));
        let program: elab_ast::Program = elab_ast::Stmt::Seq(seq).into();

        assert!(program.return_check().is_ok());
    }
}
