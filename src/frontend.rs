pub mod ast;

// hand rolled lexer/regex
pub mod lexer;
mod regex;

// LALRPOP
extern crate lalrpop_util;
use self::lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[allow(unused_imports)]
    #[rustfmt::skip]
    pub c0parser,
    "/frontend/c0parser.rs");

pub mod elab_ast;
pub mod elaboration;

#[cfg(test)]
mod parser_tests {
    use crate::frontend::lexer::Lexer;

    use super::ast::*;
    use super::c0parser::ProgramParser;

    #[test]
    fn empty_main() {
        let input = "int main() {}";
        let lexer = Lexer::new_c0c_lexer(input);
        let parser = ProgramParser::new();
        let result = parser.parse(input, lexer);
        dbg!(&result);
        assert!(result.is_ok());
    }

    #[test]
    fn pemdas() {
        let input = "int main() {return 1 + 2 - 3 * 4 / 5 % 6;}";
        let lexer = Lexer::new_c0c_lexer(input);
        let parser = ProgramParser::new();
        let result = parser.parse(input, lexer).unwrap();
        assert!(result.body.len() == 1);
        let body = result.body[0].clone();
        let expected = Stmt::Return(Exp::BinOp(
            Box::new(Exp::BinOp(
                Box::new(Exp::Num(Num::DecNum(1))),
                BinOp::Plus,
                Box::new(Exp::Num(Num::DecNum(2))),
            )),
            BinOp::Minus,
            Box::new(Exp::BinOp(
                Box::new(Exp::BinOp(
                    Box::new(Exp::BinOp(
                        Box::new(Exp::Num(Num::DecNum(3))),
                        BinOp::Times,
                        Box::new(Exp::Num(Num::DecNum(4))),
                    )),
                    BinOp::Divide,
                    Box::new(Exp::Num(Num::DecNum(5))),
                )),
                BinOp::Modulo,
                Box::new(Exp::Num(Num::DecNum(6))),
            )),
        ));
        dbg!(&body);
        assert!(body == expected);
    }

    #[test]
    fn single_digits() {
        let input = "int main() {return 1;}";
        let parser = ProgramParser::new();
        let lexer = Lexer::new_c0c_lexer(input);
        let result = parser.parse(input, lexer).unwrap();
        assert!(result.body.len() == 1);
        let body = result.body[0].clone();
        let expected = Stmt::Return(Exp::Num(Num::DecNum(1)));
        dbg!(&body);
        assert!(body == expected);
    }

    #[test]
    fn scopes() {
        let input = "int main(){int x = 3; int y = 4; {int x = 5; int y = 6;} return x + y;}";
        let lexer = Lexer::new_c0c_lexer(input);
        let parser = ProgramParser::new();
        let result = parser.parse(input, lexer).unwrap();
        dbg!(&result);
    }
}

#[cfg(test)]
mod elaborate_tests {
    use crate::frontend::lexer::Lexer;

    use super::c0parser::ProgramParser;
    use super::elab_ast;
    use super::elaboration::elaborate;

    #[test]
    fn empty_main() {
        let input = "int main() {}";
        let lexer = Lexer::new_c0c_lexer(input);
        let parser = ProgramParser::new();
        let ast = parser.parse(input, lexer);
        dbg!(&ast);
        assert!(ast.is_ok());
        let elab_ast = elaborate(ast.unwrap()).unwrap();
        dbg!(&elab_ast);
        assert!(elab_ast == elab_ast::Stmt::Nop.into());
    }

    #[test]
    fn scopes() {
        let input = "int main(){int x = 3; int y = 4; {int x = 5; int y = 6;} return x + y;}";
        let lexer = Lexer::new_c0c_lexer(input);
        let parser = ProgramParser::new();
        let ast = parser.parse(input, lexer);
        dbg!(&ast);
        assert!(ast.is_ok());
        let elab_ast = elaborate(ast.unwrap());
        dbg!(&elab_ast);
    }

    #[test]
    fn empty_declare() {
        let input = "int main(){int x;}";
        let lexer = Lexer::new_c0c_lexer(input);
        let parser = ProgramParser::new();
        let ast = parser.parse(input, lexer);
        dbg!(&ast);
        assert!(ast.is_ok());
        let elab_ast = elaborate(ast.unwrap()).unwrap();
        dbg!(&elab_ast);
        assert!(
            elab_ast
                == elab_ast::Stmt::Declare(
                    "x",
                    elab_ast::Type::Int,
                    Box::new(elab_ast::Stmt::Nop)
                )
                .into()
        );
    }

    #[test]
    fn double_declare_scopes() {
        let input_scope = "int main(){int x = 0; { int x = 1; } return x;}";
        let input_noscope = "int main(){int x = 0; int x = 1; return x;}";
        let lexer_scope = Lexer::new_c0c_lexer(input_scope);
        let lexer_noscope = Lexer::new_c0c_lexer(input_noscope);
        let parser_scope = ProgramParser::new();
        let parser_noscope = ProgramParser::new();
        let ast_scope = parser_scope.parse(input_scope, lexer_scope);
        let ast_noscope = parser_noscope.parse(input_noscope, lexer_noscope);
        assert!(ast_scope.is_ok());
        assert!(ast_noscope.is_ok());
        dbg!(&ast_scope);
        dbg!(&ast_noscope);
        let elab_scope = elaborate(ast_scope.unwrap());
        let elab_noscope = elaborate(ast_noscope.unwrap());
        dbg!(&elab_scope);
        dbg!(&elab_noscope);
        assert!(elab_scope != elab_noscope);
    }
}
