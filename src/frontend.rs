pub mod ast;

// hand rolled lexer/regex
// pub mod lexer;
// mod regex;

// LALRPOP
extern crate lalrpop_util;
use self::lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    pub c0parser,
    "/frontend/c0parser.rs");

pub mod elab_ast;
pub mod elaboration;

#[cfg(test)]
mod parser_tests {
    use super::ast::*;
    use super::c0parser::ProgramParser;

    #[test]
    fn empty_main() {
        let input = "int main() {}";
        let parser = ProgramParser::new();
        let result = parser.parse(input);
        dbg!(&result);
        assert!(result.is_ok());
    }

    #[test]
    fn pemdas() {
        let input = "int main() {return 1 + 2 - 3 * 4 / 5 % 6;}";
        let parser = ProgramParser::new();
        let result = parser.parse(input).unwrap();
        assert!(result.body.len() == 1);
        let body = result.body[0].clone();
        let expected = Stmt::Return(Exp::BinOp(
            Box::new(Exp::BinOp(
                Box::new(Exp::Num(1)),
                BinOp::Plus,
                Box::new(Exp::Num(2)),
            )),
            BinOp::Minus,
            Box::new(Exp::BinOp(
                Box::new(Exp::BinOp(
                    Box::new(Exp::BinOp(
                        Box::new(Exp::Num(3)),
                        BinOp::Times,
                        Box::new(Exp::Num(4)),
                    )),
                    BinOp::Divide,
                    Box::new(Exp::Num(5)),
                )),
                BinOp::Modulo,
                Box::new(Exp::Num(6)),
            )),
        ));
        dbg!(&body);
        assert!(body == expected);
    }

    #[test]
    fn single_digits() {
        let input = "int main() {return 1;}";
        let parser = ProgramParser::new();
        let result = parser.parse(input).unwrap();
        assert!(result.body.len() == 1);
        let body = result.body[0].clone();
        let expected = Stmt::Return(Exp::Num(1));
        dbg!(&body);
        assert!(body == expected);
    }

    #[test]
    fn scopes() {
        let input = "int main(){int x = 3; int y = 4; {int x = 5; int y = 6;} return x + y;}";
        let parser = ProgramParser::new();
        let result = parser.parse(input).unwrap();
        dbg!(&result);
        //TODO
        assert!(true);
    }
}

#[cfg(test)]
mod elaborate_tests {
    use super::c0parser::ProgramParser;
    use super::elab_ast;
    use super::elaboration::elaborate;

    #[test]
    fn empty_main() {
        let input = "int main() {}";
        let parser = ProgramParser::new();
        let ast = parser.parse(input);
        dbg!(&ast);
        assert!(ast.is_ok());
        let elab_ast = elaborate(ast.unwrap());
        dbg!(&elab_ast);
        assert!(elab_ast == elab_ast::Stmt::Nop);
    }

    #[test]
    fn scopes() {
        let input = "int main(){int x = 3; int y = 4; {int x = 5; int y = 6;} return x + y;}";
        let parser = ProgramParser::new();
        let ast = parser.parse(input);
        dbg!(&ast);
        assert!(ast.is_ok());
        let elab_ast = elaborate(ast.unwrap());
        dbg!(&elab_ast);
        //TODO
        assert!(true);
    }

    #[test]
    fn empty_declare() {
        let input = "int main(){int x;}";
        let parser = ProgramParser::new();
        let ast = parser.parse(input);
        dbg!(&ast);
        assert!(ast.is_ok());
        let elab_ast = elaborate(ast.unwrap());
        dbg!(&elab_ast);
        assert!(
            elab_ast
                == elab_ast::Stmt::Declare(
                    String::from("x"),
                    elab_ast::Type::Int,
                    Box::new(elab_ast::Stmt::Nop)
                )
        );
    }

    #[test]
    fn double_declare_scopes() {
        let input_scope = "int main(){int x = 0; { int x = 1; } return x;}";
        let input_noscope = "int main(){int x = 0; int x = 1; return x;}";
        let parser = ProgramParser::new();
        let ast_scope = parser.parse(input_scope);
        let ast_noscope = parser.parse(input_noscope);
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
