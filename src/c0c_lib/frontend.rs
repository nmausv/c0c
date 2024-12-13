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
    c0parser,
    "/c0c_lib/frontend/c0parser.rs"
);

#[cfg(test)]
mod frontend_tests {
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
        let input = "int main() {return 1;}";
        let parser = ProgramParser::new();
        let result = parser.parse(input);
        dbg!(&result);
        assert!(false);
    }
}
