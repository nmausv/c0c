#![allow(dead_code)]
use std::{
    error::Error,
    fmt::{self, Display},
};

use crate::c0c_lib::regex;

use super::regex::DFA;

#[derive(Debug)]
/// Lexer Error Type
struct LexerError {
    line: usize,
    col: usize,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Lexer error starting at line, col: ({}, {})",
            self.line, self.col
        )
    }
}

impl Error for LexerError {}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Every possible token that the lexer needs to distinguish between.
///
/// For example, if it does not need to distinguish between `\n` and `\t`,
/// and can treat them both as arbitrary whitespace, then the `Token` enum
/// only needs a `Whitespace` token, and not separate tokens for `\n` and `\t`.
///
/// You will need to store a RegExp to match each token.
pub enum Token {
    // Syntax
    COMMA,
    SEMICOLON,
    // Delimiters
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    // Binary Operators
    EQUALS,
    PLUS,
    PLUSEQ,
    MINUS, // also a unary operation, kind of annoying
    MINUSEQ,
    TIMES,
    TIMESEQ,
    DIV,
    DIVEQ,
    PERCENT,
    PERCENTEQ,
    // Identifiers
    IDENT(String),
    // Constants
    NUM(usize),
    // Type Keywords
    INT,
    BOOL,
    VOID,
    CHAR,
    STRING,
    // Keywords
    STRUCT,
    TYPEDEF,
    IF,
    ELSE,
    WHILE,
    FOR,
    CONTINUE,
    BREAK,
    RETURN,
    ASSERT,
    TRUE,
    FALSE,
    NULL,
    ALLOC,
    ALLOCARRAY,
}

/// Type for keeping location information inside a file alongside a token
#[derive(Debug, Clone)]
pub struct MarkedToken {
    line: usize,
    column: usize,
    data: Token,
}

impl MarkedToken {
    pub fn unmark(self) -> Token {
        self.data
    }
}

/// The main struct for interacting with the lexer
struct Lexer {
    /// Stores Regular Expressions, a token constructor, and the priority of the
    /// regular expression/token.
    ///
    /// For tokens with data attached, the attached data will be disregarded in
    /// favor of the matched string.
    lexemes: Vec<(regex::DFA<Option<char>>, fn(&str) -> Token, i32)>,
}

impl Lexer {
    /// Create a lexer with a given list of lexemes to match.
    ///
    /// Each lexeme needs to have an associated Regular Expression,
    /// a function which takes a (guaranteed) matching string to that
    /// regular expression and produces a Token, and a priority for
    /// the lexeme.
    ///
    /// For instance, we want to lex "int while = 0;" as
    /// [INT, WHILE, EQUALS, NUM(0), SEMICOLON]
    /// instead of
    /// [INT, IDENT("while"), NUM(0), SEMICOLON]
    /// so we need to prioritize keywords or deprioritize identifiers.
    pub fn new(patterns: Vec<(regex::RegExp, fn(&str) -> Token, i32)>) -> Self {
        Lexer {
            lexemes: patterns
                .iter()
                .map(|(r, t, p)| (DFA::from_regex(&r), *t, *p))
                .collect(),
        }
    }

    /// Internal function to match a string without whitespace against the lexemes
    ///
    /// Returns None if the raw token doesn't match any of the lexemes, or the
    /// token it matched along with the number of consumed characters if the raw
    /// token matched a lexeme.
    ///
    /// Always chooses the longest matching prefix, and in cases of ambiguity
    /// chooses the token with the highest priority.
    fn match_raw(&self, raw_token: &str) -> Option<(Token, usize)> {
        let match_iter: Vec<_> = self
            .lexemes
            .iter()
            // try matching every lexeme
            .map(|(dfa, t, p)| (dfa.matches_against(raw_token), t, p))
            // filter out any non matches
            .filter_map(|(match_len, t, p)| match_len.map(|l| (l, t, p)))
            .collect();

        // get maximum match length
        let max_len = match match_iter
            .iter()
            .max_by(|(l1, _, _), (l2, _, _)| l1.cmp(l2))
        {
            None => return None,
            Some((l, _, _)) => *l,
        };

        if max_len == 0 {
            panic!("lexemes cannot be zero width");
        }

        // get maximum priority with longest match length
        let max_pri = match match_iter
            .iter()
            .filter(|(l, _, _)| *l == max_len)
            .max_by(|(_, _, p1), (_, _, p2)| p1.cmp(p2))
        {
            None => {
                panic!("expected longest match to have at least one priority")
            }
            Some((_, _, &p)) => p,
        };

        // get list of candidate lexemes
        let candidates: Vec<_> = match_iter
            .iter()
            .filter(|(l, _, &p)| (*l == max_len) && (p == max_pri))
            .collect();

        // if there are multiple options, error out instead of just returning None
        if candidates.len() > 1 {
            todo!();
        }

        // take the first (and only) candidate
        let (tok_len, token_builder, _) = candidates[0];

        return Some((token_builder(&raw_token[..*tok_len]), *tok_len));
    }

    /// Takes in a string of characters, and returns a string of tokens
    ///
    /// Tokens are split on whitespace, and then matched against the `Token` enum
    /// via Regular Expressions.
    pub fn tokenize(
        &self,
        characters: &str,
    ) -> Result<Vec<MarkedToken>, LexerError> {
        // stores stream of tokens lexed
        let mut tokens = Vec::new();

        // store the current amount of characters consumed from the raw tokens
        // declared outside to prevent repeated allocation/deallocation
        let mut col_num: usize;

        // store token index number
        // declared outside to prevent repeated allocation/deallocation
        let mut tok_ind: usize;

        for (line_num, line) in characters.lines().enumerate() {
            col_num = 0;
            // read line by line for debug information
            // split the file_string by whitespace
            for raw_token in line.split_whitespace() {
                tok_ind = 0;
                while tok_ind < raw_token.len() {
                    // match components of the raw token until done
                    // for example, a raw token might be "x+1"
                    // which we want to tokenize as [IDENT("x"), PLUS, DECNUM(1)]
                    match self.match_raw(&raw_token[tok_ind..]) {
                        None => {
                            return Err(LexerError {
                                line: line_num,
                                col: col_num,
                            })
                        }
                        Some((t, l)) => {
                            tok_ind += l;
                            col_num += l;
                            tokens.push(MarkedToken {
                                line: line_num,
                                column: col_num,
                                data: t,
                            });
                        }
                    }
                }
            }
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod lexer_tests {
    use crate::c0c_lib::regex::RegExp;

    use super::{Lexer, MarkedToken, Token};

    fn check_tokens(lexed: Vec<MarkedToken>, reference: Vec<Token>) -> bool {
        reference
            == (lexed
                .iter()
                .map(|x| x.clone().unmark())
                .collect::<Vec<Token>>())
    }

    #[test]
    fn parens() {
        let patterns: Vec<(RegExp, fn(&str) -> Token, i32)> = vec![
            (RegExp::Single('('), |_| Token::LPAREN, 0),
            (RegExp::Single(')'), |_| Token::RPAREN, 0),
        ];
        let lexer = Lexer::new(patterns);
        assert!(check_tokens(lexer.tokenize("").expect(""), vec![]));
        assert!(check_tokens(
            lexer.tokenize("(").expect(""),
            vec![Token::LPAREN]
        ));
        assert!(check_tokens(
            lexer.tokenize("(((").expect(""),
            vec![Token::LPAREN, Token::LPAREN, Token::LPAREN]
        ));
        assert!(check_tokens(
            lexer.tokenize("()(())()").expect(""),
            vec![
                Token::LPAREN,
                Token::RPAREN,
                Token::LPAREN,
                Token::LPAREN,
                Token::RPAREN,
                Token::RPAREN,
                Token::LPAREN,
                Token::RPAREN
            ]
        ));
    }

    #[test]
    fn keywords() {
        let patterns: Vec<(RegExp, fn(&str) -> Token, i32)> = vec![
            (RegExp::Split(Box::new('i'), Box::new('f')), |_| Token::IF, 0),
            (RegExp::Single(')'), |_| Token::RPAREN, 0),
        ];
    }
}
