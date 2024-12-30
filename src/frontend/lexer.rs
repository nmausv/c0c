#![allow(dead_code)]
use std::{
    error::Error,
    fmt::{self, Display},
};

use super::regex::*;

#[derive(Debug, Eq, PartialEq)]
/// Lexer Error Type
pub enum LexerError {
    UnrecognizedCharacter { line: usize, col: usize },
    UnopenedEndComment { line: usize, col: usize },
    UnclosedStartComment { line: usize, col: usize },
    InvalidInteger { line: usize, col: usize },
}

impl Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnrecognizedCharacter { line, col } => {
                write!(
                    f,
                    "Unrecognized character at line {}, col {}.",
                    line, col
                )
            }
            Self::UnopenedEndComment { line, col } => {
                write!(
                    f,
                    "Unexpected comment end at line {}, col {}.",
                    line, col
                )
            }
            Self::UnclosedStartComment { line, col } => {
                write!(
                    f,
                    "Unclosed comment started at line {}, col {}.",
                    line, col
                )
            }
            Self::InvalidInteger { line, col } => {
                write!(f, "Invalid integer at line {}, col {}.", line, col)
            }
        }
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
///
/// For numbers and identifiers, a string must be attached to distinguish between
/// different numbers/identifiers.
/// These strings will *not* be evaluated to make sure they fit within the integer
/// size limits, and instead this check will be deferred.
///
/// This string is only guaranteed to match the supplied RegExp, further checks
/// (such as bounds checks on integers) must be done downstream.
pub enum Token {
    // Whitespace
    WHITESPACE,
    NEWLINE,
    // Comments
    LINECOMMENT,
    MULTICOMMENTSTART,
    MULTICOMMENTEND,
    // Syntax
    COMMA,
    SEMICOLON,
    SINGLEQUOTE,
    DOUBLEQUOTE,
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
    FSLASH,
    FSLASHEQ,
    PERCENT,
    PERCENTEQ,
    // Identifiers
    IDENT(String),
    // Constants
    DECNUM(String),
    HEXNUM(String),
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
    /// Extract underlying token
    pub fn unmark(self) -> Token {
        self.data
    }
}

/// Priorities of Lexical tokens
///
/// Higher priority means those tokens will be preferred over
/// lower priority tokens.
pub type Priority = i32;

#[derive(Debug, Clone)]
/// The unit for a rule of the lexer.
///
/// - `pattern` is the DFA that corresponds to the regular expression that will
///    match the token
/// - `token_builder` takes in a string that matches the regular expression
///    and creates a token for it.
/// - `priority` is the priority of the token compared to others.
///    Higher priority means that this token will match instead of another.
///
/// Assume that any string passed to `token_builder` has already been
/// checked for invalid sizes/formats.
/// Most tokens won't need more checks than the regular expression, but
/// some information that the regular expression cannot capture, such as
/// integer size limits, needs to be checked externally.
/// In those cases, the user **MUST** check any strings for these size limits
/// before passing them to `token_builder`.
pub struct Lexeme {
    engine: DFA,
    token_builder: fn(&str) -> Token,
    priority: Priority,
}

impl Lexeme {
    /// Convenience function to create Lexemes inline
    fn new(
        pattern: RegExp,
        token_builder: fn(&str) -> Token,
        priority: Priority,
    ) -> Self {
        Lexeme {
            engine: DFA::from_regex(&pattern),
            token_builder,
            priority,
        }
    }

    //TODO LEARN HOW MACROS WORK TO MAKE THIS WAY WAY EASIER

    // Convenience function for simple regular expression
    // lexemes.
    //
    // Will match only and exactly the given word, with
    // default priority of 0.
    //
    // Currently non functional since the closures capture
    // the input token parameter.
    //fn new_keyword(w: &str, t: Token) -> Self {
    //    Lexeme {
    //        pattern: regex::RegExp::from_word(w),
    //        token_builder: move |_: &str| t,
    //        priority: 0
    //    }
    //}
}

/// The main struct for interacting with the lexer
pub struct Lexer {
    /// Stores Regular Expressions, a token constructor, and the priority of the
    /// regular expression/token.
    ///
    /// For tokens with data attached, the attached data will be disregarded in
    /// favor of the matched string.
    lexemes: Vec<Lexeme>,
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
    pub fn new(patterns: Vec<Lexeme>) -> Self {
        Lexer { lexemes: patterns }
    }

    /// Convenience function which adds all of the C0 lexical tokens
    /// to the Lexer.
    ///
    /// For now, super grimy ugly, future plans to create
    /// a macro that makes this easier and less repetitive.
    pub fn new_c0c_lexer() -> Self {
        let default_priority: Priority = 0;
        let patterns: Vec<Lexeme> = vec![
            // Whitespace
            Lexeme::new(
                RegExp::from_charlist(" \t\r"),
                |_: &str| Token::WHITESPACE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("\n"),
                |_: &str| Token::NEWLINE,
                default_priority,
            ),
            // Comments
            Lexeme::new(
                RegExp::from_word("//"),
                |_: &str| Token::LINECOMMENT,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("/*"),
                |_: &str| Token::MULTICOMMENTSTART,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("*/"),
                |_: &str| Token::MULTICOMMENTEND,
                default_priority,
            ),
            // Syntax
            Lexeme::new(
                RegExp::from_word(","),
                |_: &str| Token::COMMA,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word(";"),
                |_: &str| Token::SEMICOLON,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("'"),
                |_: &str| Token::SINGLEQUOTE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("\""),
                |_: &str| Token::DOUBLEQUOTE,
                default_priority,
            ),
            // Delimiters
            Lexeme::new(
                RegExp::from_word("("),
                |_: &str| Token::LPAREN,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word(")"),
                |_: &str| Token::RPAREN,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("["),
                |_: &str| Token::LBRACKET,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("]"),
                |_: &str| Token::RBRACKET,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("{"),
                |_: &str| Token::LBRACE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("}"),
                |_: &str| Token::RBRACE,
                default_priority,
            ),
            // Binary operators
            Lexeme::new(
                RegExp::from_word("+"),
                |_: &str| Token::PLUS,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("-"),
                |_: &str| Token::MINUS,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("*"),
                |_: &str| Token::TIMES,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("/"),
                |_: &str| Token::FSLASH,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("%"),
                |_: &str| Token::PERCENT,
                default_priority,
            ),
            // Assignment operators
            Lexeme::new(
                RegExp::from_word("="),
                |_: &str| Token::EQUALS,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("+="),
                |_: &str| Token::PLUSEQ,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("-="),
                |_: &str| Token::MINUSEQ,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("*="),
                |_: &str| Token::TIMESEQ,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("/="),
                |_: &str| Token::FSLASHEQ,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("%="),
                |_: &str| Token::PERCENTEQ,
                default_priority,
            ),
            // Type keywords
            Lexeme::new(
                RegExp::from_word("int"),
                |_: &str| Token::INT,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("bool"),
                |_: &str| Token::BOOL,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("void"),
                |_: &str| Token::VOID,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("char"),
                |_: &str| Token::CHAR,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("string"),
                |_: &str| Token::STRING,
                default_priority,
            ),
            // Keywords
            Lexeme::new(
                RegExp::from_word("struct"),
                |_: &str| Token::STRUCT,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("typedef"),
                |_: &str| Token::TYPEDEF,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("if"),
                |_: &str| Token::IF,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("else"),
                |_: &str| Token::ELSE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("while"),
                |_: &str| Token::WHILE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("for"),
                |_: &str| Token::FOR,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("continue"),
                |_: &str| Token::CONTINUE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("break"),
                |_: &str| Token::BREAK,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("return"),
                |_: &str| Token::RETURN,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("assert"),
                |_: &str| Token::ASSERT,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("true"),
                |_: &str| Token::TRUE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("false"),
                |_: &str| Token::FALSE,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("NULL"),
                |_: &str| Token::NULL,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("alloc"),
                |_: &str| Token::ALLOC,
                default_priority,
            ),
            Lexeme::new(
                RegExp::from_word("alloc_array"),
                |_: &str| Token::ALLOCARRAY,
                default_priority,
            ),
            // Identifiers
            Lexeme::new(
                // [A-Za-z_][A-Za-z0-9_]*
                RegExp::Split(
                    Box::new(RegExp::Or(
                        Box::new(RegExp::Range('A', 'Z')),
                        Box::new(RegExp::Or(
                            Box::new(RegExp::Range('a', 'z')),
                            Box::new(RegExp::Single('_')),
                        )),
                    )),
                    Box::new(RegExp::Star(Box::new(RegExp::Or(
                        Box::new(RegExp::Range('A', 'Z')),
                        Box::new(RegExp::Or(
                            Box::new(RegExp::Range('a', 'z')),
                            Box::new(RegExp::Or(
                                Box::new(RegExp::Range('0', '9')),
                                Box::new(RegExp::Single('_')),
                            )),
                        )),
                    )))),
                ),
                |id: &str| Token::IDENT(String::from(id)),
                // low priority so keywords are matched first
                -1,
            ),
            // Constants
            // decimal
            Lexeme::new(
                RegExp::Or(
                    Box::new(RegExp::Single('0')),
                    Box::new(RegExp::Split(
                        Box::new(RegExp::Range('1', '9')),
                        Box::new(RegExp::Star(Box::new(RegExp::Range(
                            '0', '9',
                        )))),
                    )),
                ),
                |decnum: &str| Token::DECNUM(String::from(decnum)),
                default_priority,
            ),
            // hexadecimal
            Lexeme::new(
                RegExp::Split(
                    Box::new(RegExp::Single('0')),
                    Box::new(RegExp::Split(
                        Box::new(RegExp::Or(
                            Box::new(RegExp::Single('x')),
                            Box::new(RegExp::Single('X')),
                        )),
                        Box::new(RegExp::Star(Box::new(RegExp::Or(
                            Box::new(RegExp::Range('0', '9')),
                            Box::new(RegExp::Or(
                                Box::new(RegExp::Range('a', 'f')),
                                Box::new(RegExp::Range('A', 'F')),
                            )),
                        )))),
                    )),
                ),
                |hexnum: &str| Token::HEXNUM(String::from(hexnum)),
                default_priority,
            ),
        ];

        Self::new(patterns)
    }

    /// Internal function to match a string against the lexemes
    ///
    /// Returns None if the raw token doesn't match any of the lexemes, or the
    /// token it matched along with the number of consumed characters if the raw
    /// token matched a lexeme.
    ///
    /// Always chooses the longest matching prefix, and in cases of ambiguity
    /// chooses the token with the highest priority.
    fn match_all(&self, raw_token: &str) -> Option<(Token, usize)> {
        // stores vector of (match length, token builder, priority)
        let match_iter: Vec<_> = self
            .lexemes
            .iter()
            // try matching every lexeme
            .map(|lexeme| {
                (
                    lexeme.engine.matches_against(raw_token),
                    lexeme.token_builder,
                    lexeme.priority,
                )
            })
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
            Some((_, _, p)) => p,
        };

        // get list of max length lexemes with max priority
        let candidates: Vec<_> = match_iter
            .iter()
            .filter(|&(l, _, p)| (*l == max_len) && (p == max_pri))
            .collect();

        // if there are multiple options, error out instead of just returning None
        if candidates.len() > 1 {
            panic!("multiple lexemes with longest match and same priority");
        }

        // take the first (and only) candidate
        let &(tok_len, token_builder, _) = candidates[0];

        // return the lexeme
        return Some((token_builder(&raw_token[..tok_len]), tok_len));
    }

    /// Internal function to consume characters from a string until a certain token is reached.
    ///
    /// **TOKENS WITH ATTACHED STRING DATA MUST HAVE THEIR DATA BE THE EMPTY STRING.**
    /// **IF A TOKEN BUILDER PANICS ON EMPTY STRINGS, IT CANNOT BE USED AS A FILTER.**
    ///
    /// Returns `None` if the raw token doesn't match any of the lexemes, or `Some` of the
    /// token it matched along with the number of consumed characters.
    ///
    /// Always chooses the longest matching prefix, and in cases of ambiguity
    /// chooses the token with the highest priority.
    fn skip_until(
        &self,
        raw_token: &str,
        tokens: &[Token],
    ) -> Option<(Token, usize)> {
        let smaller_lexer = Lexer {
            lexemes: self
                .lexemes
                .iter()
                .filter(|&lexeme| tokens.contains(&(lexeme.token_builder)("")))
                .cloned()
                .collect(),
        };

        // skip characters until the first match
        let mut consumed = 0;
        let mut matched = smaller_lexer.match_all(&raw_token[consumed..]);
        while matched.is_none() {
            consumed += 1;
            matched = smaller_lexer.match_all(&raw_token[consumed..]);
        }

        // return that match, and add in the consumed characters
        matched.map(|(t, l)| (t, consumed + l))
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

        // one indexed because that's how (Neo)Vim counts columns/lines.
        let mut col_num: usize = 1;
        let mut line_num: usize = 1;

        let total_len: usize = characters.len();
        let mut consumed_len: usize = 0;

        // marks the current line as being commented
        let mut line_comment: bool = false;

        // stack of starting locations for multiline comments
        // when a closing comment is found, pop the most recent
        // start off the stack.
        // while the stack is not empty, only search for comment start/ends,
        // and newlines (to keep line counts accurate).
        let mut multi_comment_starts: Vec<(usize, usize)> = Vec::new();

        while consumed_len < total_len {
            let opt: Option<(Token, usize)>;
            if line_comment {
                opt = self
                    .skip_until(&characters[consumed_len..], &[Token::NEWLINE])
            } else if multi_comment_starts.len() > 0 {
                opt = self.skip_until(
                    &characters[consumed_len..],
                    &[
                        Token::NEWLINE,
                        Token::MULTICOMMENTSTART,
                        Token::MULTICOMMENTEND,
                    ],
                )
            } else {
                opt = self.match_all(&characters[consumed_len..]);
            }
            match opt {
                None => {
                    return Err(LexerError::UnrecognizedCharacter {
                        line: line_num,
                        col: col_num,
                    });
                }
                Some((Token::WHITESPACE, l)) => {
                    col_num += l;
                    consumed_len += l;
                }
                Some((Token::NEWLINE, l)) => {
                    line_comment = false;
                    line_num += 1;
                    col_num = 1;
                    consumed_len += l;
                }
                Some((Token::LINECOMMENT, l)) => {
                    line_comment = true;
                    col_num += l;
                    consumed_len += l;
                }
                Some((Token::MULTICOMMENTSTART, l)) => {
                    multi_comment_starts.push((line_num, col_num));
                    col_num += l;
                    consumed_len += l;
                }
                Some((Token::MULTICOMMENTEND, l)) => {
                    if let None = multi_comment_starts.pop() {
                        return Err(LexerError::UnopenedEndComment {
                            line: line_num,
                            col: col_num,
                        });
                    }
                    col_num += l;
                    consumed_len += l;
                }
                Some((t, l)) => {
                    col_num += l;
                    tokens.push(MarkedToken {
                        line: line_num + 1,
                        column: col_num + 1,
                        data: t,
                    });
                    consumed_len += l;
                }
            }
        }

        if let Some((line, col)) = multi_comment_starts.pop() {
            return Err(LexerError::UnclosedStartComment { line, col });
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    fn check_tokens(lexed: Vec<MarkedToken>, reference: Vec<Token>) -> bool {
        reference
            == (lexed
                .iter()
                .cloned()
                .map(|x| x.unmark())
                .collect::<Vec<Token>>())
    }

    #[test]
    fn parens() {
        let lexer = Lexer::new_c0c_lexer();
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
        let lexer = Lexer::new_c0c_lexer();
        assert!(check_tokens(
            lexer.tokenize("if if for if for for").expect(""),
            vec![
                Token::IF,
                Token::IF,
                Token::FOR,
                Token::IF,
                Token::FOR,
                Token::FOR,
            ]
        ));
    }

    #[test]
    fn full_main() {
        let lexer = Lexer::new_c0c_lexer();
        assert!(check_tokens(
            lexer.tokenize("int main() {\n  return 0;\n}").expect(""),
            vec![
                Token::INT,
                Token::IDENT(String::from("main")),
                Token::LPAREN,
                Token::RPAREN,
                Token::LBRACE,
                Token::RETURN,
                Token::DECNUM(String::from("0")),
                Token::SEMICOLON,
                Token::RBRACE,
            ]
        ));

        assert!(check_tokens(
            lexer.tokenize("int main() {\n  int x = 0x17;\n  int z = x * 20;\n  return z - z;\n}").expect(""),
            vec![
                Token::INT,
                Token::IDENT(String::from("main")),
                Token::LPAREN,
                Token::RPAREN,
                Token::LBRACE,
                Token::INT,
                Token::IDENT(String::from("x")),
                Token::EQUALS,
                Token::HEXNUM(String::from("0x17")),
                Token::SEMICOLON,
                Token::INT,
                Token::IDENT(String::from("z")),
                Token::EQUALS,
                Token::IDENT(String::from("x")),
                Token::TIMES,
                Token::DECNUM(String::from("20")),
                Token::SEMICOLON,
                Token::RETURN,
                Token::IDENT(String::from("z")),
                Token::MINUS,
                Token::IDENT(String::from("z")),
                Token::SEMICOLON,
                Token::RBRACE,
            ]
        ));
    }

    #[test]
    fn invalid_character() {
        let lexer = Lexer::new_c0c_lexer();
        assert_eq!(
            lexer.tokenize("if if for & if for for").expect_err(""),
            LexerError::UnrecognizedCharacter { line: 1, col: 11 }
        );
    }

    #[test]
    fn mixed_single_line() {
        let lexer = Lexer::new_c0c_lexer();
        assert!(
            check_tokens(
                lexer.tokenize(
                    "int main() {\n// what a cool comment *//*\n    return 0;\n}"
                ).expect(""),
                vec![
                    Token::INT,
                    Token::IDENT(String::from("main")),
                    Token::LPAREN,
                    Token::RPAREN,
                    Token::LBRACE,
                    Token::RETURN,
                    Token::DECNUM(String::from("0")),
                    Token::SEMICOLON,
                    Token::RBRACE
                ]
            )
        );
    }

    #[test]
    fn nested_multi_line() {
        let lexer = Lexer::new_c0c_lexer();
        assert!(check_tokens(
            lexer.tokenize("/* /* /* */ /* */ */ */").expect(""),
            vec![]
        ));
    }

    #[test]
    fn incorrectly_nested_multiline() {
        let lexer = Lexer::new_c0c_lexer();
        assert_eq!(
            lexer.tokenize("/* /* /* */ /* */ */").expect_err(""),
            LexerError::UnclosedStartComment { line: 1, col: 1 }
        );
    }

    #[test]
    fn unopened_end_comment() {
        let lexer = Lexer::new_c0c_lexer();
        assert_eq!(
            lexer.tokenize(
                "int main() {\n/* what a cool comment */\n    return 0;\n}\n*/"
            ).expect_err(""),
            LexerError::UnopenedEndComment { line: 5, col: 1 }
        );
    }
}
