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
    CustomError { msg: &'static str },
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
            Self::CustomError { msg } => write!(f, "{msg}"),
        }
    }
}

impl Error for LexerError {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// Every possible token that the lexer needs to distinguish between.
///
/// For example, if it does not need to distinguish between `\n` and `\t`,
/// and can treat them both as arbitrary whitespace, then the `Token` enum
/// only needs a `Whitespace` token, and not separate tokens for `\n` and `\t`.
///
/// You will need to store a RegExp to match each token.
pub enum TokenType {
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
    COLON,
    SINGLEQUOTE,
    DOUBLEQUOTE,
    QUESTIONMARK,
    // Delimiters
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    // Arithmetic Operators
    PLUS,
    MINUS,
    TIMES,
    FSLASH,
    PERCENT,
    SHL,
    SHR,
    AMPERSAND,
    PIPE,
    CARAT,
    // Boolean Operators
    DOUBLEAMPERSAND,
    DOUBLEPIPE,
    // Assignment Operators
    EQUALS,
    PLUSEQ,
    MINUSEQ,
    TIMESEQ,
    FSLASHEQ,
    PERCENTEQ,
    SHLEQ,
    SHREQ,
    AMPERSANDEQ,
    PIPEEQ,
    CARATEQ,
    // Comparison Operators
    EQUALSEQUALS,
    BANGEQUALS,
    LESS,
    LESSEQ,
    GREATER,
    GREATEREQ,
    // Unary Operators
    BANG,
    TILDE,
    // Postfix Operators
    PLUSPLUS,
    MINUSMINUS,
    // Identifiers
    IDENT,
    // Constants
    DECNUM,
    HEXNUM,
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

#[derive(Clone, Debug, PartialEq, Eq)]
/// Stores a slice of the input string, and the assigned token type.
///
/// Note that most token types don't need to store the exact string slice,
/// since for tokens like `IF`, the string slice can be assumed to be `"if"`.
///
/// The reason for this storage is for tokens like `IDENT` and `DECNUM`, which
/// for which the matched string can have meaningful information.
pub struct Token<'input> {
    pub token_type: TokenType,
    pub data: &'input str,
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Location {
    pub line: usize,
    pub column: usize,
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
    token_type: TokenType,
    priority: Priority,
}

impl Lexeme {
    /// Convenience function to create Lexemes inline
    fn new(pattern: RegExp, token_type: TokenType, priority: Priority) -> Self {
        Lexeme {
            engine: DFA::from_regex(&pattern),
            token_type,
            priority,
        }
    }

    // Convenience function for simple regular expression
    // lexemes.
    //
    // Will match only and exactly the given word, with
    // default priority of 0.
    fn from_keyword(w: &str, t: TokenType) -> Self {
        Lexeme {
            engine: DFA::from_regex(&RegExp::from_word(w)),
            token_type: t,
            priority: 0,
        }
    }
}

#[derive(Debug)]
/// The main struct for interacting with the lexer
pub struct Lexer<'input> {
    /// Input string to lex
    input: &'input str,
    /// Bytes consumed from the input
    consumed: usize,
    /// Last produced token location, used for line + column information
    location: Location,
    /// Whether we're currently in a line commnent (`//`)
    line_comment: bool,
    /// Stack to represent the levels of multi line comment starts (`/*`)
    multi_comment_starts: Vec<Location>,
    /// Stores Regular Expressions, token types, and the priority of the
    /// regular expression/token.
    lexemes: Vec<Lexeme>,
}

impl<'input> Lexer<'input> {
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
    pub fn new(patterns: Vec<Lexeme>, input: &'input str) -> Self {
        Lexer {
            input,
            consumed: 0,
            location: Location { line: 1, column: 1 },
            line_comment: false,
            multi_comment_starts: vec![],
            lexemes: patterns,
        }
    }

    /// Convenience function which adds all of the C0 lexical tokens
    /// to the Lexer.
    ///
    /// For now, super grimy ugly, future plans to create
    /// a macro that makes this easier and less repetitive.
    pub fn new_c0c_lexer(input: &'input str) -> Self {
        let default_priority: Priority = 0;
        let patterns: Vec<Lexeme> = vec![
            // Whitespace
            Lexeme::new(
                RegExp::from_charlist(" \t\r"),
                TokenType::WHITESPACE,
                default_priority,
            ),
            Lexeme::from_keyword("\n", TokenType::NEWLINE),
            // Comments
            Lexeme::from_keyword("//", TokenType::LINECOMMENT),
            Lexeme::from_keyword("/*", TokenType::MULTICOMMENTSTART),
            Lexeme::from_keyword("*/", TokenType::MULTICOMMENTEND),
            // Syntax
            Lexeme::from_keyword(",", TokenType::COMMA),
            Lexeme::from_keyword(";", TokenType::SEMICOLON),
            Lexeme::from_keyword(":", TokenType::COLON),
            Lexeme::from_keyword("'", TokenType::SINGLEQUOTE),
            Lexeme::from_keyword("\"", TokenType::DOUBLEQUOTE),
            Lexeme::from_keyword("?", TokenType::QUESTIONMARK),
            // Delimiters
            Lexeme::from_keyword("(", TokenType::LPAREN),
            Lexeme::from_keyword(")", TokenType::RPAREN),
            Lexeme::from_keyword("[", TokenType::LBRACKET),
            Lexeme::from_keyword("]", TokenType::RBRACKET),
            Lexeme::from_keyword("{", TokenType::LBRACE),
            Lexeme::from_keyword("}", TokenType::RBRACE),
            // Arithmetic operators
            Lexeme::from_keyword("+", TokenType::PLUS),
            Lexeme::from_keyword("-", TokenType::MINUS),
            Lexeme::from_keyword("*", TokenType::TIMES),
            Lexeme::from_keyword("/", TokenType::FSLASH),
            Lexeme::from_keyword("%", TokenType::PERCENT),
            Lexeme::from_keyword("<<", TokenType::SHL),
            Lexeme::from_keyword(">>", TokenType::SHR),
            Lexeme::from_keyword("&", TokenType::AMPERSAND),
            Lexeme::from_keyword("|", TokenType::PIPE),
            Lexeme::from_keyword("^", TokenType::CARAT),
            // Boolean operators
            Lexeme::from_keyword("&&", TokenType::DOUBLEAMPERSAND),
            Lexeme::from_keyword("||", TokenType::DOUBLEPIPE),
            // Assignment operators
            Lexeme::from_keyword("=", TokenType::EQUALS),
            Lexeme::from_keyword("+=", TokenType::PLUSEQ),
            Lexeme::from_keyword("-=", TokenType::MINUSEQ),
            Lexeme::from_keyword("*=", TokenType::TIMESEQ),
            Lexeme::from_keyword("/=", TokenType::FSLASHEQ),
            Lexeme::from_keyword("%=", TokenType::PERCENTEQ),
            Lexeme::from_keyword("<<=", TokenType::SHLEQ),
            Lexeme::from_keyword(">>=", TokenType::SHREQ),
            Lexeme::from_keyword("&=", TokenType::AMPERSANDEQ),
            Lexeme::from_keyword("|=", TokenType::PIPEEQ),
            Lexeme::from_keyword("^=", TokenType::CARATEQ),
            // Comparison Operators
            Lexeme::from_keyword("==", TokenType::EQUALSEQUALS),
            Lexeme::from_keyword("!=", TokenType::BANGEQUALS),
            Lexeme::from_keyword("<", TokenType::LESS),
            Lexeme::from_keyword("<=", TokenType::LESSEQ),
            Lexeme::from_keyword(">", TokenType::GREATER),
            Lexeme::from_keyword(">=", TokenType::GREATEREQ),
            // Unary Operators
            Lexeme::from_keyword("!", TokenType::BANG),
            Lexeme::from_keyword("~", TokenType::TILDE),
            // Postfix Operators
            Lexeme::from_keyword("++", TokenType::PLUSPLUS),
            Lexeme::from_keyword("--", TokenType::MINUSMINUS),
            // Type keywords
            Lexeme::from_keyword("int", TokenType::INT),
            Lexeme::from_keyword("bool", TokenType::BOOL),
            Lexeme::from_keyword("void", TokenType::VOID),
            Lexeme::from_keyword("char", TokenType::CHAR),
            Lexeme::from_keyword("string", TokenType::STRING),
            // Keywords
            Lexeme::from_keyword("struct", TokenType::STRUCT),
            Lexeme::from_keyword("typedef", TokenType::TYPEDEF),
            Lexeme::from_keyword("if", TokenType::IF),
            Lexeme::from_keyword("else", TokenType::ELSE),
            Lexeme::from_keyword("while", TokenType::WHILE),
            Lexeme::from_keyword("for", TokenType::FOR),
            Lexeme::from_keyword("continue", TokenType::CONTINUE),
            Lexeme::from_keyword("break", TokenType::BREAK),
            Lexeme::from_keyword("return", TokenType::RETURN),
            Lexeme::from_keyword("assert", TokenType::ASSERT),
            Lexeme::from_keyword("true", TokenType::TRUE),
            Lexeme::from_keyword("false", TokenType::FALSE),
            Lexeme::from_keyword("NULL", TokenType::NULL),
            Lexeme::from_keyword("alloc", TokenType::ALLOC),
            Lexeme::from_keyword("alloc_array", TokenType::ALLOCARRAY),
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
                TokenType::IDENT,
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
                TokenType::DECNUM,
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
                TokenType::HEXNUM,
                default_priority,
            ),
        ];

        Self::new(patterns, input)
    }

    /// Internal function to match a string against the lexemes
    ///
    /// Returns None if the raw string doesn't match any of the lexemes, or Some
    /// token it matched along with the number of consumed characters if the raw
    /// string matched a lexeme.
    ///
    /// Always chooses the longest matching prefix, and in cases of ambiguity
    /// chooses the token with the highest priority.
    fn match_all(&mut self) -> Option<(Token<'input>, Location)> {
        // stores vector of (match length, token, priority)
        let match_iter = self
            .lexemes
            .iter()
            // try matching every lexeme
            .map(|lexeme| {
                (
                    lexeme.engine.matches_against(&self.input[self.consumed..]),
                    lexeme.token_type,
                    lexeme.priority,
                )
            })
            // filter out any non matches
            .filter_map(|(match_len, t, p)| match_len.map(|l| (l, t, p)))
            .collect::<Vec<_>>();

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
        let &(tok_len, token_type, _) = candidates[0];

        let token: Token<'input> = Token {
            token_type,
            data: &self.input[self.consumed..self.consumed + tok_len],
        };

        self.consumed += tok_len;

        // update lexer location, while storing token location
        let token_location = self.location;

        if token.token_type == TokenType::NEWLINE {
            self.location.line += 1;
            self.location.column = 1;
        } else {
            self.location.column += tok_len;
        }

        // return the token
        Some((token, token_location))
    }

    /// Internal function to consume characters from a string until a certain token type is reached.
    ///
    /// Returns `None` if the raw input doesn't match any of the lexemes, or `Some` of the
    /// token it matched along with the number of consumed characters.
    ///
    /// Always chooses the longest matching prefix, and in cases of ambiguity
    /// chooses the token with the highest priority.
    fn skip_until(
        &mut self,
        token_types: &[TokenType],
    ) -> Option<(Token<'input>, Location)> {
        let mut smaller_lexer = Lexer {
            input: self.input,
            consumed: self.consumed,
            location: self.location,
            // smaller lexer not responsible for comments
            line_comment: false,
            multi_comment_starts: vec![],
            lexemes: self
                .lexemes
                .iter()
                .filter(|&lexeme| token_types.contains(&(lexeme.token_type)))
                .cloned()
                .collect(),
        };

        // skip characters until the first match
        let mut matched = smaller_lexer.match_all();
        let input_chars: Vec<_> = self.input.chars().collect();
        while matched.is_none() {
            // out of characters to consume
            if smaller_lexer.consumed >= smaller_lexer.input.len() {
                return None;
            }
            // (preemptively) update location after skipping character
            if input_chars[smaller_lexer.consumed] == '\n' {
                smaller_lexer.location.line += 1;
                smaller_lexer.location.column = 1;
            } else {
                smaller_lexer.location.column += 1;
            }
            // skip a character
            smaller_lexer.consumed += 1;
            matched = smaller_lexer.match_all();
        }

        self.consumed = smaller_lexer.consumed;
        self.location = smaller_lexer.location;

        matched
    }

    pub fn tokenize(
        self,
    ) -> Result<Vec<(Location, Token<'input>, Location)>, LexerError> {
        self.collect()
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, Location, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.consumed >= self.input.len() {
                if let Some(location) = self.multi_comment_starts.pop() {
                    eprintln!("unclosed start comment");
                    return Some(Err(LexerError::UnclosedStartComment {
                        line: location.line,
                        col: location.column,
                    }));
                }
                return None;
            }

            let opt: Option<(Token<'input>, Location)>;

            if self.line_comment {
                opt = self.skip_until(&[TokenType::NEWLINE]);
            } else if !self.multi_comment_starts.is_empty() {
                opt = self.skip_until(&[
                    TokenType::NEWLINE,
                    TokenType::MULTICOMMENTSTART,
                    TokenType::MULTICOMMENTEND,
                ]);
            } else {
                opt = self.match_all();
            }

            if opt.is_none() {
                return Some(Err(LexerError::UnrecognizedCharacter {
                    line: self.location.line,
                    col: self.location.column,
                }));
            }

            let (token, token_location) = opt.unwrap();

            match token.token_type {
                TokenType::WHITESPACE => {}
                TokenType::NEWLINE => {
                    self.line_comment = false;
                }
                TokenType::LINECOMMENT => {
                    self.line_comment = true;
                }
                TokenType::MULTICOMMENTSTART => {
                    self.multi_comment_starts.push(token_location);
                }
                TokenType::MULTICOMMENTEND => {
                    if self.multi_comment_starts.pop().is_none() {
                        return Some(Err(LexerError::UnopenedEndComment {
                            line: token_location.line,
                            col: token_location.column,
                        }));
                    }
                }
                _ => {
                    return Some(Ok((self.location, token, token_location)));
                }
            }
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    fn check_tokens(
        lexed: Vec<(Location, Token, Location)>,
        reference: Vec<Token>,
    ) -> bool {
        reference == lexed.into_iter().map(|x| x.1).collect::<Vec<_>>()
    }

    #[test]
    fn parens() {
        assert!(check_tokens(
            Lexer::new_c0c_lexer("").tokenize().expect(""),
            vec![]
        ));
        assert!(check_tokens(
            Lexer::new_c0c_lexer("(").tokenize().expect(""),
            vec![Token {
                token_type: TokenType::LPAREN,
                data: "("
            }]
        ));
        assert!(check_tokens(
            Lexer::new_c0c_lexer("(((").tokenize().expect(""),
            vec![
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
            ]
        ));
        assert!(check_tokens(
            Lexer::new_c0c_lexer("()(())()").tokenize().expect(""),
            vec![
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::RPAREN,
                    data: ")"
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::RPAREN,
                    data: ")"
                },
                Token {
                    token_type: TokenType::RPAREN,
                    data: ")"
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::RPAREN,
                    data: ")"
                },
            ]
        ));
    }

    #[test]
    fn keywords() {
        assert!(check_tokens(
            Lexer::new_c0c_lexer("if if for if for for")
                .tokenize()
                .expect(""),
            vec![
                Token {
                    token_type: TokenType::IF,
                    data: "if"
                },
                Token {
                    token_type: TokenType::IF,
                    data: "if"
                },
                Token {
                    token_type: TokenType::FOR,
                    data: "for"
                },
                Token {
                    token_type: TokenType::IF,
                    data: "if"
                },
                Token {
                    token_type: TokenType::FOR,
                    data: "for"
                },
                Token {
                    token_type: TokenType::FOR,
                    data: "for"
                },
            ]
        ));
    }

    #[test]
    fn full_main() {
        assert!(check_tokens(
            Lexer::new_c0c_lexer("int main() {\n  return 0;\n}")
                .tokenize()
                .expect(""),
            vec![
                Token {
                    token_type: TokenType::INT,
                    data: "int"
                },
                Token {
                    token_type: TokenType::IDENT,
                    data: "main"
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::RPAREN,
                    data: ")"
                },
                Token {
                    token_type: TokenType::LBRACE,
                    data: "{"
                },
                Token {
                    token_type: TokenType::RETURN,
                    data: "return"
                },
                Token {
                    token_type: TokenType::DECNUM,
                    data: "0"
                },
                Token {
                    token_type: TokenType::SEMICOLON,
                    data: ";"
                },
                Token {
                    token_type: TokenType::RBRACE,
                    data: "}"
                },
            ]
        ));

        assert!(check_tokens(
            Lexer::new_c0c_lexer("int main() {\n  int x = 0x17;\n  int z = x * 20;\n  return z - z;\n}").tokenize().expect(""),
            vec![
                Token{token_type: TokenType::INT, data: "int"},
                Token{token_type: TokenType::IDENT, data: "main"},
                Token{token_type: TokenType::LPAREN, data: "("},
                Token{token_type: TokenType::RPAREN, data: ")"},
                Token{token_type: TokenType::LBRACE, data: "{"},
                Token{token_type: TokenType::INT, data: "int"},
                Token{token_type: TokenType::IDENT, data: "x"},
                Token{token_type: TokenType::EQUALS, data: "="},
                Token{token_type: TokenType::HEXNUM, data: "0x17"},
                Token{token_type: TokenType::SEMICOLON, data: ";"},
                Token{token_type: TokenType::INT, data: "int"},
                Token{token_type: TokenType::IDENT, data: "z"},
                Token{token_type: TokenType::EQUALS, data: "="},
                Token{token_type: TokenType::IDENT, data: "x"},
                Token{token_type: TokenType::TIMES, data: "*"},
                Token{token_type: TokenType::DECNUM, data: "20"},
                Token{token_type: TokenType::SEMICOLON, data: ";"},
                Token{token_type: TokenType::RETURN, data: "return"},
                Token{token_type: TokenType::IDENT, data: "z"},
                Token{token_type: TokenType::MINUS, data: "-"},
                Token{token_type: TokenType::IDENT, data: "z"},
                Token{token_type: TokenType::SEMICOLON, data: ";"},
                Token{token_type: TokenType::RBRACE, data: "}"},
            ]
        ));
    }

    #[test]
    fn invalid_character() {
        assert_eq!(
            Lexer::new_c0c_lexer("if if for \\ if for for")
                .tokenize()
                .expect_err(""),
            LexerError::UnrecognizedCharacter { line: 1, col: 11 }
        );
    }

    #[test]
    fn mixed_single_line() {
        assert!(check_tokens(
            Lexer::new_c0c_lexer(
                "int main() {\n// what a cool comment *//*\n    return 0;\n}"
            )
            .tokenize()
            .expect(""),
            vec![
                Token {
                    token_type: TokenType::INT,
                    data: "int"
                },
                Token {
                    token_type: TokenType::IDENT,
                    data: "main"
                },
                Token {
                    token_type: TokenType::LPAREN,
                    data: "("
                },
                Token {
                    token_type: TokenType::RPAREN,
                    data: ")"
                },
                Token {
                    token_type: TokenType::LBRACE,
                    data: "{"
                },
                Token {
                    token_type: TokenType::RETURN,
                    data: "return"
                },
                Token {
                    token_type: TokenType::DECNUM,
                    data: "0"
                },
                Token {
                    token_type: TokenType::SEMICOLON,
                    data: ";"
                },
                Token {
                    token_type: TokenType::RBRACE,
                    data: "}"
                },
            ]
        ));
    }

    #[test]
    fn nested_multi_line() {
        assert!(check_tokens(
            Lexer::new_c0c_lexer("/* /* /* */ /* */ */ */")
                .tokenize()
                .expect(""),
            vec![]
        ));
    }

    #[test]
    fn incorrectly_nested_multiline() {
        assert_eq!(
            Lexer::new_c0c_lexer("/* /* /* */ /* */ */")
                .tokenize()
                .expect_err(""),
            LexerError::UnclosedStartComment { line: 1, col: 1 }
        );
    }

    #[test]
    fn unopened_end_comment() {
        assert_eq!(
            Lexer::new_c0c_lexer(
                "int main() {\n/* what a cool comment */\n    return 0;\n}\n*/"
            )
            .tokenize()
            .expect_err(""),
            LexerError::UnopenedEndComment { line: 5, col: 1 }
        );
    }
}
