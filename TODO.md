
# Lexer

## Regex
- [ ] Add better error handling for converting `DFA<RegExp>` to `DFA<Option<char>>`
- [x] Privatize the generic, so that the type is only exposed as a DFA.

## Tokenizing

- [ ] Line and column numbers are incorrect, they don't account for whitespace
- [>] Rework: read character by character, create whitespace token and just don't put those token in the token stream
    - should fix the line/column number problem

