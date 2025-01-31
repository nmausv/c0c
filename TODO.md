
# Global
- [x] pretty print functionality

# Front End

## Hand rolled

### Regex
- [ ] Add better error handling for converting `DFA<RegExp>` to `DFA<Option<char>>`
- [x] Privatize the generic, so that the type is only exposed as a DFA.

### Tokenizing
- [ ] Line and column numbers are incorrect, they don't account for whitespace
- [>] Rework: read character by character, create whitespace token and just don't put those token in the token stream
    - should fix the line/column number problem

## LALRPOP
- [x] enable comments
- [x] create `ast` types for LALRPOP to parse into
- [x] add L2 additions to parser
    - [x] Control
        - [x] If
            - [x] handle dangling else via open/closed statements
        - [x] For
        - [x] While
    - [x] binops
    - [x] unops
    - [x] postops

## Translation
- [x] create `elab_ast` types which have scope information
- [x] write `translate` function to convert `ast` to `elab_ast`
    - [x] elaborate `for` loops into equivalent `while` loops (with proper scoping)

# Static Semantics

## Initialization
- [x] ensure variables are defined before being used, and `return`s define all and only variables currently in scope
- [x] are double declares allowed? for instance:
    ```c
    int main() {
        int x = 0;
        int x = 1;
        return x;
    }
    ```
    - `gcc` doesn't allow it, so I guess I won't either, but there's a problem: double declarations are allowed in different scopes, for example
    ```c
    int main() {
        int x = 0;
        { int x = 1; }
        return x;
    }
    ```
    is valid and returns 0.
    - further update: distinguishing the above two cases is actually not possible with the elaboration procedure I'm using, and I can't really tell how to extend it to work. moreover, the lecture notes actually specify that we can't declare variables twice, so *neither* of the above two programs are valid, while
    ```c
    int main() {
        { int x = 0; }
        int x = 1;
        return x;
    }
    ```
    is valid and returns 1.
    for now I'll just disallow double declares entirely, but I could allow shadowing in the future
- [>] tests
    - [x] check double `declare`, something like `Declare(x, int, Seq([Declare(x, int, Nop), Assign(x, x)]))`

## Typechecking

- [x] Ensure that functions always return the same type
- [>] tests
    - [ ] TODO

# IR Translation
- [x] separate between commands (impure) and expressions (pure)
- [>] tests
    - [ ] TODO

# Code Generation

## Abstract Assembly
- [x] write a lightweight runner to allow end to end testing
