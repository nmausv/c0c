# Syntax

## Whitespace

Whitespace is one of:
- `< >`
- `<\t>`
- `<\v>`
- `<\r>`
- `<\n>`
- `<\f>`

## Grammar
<program>   ::=     `int` `ident` `()` <block>
<block>     ::=     { <stmts> }
<stmts>     ::=     `e` (empty)
            |       <block>
            |       <stmt> <stmts>
<stmt>      ::=     <decl> ;
            |       <simp> ;
            |       `return` <exp>;
<decl>      ::=     `int` `ident`
            |       `int` `ident` = <exp>
<simp>      ::=     <lvalue> <asnop> <exp>
<lvalue>    ::=     `ident`
            |       ( <lvalue> )
<exp>       ::=     ( <exp> )
            |       <intconst>
            |       `ident`
            |       <exp> <binop> <exp>
            |       `-` <exp>
<intconst>  ::=     `decnum`
            |       `hexnum`
<asnop>     ::=     `=` | `+=` | `-=` | `*=` | `/=` | `%=`
<binop>     ::=     `+` | `-` | `*` | `/` | `%`

## Lexical tokens

<ident>     ::=     `[A-Za-z_][A-Za-z0-9]*`
<num>       ::=     <decnum> | <hexnum>
<decnum>    ::=     `0 | [1-9][0-9]`
<hexnum>    ::=     `0[xX][0-9a-fA-F]+`
<unop>      ::=     `-`
<binop>     ::=     `+` | `-` | `*` | `/` | `%`
<asnop>     ::=     `=` | `+=` | `-=` | `*=` | `/=` | `%=`
<reserved>  ::=     `--`

## Comments
### Multi-line Comment
`/* ... */`
may be nested, must have balanced delimiters
### Single line comment
`// ...` for

### Note on ambiguity

Code such as
```C
5 *//one line comment
3
```
is either parsed as `5 * 3`, parsing and ignoring the line comment
or parsed as a syntax error, since `*/` was not matched.

IGNORE FOR NOW

## Precedence

operator    |   associates  |   Class   |   Meaning
`-`         |   right       |   unary   |   unary negation
`* / %`     |   left        |   binary  |   integer multiplication, division, modulo
`+ -`       |   left        |   binary  |   integer addition, subtraction
`=  += -=`  |   right       |   binary  |   assignment
`*= /= %=`

## Reserved Keywords

- `struct`
- `typedef`
- `if`
- `else`
- `while`
- `for`
- `continue`
- `break`
- `return`
- `assert`
- `true`
- `false`
- `NULL`
- `alloc`
- `alloc_array`
- `int`
- `bool`
- `void`
- `char`
- `string`

# Static Semantics

## Declarations

Every variable must be declared (with the correct type) before being used.
Variables may not be redeclared.

## Initialization checking

Along each control flow path that starts from a variable declaration, that variable is initialized before it is used. Even if a variable declaration is after a `return` statment and therefore can't be executed, it must be treated as the beginning of a control flow path.

Using a variable before it is initialized must generate a compile-time error message.

If no control flow paths connect the declaration of a variable to its use (ie it is only used after a return statement) then it need not be initialized, but it must still be declared and the use lie in the scope of the declaration.

## Return checking

Every control flow path in the body of a function must end with a return statement (unless the return type is `void`).
In other words, every function must contain a return statement, but not necessarily only one or as the last statement.

# Dynamic semantics

Follow the standard rules of arithmetic.
Statments <lvalue> <asnop> <exp> are executed in the following order:
First, <exp> is executed.
Then, <lvalue> is read (as needed), and the <asnop> operation is computed and stored in <lvalue>.




