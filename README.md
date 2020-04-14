# AmberC

AmberC is an experimental C compiler that emits x86-64 machine code.
It is still at a very early stage. I'm currently working on the parser.

- Expressions
    - [x] identifier, integer, FP and string constant, and parenthesized-expression.
    - [x] func-call and array access.
    - [x] member-expression.
    - [x] postfix-increment/decrement.
    - [x] unary-expressions.
    - [x] cast-expressions.
    - [x] multiplicative- and additive-expressions.
    - [x] shift-expressions.
    - [x] relational-expressions.
    - [x] equality-expressions.
    - [x] bitwise-expressions(&, ^, |).
    - [x] logical-expressions(&&, ||).
    - [x] conditional-expressions(cond ? expr1 : expr2).
    - [x] assignments.
    - [x] initializers.

- Types
    - [x] qual/unqual simple types.
    - [x] non-def struct or union types.
    - [ ] def struct or union types.

    