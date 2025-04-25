# Parrot

**P**yrets **A**re ve**R**ifying co**R**rectness thr**O**ugh **T**esting

Ari Prakash and Owen Duckham

![logo](img/logo.png)

## Concrete Syntax

Syntax built on top of Fer-de-lance/Garter/Racer:

```bnf
<decl> ::= ...
         | <check> <decls>

<check> ::= check: <check_statements>.

<check_statements> ::= <check_statement>
                     | <check_statement>, <check_statements>

<check_statement> ::= <val> <bind> = <expr>
                    | <expr> <test_op> <expr>
                    | <expr> <test_op> <expr> because <expr>

<val> ::= val | example

<test_op> ::= is
            | isn't
            | is==
            | isn't==
            | is%(<expr>)
            | isn't%(<expr>)
            | satisfies
            | violates
```

## Design

Check blocks are a `decl` and can be written at the top level of the program,
_before_ the main body expression. They can contain either tests or example
declarations.

## Semantics

Check blocks require a different context to run the program, since they might
error, and we don't want this to completely exit the program (and only exit the
current block). We also want test failures to report with a good error message
to the user, including the source location of the test that failed.

> We wanted to load the source code itself into the executable and print out
> the actual code, but did not have time for it.

## Implementation

### Desugaring

Most of the functionality here is done via desugaring. We added a new phase to
specifically desguar `check` blocks because they need to be done while we have
`sourcespan` information still available. `tag`ging the AST removes this, so it
couldn't be done during the desugaring pass that we already had. We desugar
blocks into a `Check` type IIFE, where declared values get turned into `let`s
and individual tests are transformed into specific desugarings based on what
they actually test.

### `Check` Call Type

We introduce a new call type to accomodate `check` blocks that works like the
`Snake` call type but with the following differences:

1. At the beginning, Call the C function
   `start_check_block(Reg RBP, Reg RSP, end_label)`.
2. At the end, insert `end_label`, and then call the C function
   `end_check_block()`.
3. Clobber whatever might have been in `RAX` with `nil`.

This is the extent of the codegen changes, everything else is done via
desugaring.

### Test Type Tag

Each test calls the `test` function in C, whose first argument represents the
kind of test being run. This is represented as a snake-encoded integer whose
actual value looks like the following:

| `because` flag | Negative flag | Base Type |
| -------------- | ------------- | --------- |
| 1 bit          | 1 bit         | 2 bits    |

| Bit Pattern | Base Type   |
| ----------- | ----------- |
| `00`        | `equal`     |
| `01`        | `==`        |
| `10`        | `%`         |
| `11`        | `satisfies` |

These tags are used to determine the way to check for equality (or not
_actually_ check for equality in case of `%` and `satisfies`) and to report
error messages in the appropriate terms for the user.

### Error Handling

The `error` function calls `die` instead of `exit`. The `die` function checks
if it was being run within the dynamic extent of a `check` block, and returns
control to the Parrot program appropriately. It restores `rbp` and `rsp` to the
values that they were before the call to `start_check_block`, and then `jmp`s
to the end of the check block.
