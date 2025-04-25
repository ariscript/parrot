# Parrot

Language name: Parrot, because it's a pirate reference

## Syntax

```bnf
<program> ::= <toplevel> ... <expr>

<toplevel> ::= <def>
             | <check>

<check> ::= check <test>
# or
<check> ::= check: <test> ...+ end

<test> ::= <expr> <test-op> <expr>
		 | <expr> <test-op> <expr> because <expr>

<test-op> ::= is
            | is-not
            | is==
            | is-not==
            | is%(pred)
            | is-not%(pred)
            | satisfies
            | violates
```

## Semantics

*Some* desugaring

```
def foo(bar):
	42 * bar
check:
	foo(13) is 546 because 42 * 13,
	foo(1) is 42 because 42 * 1.
#                              ^ this is like `end` but worse :)
```

$$\\Downarrow$$

```
def foo(bar):
	42 * bar

*assert_equal_because(546, 42 * 13) && *assert_equal(foo(13), 546);
(lambda: *assert_equal_because(42, 42 * 1)))
```

- errors in the main program should prevent further program execution
- the main program should be executed before any check blocks
- each check block should be able to error independently
- ## should we maybe just add exceptions???

Some run-time support

## Based off pyret (TODO)

```arr
var a = 0

fun count():
  block:
    a := a + 1
    a
  end
end

check:
  count() is 1
  a := 0
  count() is count() - 1 because 1 
  a := 0
  count() is count() - 1 because count() - 2
  # evaluation order:
  # 1. left side
  # 2. right side
  # 3. because
  # reporting order:
  # 1. between right and because
  # 2. between left and right 
end
```

## added c funcs

we need to add the following c functions
// invalid bool for `because` if it doesn't exist

- check_is(notion, is_negated, span, left, right, because)

<!-- how do we want to repr still? -->

<!-- we *only* need to provide the **adress** (snake)
      - then we need to keep a copy at the *contents* at the addr

 -->
