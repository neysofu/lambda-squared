#!/usr/bin/env python3

# This Python script declares a binary lambda calculus interpreter. The
# interpreter is written in untyped lambda calculus itself (i.e. only function
# declarations and applications are allowed). Native recursion in particular
# is disallowed.

# All functions allow prefix notation; for example:
#   NOT (TRUE)
#   _1ST (tuple)
#   EQ (n) (m)

# See https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
# for more.
# It lets you define recursive functions by passing a dummy function definition
# as an argument. E.g.
#   >>> FACTORIAL = lambda f: lambda n: 1 if n == 0 else n * f(n-1)
#   >>> Y (FACTORIAL) (4)
#   24
Y = lambda f: ((lambda x: f(lambda y: x(x)(y)))(lambda x: f(lambda y: x(x)(y))))
ID = NONE = lambda x: x

# Boolean values as ternary operators. E.g.
#   >>> TRUE ('foo') ('bar')
#   'foo'
#   >>> (AND (TRUE) (FALSE)) ('foo') ('bar')
#   'bar'
TRUE = lambda p: lambda q: p
FALSE = lambda p: lambda q: q
NOT = lambda p: p(FALSE)(TRUE)
AND = lambda p: lambda q: p(q)(FALSE)
OR = lambda p: lambda q: p(TRUE)(q)

# This tuple holds any two elements and provides access via postfix boolean
# values. E.g.
#   >>> TUPLE ('foo') ('bar') (TRUE)
#   'foo'
#   >>> TUPLE ('foo') ('bar') (FALSE)
#   'bar'
# You can view it as a ternary operator with postfix notation.
TUPLE = lambda x: lambda y: lambda f: f(x)(y)

# Church numerals. These will be useful for defining variables.
#
# See:
# - https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals

INCR = lambda a: lambda p: lambda q: p(n(p)(q))
DECR = (
    lambda a: lambda p: lambda q: (a)(lambda r: lambda s: s(r(p)))(lambda u: q)(ID)
)

PLUS = lambda a: lambda b: a(INCR)(b)
MINUS = lambda a: lambda b: a(DECR)(b)

ZERO = FALSE
ONE = INCR(ZERO)

IS_ZERO = lambda a: a(lambda p: FALSE)(TRUE)
LESS_OR_EQUAL = lambda m: lambda n: IS_ZERO(MINUS(m)(n))
EQUAL = lambda a: lambda b: ((AND)(LESS_OR_EQUAL(a)(b))(LESS_OR_EQUAL(b)(a)))

# Lambda terms
# ============
# Our code will be a function that takes boolean inputs and then outputs a lazy
# evaluation encoded using booleans as well.

VARIABLE = lambda a: lambda b: lambda c: a
ABSTRACTION = lambda a: lambda b: lambda c: b
APPLICATION = lambda a: lambda b: lambda c: c

CHAR_0 = lambda a: lambda b: lambda c: a
CHAR_1 = lambda a: lambda b: lambda c: b
CHAR_EOF = lambda a: lambda b: lambda c: c
CHAR_IS_EOF = lambda char: char(FALSE)(FALSE)(TRUE)

DE_BRUIJN_INDEX_PARSER = (
    lambda f: lambda current_index: lambda char: char(f(INCR(current_index)))(current_index)(NONE)
)

# Takes in a stream of boolean values and outputs an expression
PARSER = (lambda f:
          lambda char:
          char
          (TUPLE (VARIABLE) (Y (DE_BRUIJN_INDEX_PARSER) (ZERO)))
          (lambda next_char:
           next_char
           (TUPLE (APPLICATION) (TUPLE (f) (f)))
           (TUPLE (ABSTRACTION) (f))
           (NONE))
          (NONE))

# λ-calculus call-by-name reduction
# =================================

CALL_BY_NAME = (lambda f:
                lambda expression:
                lambda term:
                # Check expression type
                (expression (TRUE))
                (lambda variable: variable)
                (lambda abstraction:
                 (TUPLE (ABSTRACTION))
                 # We increment the counter 'n' and keep searching for terms bound to 'x'.
                 (f (expression (FALSE)) (term) (INCR (n))))
                (lambda application:
                 (TUPLE
                  (APPLICATION)
                  # We reduce both terms and then the second is applied to the first.
                  (f (_1ST (expression (FALSE))) (x) (n))
                  # The dummy argument g=ID is just to avoid strict evaluation.
                  (f (_2ND (_2ND (e))) (x) (n)))))

# It receives a null-terminated stream of characters and finally returns 'TRUE'
# if it's a valid BLC program.
SYNTAX_CHECK = (lambda f:
                lambda count:
                lambda char:
                (lambda next_char:
                 (f (INCR (INCR (count))))
                 (f (INCR (count)))
                ( DE_BRUIJN_INDEX_PARSER )
                (IS_ZERO (count))))

# Reads characters until EOF and returns its internal state. Then prints its response.
INTERPRETER_STEP = (lambda f:
                    lambda syntax_check:
                    lambda parser:
                    lambda char:
                    CHAR_IS_EOF
                    (f
                     (syntax_check (char))
                     (parser (char)))
                    (syntax_check (char)
                     (RUN_BLC (parser (char)))
                     ((lambda _: print("<invalid program>") (NONE)))))

INTERPRETER = Y(INTERPRETER_STEP)(SYNTAX_CHECK(ZERO))(PARSER)

# Takes in a stream of bits and outputs a stream of lambda terms.
# 1. Transform 0s and 1s into lambda booleans. This step requires operating on
#    Python strings, something that lambda calculus obviously can't do, so we
#    do it in normal Python.
# 2. Parse lambda booleans into tokens (00, 01, or 1{n}0).
# 3. Run.

if __name__ == "__main__":
    print("A binary lambda calculus interpreter by Filippo Costa")
    print("Abstraction is 00")
    print("Application is 01")
    print("Variables are 1s followed by 0s")
    while True:
        source = input("> Type in your program: ")
        if not source:
            continue
        elif source == "exit":
            break
        state = Y(INTERPRETER)
        for c in source:
            state = state(CHAR_1 if c == '1' else CHAR_0)
        print("Normal form: ")
        state = state(CHAR_EOF)
    print("-- Moriturus te saluto.")
