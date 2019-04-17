#!/usr/bin/env python3

import unittest

# This Python script declares a binary lambda calculus interpreter. The
# interpreter is written in untyped lambda calculus itself (i.e. only function
# declarations and applications are allowed). Native recursion in particular
# is disallowed.

# See https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
# for more.
# It lets you define recursive functions by passing a dummy function definition
# as an argument. E.g.
#   >>> FACTORIAL = lambda f: lambda n: 1 if n == 0 else n * f(n-1)
#   >>> Y (FACTORIAL) (4)
#   24
# You are not expected to understand this. To see how deep the rabbit hole
# goes, start by learning the much simpler call-by-value version:
# https://hisham.hm/2011/04/04/understanding-at-last-the-y-combinator-a-programmer-friendly-perspective/
Y = lambda f: (
    (lambda x: f (lambda y: x (x) (y)))
    (lambda x: f (lambda y: x (x) (y)))
)

ID = NONE = lambda x: x

# Boolean values as ternary operators. E.g.
#   >>> TRUE ('foo') ('bar')
#   'foo'
#   >>> (AND (TRUE) (FALSE)) ('foo') ('bar')
#   'bar'
TRUE  = lambda p: lambda q: p
FALSE = lambda p: lambda q: q
NOT = lambda p: p (FALSE) (TRUE)
AND = lambda p: lambda q: p (q) (FALSE)
OR  = lambda p: lambda q: p (TRUE) (q)

# This tuple holds any two elements and provides access via postfix boolean
# values. E.g.
#   >>> TUPLE ('foo') ('bar') (TRUE)
#   'foo'
#   >>> TUPLE ('foo') ('bar') (FALSE)
#   'bar'
# You can view it as a ternary operator with postfix notation.
TUPLE = lambda x: lambda y: lambda f: f (x) (y)

# Church numerals. These will be useful for defining variables.
#
# See:
# - https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals

INCR = lambda n: lambda f: lambda x: f (n (f) (x))
DECR = (
    lambda n:
    lambda f:
    lambda x:
    n
        (lambda g: lambda h: h (g (f)))
        (lambda u: x)
        (ID)
)

MINUS = lambda m: lambda n: n (DECR) (m)
ZERO = FALSE
IS_ZERO = lambda n: n (lambda x: FALSE) (TRUE)
LEQ = lambda x: lambda y: IS_ZERO (MINUS (x) (y))
EQ  = lambda x: lambda y: AND (LEQ (x) (y)) (LEQ (y) (x))

# Enumeration types.
VARIABLE    = CHAR_0   = lambda a: lambda b: lambda c: a
ABSTRACTION = CHAR_1   = lambda a: lambda b: lambda c: b
APPLICATION = CHAR_EOF = lambda a: lambda b: lambda c: c
CHAR_IS_EOF = lambda char: char (FALSE) (FALSE) (TRUE)

BUILD_VARIABLE    = lambda i: TUPLE (VARIABLE) (i)
BUILD_ABSTRACTION = lambda x: TUPLE (ABSTRACTION) (x)
BUILD_APPLICATION = lambda x: lambda y: TUPLE (APPLICATION) (TUPLE (x) (y))

# It takes in a stream of characters until EOF is reached, then returns 'TRUE'
# if the input was a valid BLC program, 'FALSE' otherwise. Empty programs are
# not valid!
# - 'count' starts at 1.
# - Variables decrement by 1.
# - Abstractions leave it unchanged.
# - Applications increment it by 1.
# - If EOF, 'count' must be 0. 
# - If 0, next must be EOF.
CHECK_SYNTAX = Y (
    lambda f:
    lambda count:
    lambda char:
    IS_ZERO (count)
        (CHAR_IS_EOF (char))
        (char
            (lambda char: char
                (f (count))
                (f (INCR (count)))
                (FALSE))
            (Y (lambda g:
                lambda char:
                char (f (DECR (count))) (g) (FALSE)))
            (FALSE))
) (INCR (ZERO))

# Accepts 1s as arguments until a 0 is met, then it finally returns the
# variable index.
DE_BRUIJN_INDEX_PARSER = Y (
    lambda f:
    lambda current_index:
    lambda char:
    char
        (current_index)
        (f (INCR (current_index)))
        (NONE)
) (ZERO)

# Takes in a stream of characters and builds an internal representation of the
# program that allows easy evaluation. Finally, when it reads an EOF character,
# it returns the final representation, ready to be feed into 'REDUCTION'.
PARSER = Y (
    lambda f:
    lambda char:
    char
        (lambda char: char
            (TUPLE (ABSTRACTION) (f))
            (TUPLE (APPLICATION) (TUPLE (f) (f)))
            (NONE))
        (lambda char: TUPLE (VARIABLE) (DE_BRUIJN_INDEX_PARSER (char)))
        (NONE)
)

# It effectively runs a parsed BLC expression. Starting from the outermost
# application, variables are matched with the bound variable and replaced
# accordingly. This reduction strategy is known as 'call by name'.
#
# Readings on lambda reduction:
# - 'Types and Programming Languages', by Thomas Pierce
# - http://www.cs.yale.edu/homes/hudak/CS201S08/lambda.pdf
# - https://en.wikipedia.org/wiki/Reduction_strategy_(lambda_calculus)
REDUCTION = (
    lambda f:
    lambda expression:
    lambda term:
    expression
        (TRUE)
        (lambda variable: variable)
        (lambda abstraction: TUPLE
            (ABSTRACTION)
            # We increment the counter 'n' and keep searching for terms bound to 'x'.
            (f (expression (FALSE)) (term) (INCR (n))))
        (lambda application: TUPLE
            (APPLICATION)
            # We reduce both terms and then the second is applied to the first.
            (f (_1ST (expression (FALSE))) (x) (n))
            # The dummy argument g=ID is just to avoid strict evaluation.
            (f (_2ND (_2ND (e))) (x) (n)))
)

VARIABLE_TO_STRING = Y (
    lambda f:
    lambda i:
    IS_ZERO (i)
        (lambda _: '10')
        (lambda _: '1' + f (DECR (i)))
    (NONE)
)

EXPRESSION_TO_STRING = Y (
    lambda f:
    lambda expression:
    # Switch-case the expression type
    expression (TRUE)
        # Variable
        (lambda _: VARIABLE_TO_STRING (expression (FALSE)))
        # Abstraction
        (lambda _: '00' + f (expression (FALSE)))
        # Application
        (lambda _: '01' + f (expression (FALSE) (TRUE)) + f (expression (FALSE) (FALSE)))
        (NONE)
)

# Reads characters until EOF and returns its internal state. Then prints its response.

INTERPRETER_STEP = (
    lambda f:
    lambda syntax_check:
    lambda parser:
    lambda char: CHAR_IS_EOF
        (f (syntax_check (char)) (parser (char)))
        (syntax_check
            (char)
            (RUN_BLC (parser (char)))
            ((lambda _: print('<invalid program>') (NONE))))
)

INTERPRETER = Y (INTERPRETER_STEP) (CHECK_SYNTAX) (PARSER)

# Takes in a stream of bits and outputs a stream of lambda terms.
# 1. Transform 0s and 1s into lambda booleans. This step requires operating on
#    Python strings, something that lambda calculus obviously can't do, so we
#    do it in normal Python.
# 2. Parse lambda booleans into tokens (00, 01, or 1{n}0).
# 3. Run.

class TestChurchNumerals(unittest.TestCase):

    def test_is_zero(self):
        self.assertEqual(IS_ZERO (ZERO), TRUE)
        self.assertEqual(EQ (ZERO) (ZERO), TRUE)

    def test_less_than_or_equal(self):
        self.assertEqual(LEQ (ZERO) (INCR (ZERO)), TRUE)
        self.assertEqual(LEQ (INCR (ZERO)) (ZERO), FALSE)

    def test_incr_then_decr(self):
        self.assertEqual(IS_ZERO (DECR (INCR (ZERO))), TRUE)

class TestStringify(unittest.TestCase):

    def test_variable_to_string(self):
        self.assertEqual('10', VARIABLE_TO_STRING (ZERO))
        self.assertEqual('110', EXPRESSION_TO_STRING (BUILD_VARIABLE (INCR (ZERO))))

    def test_abstraction_to_string(self):
        self.assertEqual('0010', EXPRESSION_TO_STRING (BUILD_ABSTRACTION (BUILD_VARIABLE (ZERO))))

    def test_application_to_string(self):
        self.assertEqual('0110110',
                         EXPRESSION_TO_STRING
                         (BUILD_APPLICATION (BUILD_VARIABLE (ZERO)) (BUILD_VARIABLE (INCR (ZERO)))))

class TestSyntax(unittest.TestCase):

    def test_empty_program(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_EOF))

    def test_invalid_programs(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_0) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_1) (CHAR_EOF))

    def test_valid_programs(self):
        self.assertEqual(TRUE, CHECK_SYNTAX (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_0) (CHAR_EOF))
        self.assertEqual(TRUE, CHECK_SYNTAX (CHAR_1) (CHAR_0) (CHAR_EOF))
        self.assertEqual(TRUE, CHECK_SYNTAX (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_EOF))

    def test_variable(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_1) (CHAR_EOF))
        self.assertEqual(TRUE, CHECK_SYNTAX (CHAR_1) (CHAR_0) (CHAR_EOF))
        self.assertEqual(TRUE, CHECK_SYNTAX (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_EOF))

class TestParser(unittest.TestCase):

    def test_parse_variable(self):
        var = PARSER (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_0)
        self.assertEqual('0010', EXPRESSION_TO_STRING (var))
        #var = PARSER (CHAR_1) (CHAR_0)
        #self.assertEqual('10', EXPRESSION_TO_STRING (var))
        #var = PARSER (CHAR_1) (CHAR_1) (CHAR_0)
        #self.assertEqual('110', EXPRESSION_TO_STRING (var))

if __name__ == '__main__':
    print('A binary lambda calculus interpreter by Filippo Costa')
    print('Abstraction is 00')
    print('Application is 01')
    print('Variables are 1s followed by 0s')
    while True:
        source = input('> Type in your program: ')
        if not source:
            continue
        elif source == 'test':
            unittest.main()
        elif source == 'exit':
            break
        elif source.split(' ', 1)[0] == 'eval':
            eval(source.split(' ', 1)[1])
            continue
        else:
            state = INTERPRETER
            for c in source:
                state = state(CHAR_1 if c == '1' else CHAR_0)
            print('Normal form: ')
            state = state(CHAR_EOF)
    # A MIT/GNU Scheme tradition :)
    print('Moriturus te salutat.')
