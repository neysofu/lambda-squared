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

# Church Boolean values work as ternary operators; e.g.
#   >>> TRUE ('foo') ('bar')
#   'foo'
#   >>> (AND (TRUE) (FALSE)) ('foo') ('bar')
#   'bar'
# See:
# - https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans
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
# See:
# - https://en.wikipedia.org/wiki/Cons#Functional_Implementation
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
ONE = INCR (ZERO)
IS_ZERO = lambda n: n (lambda x: FALSE) (TRUE)
LEQ = lambda x: lambda y: IS_ZERO (MINUS (x) (y))
EQ  = lambda x: lambda y: AND (LEQ (x) (y)) (LEQ (y) (x))

# 'VARIABLE', 'ABSTRACTION', and 'APPLICATION' are enums.
VARIABLE    = CHAR_0   = lambda a: lambda b: lambda c: a
ABSTRACTION = CHAR_1   = lambda a: lambda b: lambda c: b
APPLICATION = CHAR_EOF = lambda a: lambda b: lambda c: c
CHAR_IS_EOF = lambda char: char (FALSE) (FALSE) (TRUE)
BUILD_VARIABLE    = lambda i: TUPLE (VARIABLE) (i)
BUILD_ABSTRACTION = lambda x: TUPLE (ABSTRACTION) (x)
BUILD_APPLICATION = lambda x: lambda y: TUPLE (APPLICATION) (TUPLE (x) (y))

# A fixed-point combinator for 'CHAR_0' and 'CHAR_0'. Useful for entering
# nonrecoverable error states during parsing; it finally returns 'FALSE' on
# EOF.
FALSE_ON_EOF = Y (lambda f: lambda c: CHAR_IS_EOF (c) (FALSE) (f))

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
        (CHAR_IS_EOF (char) (TRUE) (FALSE_ON_EOF))
        (char
            (lambda char: char
                (f (count))
                (f (INCR (count)))
                (FALSE))
            (Y (lambda g:
                lambda char:
                char (f (DECR (count))) (g) (FALSE)))
            (FALSE))
) (ONE)

# Accepts 1s as arguments until a 0 is met, then it finally returns the
# variable index.
VARIABLE_PARSER = Y (
    lambda f:
    lambda count:
    lambda callback:
    lambda char:
    char
        (callback (BUILD_VARIABLE (count)))
        (f (INCR (count)) (callback))
        (NONE)
) (ZERO)

# Accepts 'CHAR_0' and 'CHAR_0' as arguments and finally returns the input
# program in a format that is easily processable to reduce.
# When a variable is met, it returns 'backtrace (expression)'
PARSER = Y (
    lambda f:
    lambda backtrace:
    lambda char:
    char
        (lambda char: char
            (f (lambda x: backtrace (BUILD_ABSTRACTION (x))))
            (f (lambda x: f (lambda y: backtrace (BUILD_APPLICATION (x) (y)))))
            (NONE))
        (VARIABLE_PARSER (backtrace))
        (NONE)
) (ID)

SEARCH_AND_REPLACE_VARIABLE_IN_TERM = Y (
    lambda f:
    lambda i:
    lambda argument:
    lambda term: term (TRUE)
        (lambda _: EQ (i) (term (FALSE)) (argument) (term))
        (lambda _: BUILD_ABSTRACTION (f (INCR (i)) (argument) (term (FALSE))))
        (lambda _: BUILD_APPLICATION
            (f (i) (argument) (term (FALSE) (TRUE)))
            (f (i) (argument) (term (FALSE) (FALSE))))
        (NONE)
) (ZERO)

# It effectively runs a parsed BLC expression. Starting from the outermost
# application, variables are matched with the bound variable and replaced
# accordingly. This reduction strategy is known as 'call by name'.
#
# Readings on lambda reduction:
# - 'Types and Programming Languages', by Thomas Pierce
# - http://www.cs.yale.edu/homes/hudak/CS201S08/lambda.pdf
# - https://en.wikipedia.org/wiki/Reduction_strategy_(lambda_calculus)
TERM_TO_NORMAL_FORM = Y (
    lambda f:
    lambda term: term (TRUE)
        (ID)
        (ID)
        (lambda _: term (FALSE) (TRUE) (TRUE)
            (ID)
            (lambda _: f (SEARCH_AND_REPLACE_VARIABLE_IN_TERM
                (term (FALSE) (FALSE))
                (term (FALSE) (TRUE) (FALSE))))
            (lambda _: f (BUILD_APPLICATION (f (term (FALSE) (TRUE))) (term (FALSE) (FALSE))))
            (term))
        (term)
)

VARIABLE_TO_STRING = Y (
    lambda f:
    lambda i:
    IS_ZERO (i)
        (lambda _: '10')
        (lambda _: '1' + f (DECR (i)))
    (NONE)
)

TERM_TO_STRING = Y (
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

INTERPRETER = Y (
    lambda f:
    lambda check_syntax:
    lambda parser:
    lambda char:
    CHAR_IS_EOF (char)
        (lambda _: check_syntax (char)
            (lambda _: TERM_TO_STRING (TERM_TO_NORMAL_FORM (parser)))
            (lambda _: '<invalid program>')
            (NONE))
        (lambda _: f (check_syntax (char)) (parser (char)))
        (NONE)
) (CHECK_SYNTAX) (PARSER)

# Takes in a stream of bits and outputs a stream of lambda terms.
# 1. Transform 0s and 1s into lambda booleans. This step requires operating on
#    Python strings, something that lambda calculus obviously can't do, so we
#    do it in normal Python.
# 2. Parse lambda booleans into tokens (00, 01, or 1{n}0).
# 3. Run.

class TestFalseOnEOF(unittest.TestCase):

    def test_basic(self):
        self.assertEqual(FALSE, FALSE_ON_EOF (CHAR_EOF))
        self.assertEqual(FALSE, FALSE_ON_EOF (CHAR_0) (CHAR_1) (CHAR_EOF))
        self.assertEqual(FALSE, FALSE_ON_EOF (CHAR_1) (CHAR_EOF))

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
        self.assertEqual('110', TERM_TO_STRING (BUILD_VARIABLE (INCR (ZERO))))

    def test_abstraction_to_string(self):
        self.assertEqual('0010', TERM_TO_STRING (BUILD_ABSTRACTION (BUILD_VARIABLE (ZERO))))

    def test_application_to_string(self):
        self.assertEqual('0110110',
                         TERM_TO_STRING
                         (BUILD_APPLICATION (BUILD_VARIABLE (ZERO)) (BUILD_VARIABLE (INCR (ZERO)))))

class TestSyntax(unittest.TestCase):

    def test_empty_program(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_EOF))

    def test_unbalanced_programs(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_0) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_EOF))

    def test_programs_with_incomplete_tokens(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_1) (CHAR_0) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_1) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_1) (CHAR_0) (CHAR_0) (CHAR_EOF))
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_0) (CHAR_EOF))

    def test_programs_with_wrong_token_order(self):
        self.assertEqual(FALSE, CHECK_SYNTAX (CHAR_1) (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_EOF))

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
        var = PARSER (CHAR_1) (CHAR_0)
        self.assertEqual('10', TERM_TO_STRING (var))
        var = PARSER (CHAR_1) (CHAR_1) (CHAR_0)
        self.assertEqual('110', TERM_TO_STRING (var))

    def test_parse_abstraction(self):
        var = PARSER (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_0)
        self.assertEqual('0010', TERM_TO_STRING (var))
        var = PARSER (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_0)
        self.assertEqual('000010', TERM_TO_STRING (var))
        var = PARSER (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_0)
        self.assertEqual('000000110', TERM_TO_STRING (var))

    def test_parse_application(self):
        var = PARSER (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_1) (CHAR_0)
        self.assertEqual('011010', TERM_TO_STRING (var))
        var = PARSER (CHAR_0) (CHAR_1) (CHAR_0) (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_1) (CHAR_0)
        self.assertEqual('010011010', TERM_TO_STRING (var))
        var = PARSER (CHAR_0) (CHAR_1) (CHAR_0) (CHAR_1) (CHAR_1) (CHAR_1) (CHAR_0) (CHAR_1) (CHAR_0) (CHAR_1) (CHAR_0)
        self.assertEqual('01011101010', TERM_TO_STRING (var))

def run_string(string):
    state = INTERPRETER
    for c in source:
        if c == '0':
            state = state(CHAR_0)
        elif c == '1':
            state = state(CHAR_1)
        else:
            state = lambda _: '<invalid input>'
            break
    return state(CHAR_EOF)

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
            print(run_string(source))
    # A MIT/GNU Scheme tradition :)
    print('Moriturus te salutat.')
