#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This Python script declares a binary lambda calculus interpreter. The
# interpreter is written in untyped lambda calculus itself (i.e. only function
# declarations and applications are allowed). Native recursion in particular
# is disallowed.

# All functions allow prefix notation; for example:
#   NOT (TRUE)
#   _1ST (tuple)
#   EQ (n) (m)

# Utils
# =====

# Implements recursion; for example:
#   FACTORIAL = Y (lambda f: lambda n: 1 if n == 0 else n * f(n-1))
# See https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
# for more.
Y = lambda f: (
    (lambda x: f (lambda y: x (x) (y)))
    (lambda x: f (lambda y: x (x) (y))))

ID = lambda x: x

# Booleans
# ========

TRUE = lambda x: lambda y: x
FALSE = lambda x: lambda y: y

NOT = lambda p: p (FALSE) (TRUE)
AND = lambda p: lambda q: p (q) (FALSE)
OR = lambda p: lambda q: p (TRUE) (q)
XOR = lambda p: lambda q: p (NOT (q)) (q)

# Tuples
# ======

# Instantiate a 2-tuple by feeding 'TUPLE' with two elements. 'f' then allows
# to retrieve the elements without 'b' applying to 'a'.
TUPLE = lambda a: lambda b: lambda f: f (a) (b)
_1ST = lambda t: t (FALSE)
_2ND = lambda t: t (TRUE)

# Integers
# ========
# See https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals

# SUCC(n) = n+1
SUCC = lambda n: lambda f: lambda x: f (n (f) (x))
# PREC(n) = n-1
PREC = lambda n: lambda f: lambda x: (n
    (lambda g: lambda h: h (g (f)))
    (lambda u: x)
    (ID))

SUB = lambda m: lambda n: n (PREC) (m)

ZERO = FALSE
ONE = SUCC (ZERO)
TWO = SUCC (ONE)
THREE = SUCC (TWO)

# IS_ZERO(n) = n == 0
IS_ZERO = lambda n: n (lambda x: FALSE) (TRUE)
# LEQ(n,m) = n <= m
LEQ = lambda m: lambda n: IS_ZERO (SUB (m) (n))
# EQ(n,m) = n == m
EQ = lambda m: lambda n: AND (LEQ (m) (n)) (LEQ (n) (m))

# Lambda terms
# ============

ENUM_3_1 = lambda a: lambda b: lambda c: a
ENUM_3_2 = lambda a: lambda b: lambda c: b
ENUM_3_3 = lambda a: lambda b: lambda b: c

LAMBDA_VARIABLE_BUILDER = lambda n: TUPLE (ENUM_3_1) (n)
LAMBDA_ABSTRACTION_BUILDER = lambda t: TUPLE (ENUM_3_2) (t)
LAMBDA_APPLICATION_BUILDER = lambda x: lambda y: TUPLE (ENUM_3_3) (TUPLE (x) (y))

# λ-calculus call-by-name reduction
# =================================

# The arguments are:
#   f   a self-reference dummy function for the Y combinator.
#   e   the lambda term to reduce.
#   x   the lambda term which the current expression is bound to.
#   n   the current De Bruijn index.
# Illegal lambda terms will cause undefined behavior.
REDUCE = Y (lambda f: lambda e: lambda x: lambda n: _1ST (e)
    # If the variable is bound to 'x', then we replace; if not, we leave it
    # there.
    (lambda g: LAMBDA_VARIABLE_BUILDER
        (EQ (n) (_2ND (e))
            (x)
            (_2ND (e)))
    # We increment the counter 'n' and keep searching for terms bound to 'x'.
    (lambda g: LAMBDA_ABSTRACTION_BUILDER
        (f (_2ND (e)) (x) (SUCC (n))))
    (lambda g: LAMBDA_APPLICATION_BUILDER
        # We reduce both terms and then the second is applied to the first.
        (f (_1ST (_2ND (e))) (x) (n))
        # The dummy argument g=ID is just to avoid strict evaluation.
        (f (_2ND (_2ND (e))) (x) (n)))) (ID))

APPLY = Y (lambda f: lambda e: _1ST (e)
    # We only reduce applications. (This is the 'call-by-name reduction
    # strategy'.)
    (LAMBDA_VARIABLE_BUILDER (_2ND (e)))
    (LAMBDA_ABSTRACTION_BUILDER (_2ND (e)))
    (REDUCE (_1ST (e)) (_2ND (e)) (ONE)))

# Binary λ-calculus parser and interpreter
# ========================================
"""
PARSE_LAMBDA = Y (lambda f: lambda b: b (
    # If p == 1, we pass control over to PARSER_DE_BRUIJN_INDEX.
    (PARSE_DE_BRUIJN_INDEX (ONE))
    # Otherwise, we distinguish between abstraction (00) and application (01).
    (lambda b1: b1
        # 01
        (lambda b2:)
        # 00
    )
))

# Parse a de Bruijn index, for example:
#   1111110 => $6
#   10 => $1
#
# In general,
#   1{n}0 => $n
PARSE_DE_BRUIJN_INDEX = Y (lambda f: lambda n: lambda b: b
    (lambda b1: f (SUCC (n)) (b1))
    (LAMBDA_VARIABLE_BUILDER (n)))

PARSE_ABSTRACTION = (
    (lambda b: LAMBDA_ABSTRACTION_BUILDER () ))

PARSE_APPLICATION = LAMBDA_APPLICATION_BUILDER (PARSE_LAMBDA)
"""
