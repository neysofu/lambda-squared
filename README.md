# λ²
λ² is a λ-calculus implementation written only in Python lambdas.

- No recursion.
- No Python built-ins (integers, strings, conditionals... nothing!).
- No external dependencies. (Duh.)

### What is this?

From [the Wikipedia entry](https://en.wikipedia.org/wiki/Lambda_calculus) on λ-calculus:

> Lambda calculus (also written as λ-calculus) is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution. It is a universal model of computation that can be used to simulate any single-taped Turing machine and was first introduced by mathematician Alonzo Church in the 1930s as part of his research of the foundations of mathematics.

Binary λ-calculus is just a convenient encoding for already valid λ-calculus programs. `00` is abstraction and `01` is application. Instead of variables, binary λ-calculus uses [De Bruijn indexes](https://en.wikipedia.org/wiki/De_Bruijn_index).

### Why?
Why not?
