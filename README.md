# Theorem Prover

A propositional logic verifier, now with linear equations!

I wrote a [blog post](https://stowell.dev/posts/2024-12-16-linear-systems-with-negated-equations/) if you want to learn more details.

## Why?

I started reading a book on [set theory and abstract algrebra](https://openlibrary.org/books/OL5069466M/Set_theory_and_abstract_algebra),
and got really inspired to make some kind of theorem prover to verify some expressions for me.

## Status

Currently, this project is able to prove if a set of linear equations arranged in a logical expression, 
such as `(a = b) & (b = a)`, is always true (proven), conditionally true (satisfiable), or never true.

It accomplishes this by combining a [SAT Solver](https://en.wikipedia.org/wiki/SAT_solver) with some fun matrix math.

If you're curious about how I went about this, please check out the aforementioned [blog post](https://stowell.dev/posts/2024-12-16-linear-systems-with-negated-equations/).

- [x] Language description and parsing
- [x] Verifies all expressions
- [x] Conditional predicates (predicates who's truth value is constrained by some condition)
- [ ] Sets
- [ ] Predefined types (i.e. ℕ, ℤ, ℚ, ℝ)
  
