# Introduction

*Sesterl* (pronounced as /səsˈtɚːl/) is an ML-like statically-typed functional language that is intended to compile to Erlang. Contrary to its name, Sesterl has not supported session types yet; it only checks the type of messages every process can receive. As mentioned in this book, however, many features as a typed functional language have already been furnished. Among them are the following:

* First-class higher-order functions
* ADTs and pattern matching
* The standard *Damas–Milner polymorphism* (i.e. so-called the *let-polymorphism*) and *Hindley–Milner type inference* \[Hindley 1969\]\[Milner 1978\]
* Type-level distinction between pure calculations and concurrent computations by a kind of monads \[Fowler 2019\]
* A module system equipped with functors and first-class modules based on *F-ing modules* \[Rossberg, Russo & Dreyer 2014\]
