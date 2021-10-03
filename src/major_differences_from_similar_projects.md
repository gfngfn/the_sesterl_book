# Major differences from similar projects

There have been brilliant functional languages that compile to Erlang or BEAM (i.e. bytecode for Erlang VM). Some of them are the following:

* [*Elixir*](https://elixir-lang.org/) \[Valim et al. 2011–2021\]
  - Definitely the most well-known AltErlang language, and well-used in productions.
  - Compiles to Erlang AST.
  - Untyped (i.e. dynamically typed).
  - Has Ruby-like syntax.
  - Supports Lisp-like meta-programming features by quoting/unquoting.
* [*Alpaca*](https://github.com/alpaca-lang/alpaca) \[Pierre et al. 2016–2019\]
  - Statically typed.
  - Compiles to Core Erlang compiler IR.
  - Has static guarantee about types of messages sent or received between processes.
  - Has OCaml- or Elm-like syntax.
  - Implemented in Erlang.
* [*Gleam*](https://github.com/gleam-lang/gleam) \[Pilfold et al. 2018–2021\]
  - Statically typed.
  - Compiles to sources in Erlang.
  - Has Rust-like syntax.
  - Implemented in Rust.

Major differences between the features of Sesterl and those of the languages above are:

* an ML-like module system that supports:
  - abstraction by using signatures, and
  - functors and their elimination at compilation time (called the *static interpretation* \[Elsman, Henriksen, Annenkov & Oancea 2018\]);
* a kind of monadic types for distinguishing pure calculations from concurrent computations.

Also, though not supporting them currently, we want to add features like the following (see “[Future work](#future-work)” for detail):

* GADTs for typing synchronous message-passing operations more strictly.
* Session types in a gradually-typed manner.


## Future work

* Support GADTs.
  - This is mainly for typing `gen_server` callbacks as to synchronous messages.
  - The formalization of such a type system and a type inference algorithm will probably be based on *choice types* \[Chen & Erwig 2016\].
* Support (multiparty) session types.
  - Type checking based on session types may well be optional or something like gradual types. This is because message passing is quite frequent in typical uses of Erlang-style concurrency and thereby strict assertion for sessions may rather complicate in the short term how to program concurrent computations.
