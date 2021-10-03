# Libraries

* [`sesterl_stdlib`](https://github.com/gfngfn/sesterl_stdlib)
  - The standard library for Sesterl.
  - Contains modules for manipulating basic values and collections (e.g. `Binary`, `List`).
  - Contains modules for constructing OTP-compliant processes (e.g. `GenServer`, `Supervisor`).
* [`sesterl_testing`](https://github.com/gfngfn/sesterl_testing)
  - A testing library for Sesterl.
  - Uses [*EUnit*](http://erlang.org/doc/apps/eunit/chapter.html).
  - Tests written by this module can be run by `rebar3 sesterl test`.
* [`sesterl_json`](https://github.com/gfngfn/sesterl_json)
  - A JSON-handling library.
  - Has APIs similar to those of Elm’s [`elm/json`](https://package.elm-lang.org/packages/elm/json/latest/).
  - Uses [*jsone*](https://github.com/sile/jsone) internally.
* [`sesterl_cowboy`](https://github.com/gfngfn/sesterl_cowboy)
  - A small wrapper for [*Cowboy*](https://github.com/ninenines/cowboy).
