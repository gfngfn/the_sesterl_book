## Module system

One of the Sesterl’s largest features is the support for a subset of *F-ing modules* \[Rossberg, Russo & Dreyer 2014\], where kinds and functors are restricted to first-order (i.e., type constructors cannot take type constructors as arguments and functors cannot take functors as arguments). For example, Sesterl can type-check the following definition of modules and functors:

```
/* mod.sest */

module Mod = struct

  signature Ord = sig
    type s :: o
    val compare : fun(s, s) -> int
  end

  module Map = fun(Elem : Ord) ->
    struct
      type elem = Elem.s
      type t<$a> = list<{elem, $a}>
      val rec find<$b>(x : elem, assoc : t<$b>) : option<$b> =
        case assoc of
        | [] ->
            None

        | {k, v} :: tail ->
            if Elem.compare(k, x) == 0 then
              Some(v)
            else
              find(x, tail)
        end
    end

  module Int = struct
    type s = int
    val compare(x : int, y : int) = y - x
  end

  module IntMap = Map(Int)

end
```

The program above is compiled to the following Erlang modules (where line breaks and indentation are manually added for clarity):

```erlang
-module('Mod.Int').
-export([compare/2]).

compare(S13X, S14Y) -> (S14Y - S13X).
```

```erlang
-module('Mod.IntMap').
-export([find/2]).

find(S17X, S18Assoc) ->
  case S18Assoc of
    [] ->
      error;

    [{S19K, S20V} | S21Tail] ->
      case ('Mod.Int':compare(S19K, S17X) == 0) of
        true  -> {ok, S20V};
        false -> 'Mod.IntMap':find(S17X, S21Tail)
      end
  end.
```

Note that nested modules are flattened and given names of the form `'<M_1>.<M_2>. ... .<M_n>'` where each `<M_i>` is a module identifier.

What is more important here is that functors are eliminated *at compilation time*. This is realized by the technique of so-called the *static interpretation* \[Elsman, Henriksen, Annenkov & Oancea 2018\].
