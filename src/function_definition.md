## Function definition

Top-level (resp. local) functions are defined by `val`-bindings (resp. `let`-expressions):

```
val add(x, y) = x + y

val add_partial(x) =
  let f(y) = x + y in f
```

Unlike ML family, however, in order to realize seemless compilation to top-level function definitions in Erlang, functions have their own arity (i.e. not curried by nature) and thereby have types of the form `fun(τ_1, …, τ_n) -> τ`. The function `add` defined above, for instance, has type `fun(int, int) -> int`, which is **NOT** equivalent to the type of `add_partial`, i.e., `fun(int) -> (fun(int) -> int)`.

By using `fun`-expressions (i.e. *lambda abstractions*), `add_partial` can also be defined as follows:

```
val add_partial(x) =
  fun(y) -> x + y end
```

Incidentally, you do not have to annotate types of arguments or return values; they will be reconstructed by standard *Hindley–Milner type inference*. you can nonetheless add type annotations like the following:

```
val add(x : int, y : int) : int = x + y
```

You can define higher-order functions, of course:

```
val apply(f, x) = f(x)
```

As is the case in ML, `apply` has a polymorphic type. Features related to type polymorphism is explained later.

Recursive or mutually recursive functions can be defined by using `rec`/`and` keywords, not only globally but also in a local scope:

```
val rec fact(n) =
  if n <= 0 then 1 else n * fact(n - 1)

val is_even_nat(n) =
  let rec odd(n) =
    if n == 0 then false else even(n - 1)

  and even(n) =
    if n == 0 then true else odd(n - 1)
  in
  if n < 0 then false else even(n)
```

Note that, unlike Erlang, function names are all lowercased regardless of whether they are defined in the global scope or in a local one. You can also write, for example, `apply(fact, 6)`; each name of globally-defined functions can be used for the function value the name is bound to, just as locally defined function names can be. This is different from the situation in Erlang, where a globally-defined function name by itself will be interpreted as an atom of the same text.
