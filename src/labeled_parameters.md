## Labeled parameters

### Labeled optional parameters

Functions can have labeled optional parameters:

```
val succ(n : int, ?diff dopt : option<int>) =
  case dopt of
  | None    -> n + 1
  | Some(d) -> n + d
  end

val f(g) =
  {g(36), g(36, ?diff 64)}

val main() =
  {succ(42), succ(42, ?diff 15), f(succ)}
    /* This evaluates to {43, 57, {37, 100}} in Erlang. */
```

In this example, `?diff` is a label for an optional parameter. By not giving a `?diff`-labeled argument you can use `succ` as the standard successor function, while by giving one you can use `succ` as the integer addition function.

The functions `succ` and `f` defined above are given types as follows:

```
val succ : fun(int, ?diff int) -> int
val f<$a, ?$r :: (diff)> : fun(fun(int, ?diff int, ?$r) -> $a) -> ($a, $a)
```

Here, `?diff int` signifies that `succ` can take a `?diff`-labeled optional argument of type `int`, and the absense of other labels in the same domain means that `succ` cannot take optional arguments with labels other than `?diff`.

`?$r :: (diff)` is a *row variable* with its kind; it can be instantiated with any rows that do NOT contain the label `diff`; kinds for row variables stand for the prohibited set of labels. This is based on an original type system that resembles record polymorphism \[Gaster & Jones 1996\] (The type system is currently not documented anywhere).


### Labeled mandatory parameters

You can also use labeled mandatory parameters/arguments:

```
val rec foldl(-f f, -init init, -list xs) =
  case xs of
  | []      -> init
  | y :: ys -> foldl(-init f(init, y), -list ys, -f f)
  end
```

Here, `-f`, `-init`, and `-list` are labels for mandatory parameters. Note the order in which labeled arguments are applied is not necessarily the same as that in which labeled parameters are defined. The function `foldl` defined above is typed as follows:

```
val fold<$a, $b> :
  fun(
    -f    fun($a, $b) -> $a,
    -init $a,
    -list list<$b>,
  ) -> $a
```

You can use non-labeled parameters (resp. arguments) and labeled ones for the same function. At least currently, however, their order must be:

1. (possibly empty) non-labeled parameters (resp. arguments),
2. (possibly empty) labeled mandatory ones, and
3. (possibly empty) labeled optional ones.

In other words, abstractions (resp. applications) must be of the following form:

```
fun(param1, …, paramL, -m1 mparam1, … -mM mparamM, ?o1 oparam1, … ?oN oparamN) -> …

f(arg1, …, argL, -m1 marg1, … -mM margM, ?o1 oarg1, … ?oN oargN)
```
