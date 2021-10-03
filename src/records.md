## Records

A *record* is a labeled tuple that has the following syntax:

```
{foo = 42, bar = true}
```

Labels should be distinct from each other in one record value. The expression above has the following type:

```
{foo : int, bar : bool}
```

You can also extract values from records as follows:

```
let r = {foo = 42, bar = true} in
r.foo  /* => 42 */
```

In Sesterl, operations for records are made polymorphic by using the type system for extensible rows \[Gaster & Jones 1996\]. For example, consider the function definition below:

```
val get_foo(x) = x.foo
```

The function `get_foo` is typed like the following:

```
val get_foo<$a, ?$r :: (foo)> : fun({foo : $a, ?$r}) -> $a
```

Here, `(foo)` is the kind for row variables that does NOT contain the label `foo`, similar to ones used for optional parameters. Thanks to the constraint expressed by the kind, `{foo : $a, ?$r}` can be instantiated by `{foo : int, bar : bool}`, `{foo : int, baz : binary}`, and so on, but not by `{bar : bool}` etc.  Then, for instance, the following program is well-typed:

```
val main() =
  get_foo({foo = 42, bar = true})
```

and the following is ill-typed on the other hand:

```
val main() =
  get_foo({bar = true})
```

Note: Prior to Sesterl 0.2.0, polymorphic typing for records was based on the one used in *SML\#* \[Ohori 1995\].
