## Polymorphism

Values defined by `val`, `val rec`, `let`, or `let rec` can be polymorphic. For instance, the function `proj1` defined as follows has type `<$a, $b> fun($a, $b) -> $a` (where `<$a, $b>` stands for universal quantification):

```
val proj1(x, y) = x
```

Instead of relying upon type inference, you can also annotate polymorphic types and check that the defined function is indeed polymorphic:

```
val proj1<$a, $b>(x : $a, y : $b) : $a = x
```
