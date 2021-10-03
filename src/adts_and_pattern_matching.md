## ADTs and pattern matching

### ADTs

You can define (non-generalized) algebraic data types and type synonyms in a standard way like the following:

```
type name = binary

type with_number<$a> = {$a, int}

type bintree<$b> =
  | Node($b, bintree<$b>, bintree<$b>)
  | Empty
```

Here, `{$a, int}` is an example use of standard product types.

As can be seen from the example above, type names start with a lowercase letter, constructors do with an uppercase one, and type variables are denoted by using a preceding `$`.

Each application of a constructor `Ctor(e_1, …, e_n)` will be compiled to a tuple `{ctor, e_1, …, e_n}` in Erlang where `ctor` is basically a lowercased atom corresponding to `Ctor`. You can, however, change what atoms are generated for constructors by using `#[atom(...)]` attribute:

```
type bintree<$b>
  | #[atom("branch")] Node($b, bintree<$b>, bintree<$b>)
  | #[atom("leaf")]   Empty
```

List-generating constructors, `[]` (nil) and `::` (cons), are also supported by default. Optionals are also provided by default as follows:

```
type option<$a> =
  | #[atom("error")] None
  | #[atom("ok")]    Some($a)
```

### Pattern matching

You can decompose values of ADTs by using `case`-expressions in an ordinary way like the following:

```
val reverse<$a>(xs : list<$a>) : list<$a> =
  let rec aux(acc, xs) =
    case xs of
    | []        -> acc
    | x :: tail -> aux(x :: acc, tail)
    end
  in
  aux([], xs)

val rec tree_size<$a>(t : bintree<$a>) =
  case t of
  | Empty           -> 0
  | Node(_, t1, t2) -> 1 + tree_size(t1) + tree_size(t2)
  end
```
