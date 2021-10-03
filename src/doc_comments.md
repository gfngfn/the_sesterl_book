## Doc comments

You can add doc comments to members in signatures by using `#[doc(String)]` attribute where `String` is an arbitrary string literal containing a text in Markdown:

````
module List :> sig
  ...

  #[doc(```
    `map f [v_1, …, v_n]` applies function `f` to each `v_i` in the given order,
    and builds the list [f v_1, …, f v_n] with the results produced by `f`.
  ```)]
  val map<$a, $b> : fun(fun($a) -> $b, list<$a>) -> list<$b>

  ...
end = struct
  ...
end
````

(Note: The outermost triple back ticks in the example above are NOT included in Markdown contents; they just start/terminate the string literal as double quotes do. If you want to use triple back ticks in Markdown contents to display code blocks, you can use quadruple back ticks for enclosing string literals.)

You can, for example, generate documents `./_docs/your_package.html` by specifying the following description in your configuration file:

```yaml
document_outputs:
  - format:
      type: "html"
    output_directory: "./_doc"
```
