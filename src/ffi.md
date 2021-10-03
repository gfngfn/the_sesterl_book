## FFI

Functions written in Erlang can be called from Sesterl via FFI (foreign function interface) as follows:

````
module FfiExample = struct

  val assoc<$a> : fun(int, list<(int, $a)>) -> option<($a, list<(int, $a)>)> = external 2 ```
assoc(Key, Xs) ->
    case lists:keytake(Key, 1, Xs) of
        false                 -> error;
        {value, {_, V}, Rest} -> {ok, {V, Rest}}
    end.
  ```

  val main() =
    assoc(1, [
      (3, "Komaba"),
      (1, "Hongo"),
      (4, "Yayoi"),
      (1, "Asano"),
      (5, "Kashiwa")
    ])

end
````

This program compiles to the following implementation:

```erlang
-module('FfiExample').
-export([assoc/2, main/0]).

assoc(Key, Xs) ->
  case lists:keytake(Key, 1, Xs) of
    false                 -> error;
    {value, {_, V}, Rest} -> {ok, {V, Rest}}
  end.

main() ->
  'FfiExample':assoc(1, [
    {3, <<"Komaba">>},
    {1, <<"Hongo">>},
    {4, <<"Yayoi">>},
    {1, <<"Asano">>},
    {5, <<"Kashiwa">>}]).
```
