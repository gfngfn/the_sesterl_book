## Concurrency

As in Erlang, you can use primitives `self`, `send`, and `spawn` for message-passing concurrency. They are given types by using a kind of monadic function types `fun(τ_1, …, τ_n) -> [τ]τ'` and types `pid<τ>` for PIDs (i.e. process identifiers) as follows:

* `self<$p> : fun() -> [$p]pid<$p>`
* `send<$p, $q> : fun(pid<$q>, $q) -> [$p]unit`
* `spawn<$p, $q> : fun(fun() -> [$q]unit) -> [$p]pid<$q>`

Intuitively, `[τ]τ'` in `fun(τ_1, …, τ_n) -> [τ]τ'` stands for concurrent computations that will be run on processes capable of receiving messages of type `τ` and that finally produce a value of type `τ'`. The composition of such computations can be done by `do`-notation. Messages can be received by using `receive`-expressions. See a small example below:

```
module Example = struct

  /* dummy */
  val some_heavy_calculation(n) =
    n

  val rec wait_all(msgacc, n) = act
    if n <= 0 then
      return(msgacc)
    else
      receive
      | {pid, msg} ->
          let _ = print_debug(format(f'message ~p received from: ~p~n', {msg, pid})) in
          wait_all(msg :: msgacc, n - 1)
      end

  val rec spawn_all(pidacc, n) = act
    if n <= 0 then
      return(pidacc)
    else
      do parent <- self() in
      do pid <-
        spawn(fun() -> act
          do me <- self() in
          let msg = some_heavy_calculation(n) in
          send(parent, {me, msg})
        end)
      in
      spawn_all(pid :: pidacc, n - 1)

  val main(arg) = act
    let n = 10 in
    do pids <- spawn_all([], n) in
    let _ = print_debug(format(f'spawned: ~p~n', {pids})) in
    do msgs <- wait_all([], n) in
    let _ = print_debug(msgs) in
    return({})

end
```

Here, the primitive `return<$p, $a> : fun($a) -> [$p]$a` lifts a pure value to the computation that has no effect and simply returns the value.

The function `spawn_all` takes an integer `n`, spawns `n` processes that perform some heavy calculation in parallel, and returns their PIDs. `wait_all`, on the other hand, waits all the messages sent from the processes spawned by `spawn_all` and makes a list of the received messages. These functions are typed as follows, supposing `some_heavy_calculation` is of type `fun(int) -> answer`:

* `spawn_all<$p, $q> : fun(list<pid<$q>>, int) -> [$p]list<pid<$q>>`
* `wait_all<$q> : fun(list<answer>, list<pid<$q>>) -> [{pid<$q>, answer}]list<answer>`

As mentioned earlier, supporting session types is an important future work. One possible way of supporting session types would be adopting types of the form `[S]τ` where `S` is a session type by using theories like \[Orchard & Yoshida 2016\].
