## OTP as functors

One of the interesting use cases of the module system is to represent OTP libraries by using functors; for example, `gen_server` can be represented by a functor that takes the callback functions (such as `init/1` or `handle_cast/3`) and related types and that returns modules that contains the specialized version of functions provided by `gen_server` (such as `cast/2`, `call/3`, `start_link/1`, etc.). The functor `GenServer.Make` defined in `sesterl_stdlib` as follows represents principal functionalities of `gen_server`:

```
  module GenServer : sig

    type initialized :: (o) -> o
    val init_ok<$msg, $state> : fun($state) -> [$msg]initialized<$state>
    val init_stop<$msg, $state> : fun(StopReason.t) -> [$msg]initialized<$state>
    type reply :: (o, o, o) -> o
    val reply<$msg, $response, $state> :
      fun($response, $state, ?timeout int) -> [$msg]reply<$msg, $response, $state>
    val reply_and_stop<$msg, $response, $state> :
      fun(StopReason.t, $response, $state) -> [$msg]reply<$msg, $response, $state>
    type no_reply :: (o) -> o
    val no_reply<$msg, $state> : fun($state, ?timeout int) -> [$msg]no_reply<$state>
    val no_reply_and_stop<$msg, $state> : fun(StopReason.t, $state) -> [$msg]no_reply<$state>
    type start_link_error = RawValue.t
    type call_error = RawValue.t

    signature Behaviour = sig
      type init_arg :: o
      type request :: o
      type response :: o
      type cast_message :: o
      type info :: o
      type state :: o
      type global :: o
      val init : fun(init_arg) -> [info]initialized<state>
      val handle_call<$a> : fun(request, pid<$a>, state) -> [info]reply<info, response, state>
      val handle_cast : fun(cast_message, state) -> [info]no_reply<state>
      val handle_info : fun(info, state) -> [info]no_reply<state>
      val handle_timeout : fun(state) -> [info]no_reply<state>
      val handle_down<$a> : fun(MonitorRef.t, pid<$a>, StopReason.t, state) -> [info]no_reply<state>
      val terminate : fun(StopReason.t, state) -> [info]unit
    end

    module Make : fun(Callback : Behaviour) -> sig
      type proc :: o
      val as_pid : fun(proc) -> pid<Callback.info>
      val from_pid : fun(pid<Callback.info>) -> proc
      val call<$a> : fun(proc, Callback.request, ?timeout int) -> [$a]result<Callback.response, call_error>
      val cast<$a> : fun(proc, Callback.cast_message) -> [$a]unit
      val send_info<$a> : fun(proc, Callback.info) -> [$a]unit
      val start_link<$a> : fun(Callback.init_arg) -> [$a]result<proc, start_link_error>
      val start_link_name<$a> : fun(Callback.init_arg, -name name<Callback.global>) -> [$a]result<proc, start_link_error>
      val where_is_local<$a> : fun(binary) -> [$a]option<proc>
      val where_is_global<$a> : fun(Callback.global) -> [$a]option<proc>
      val stop<$a> : fun(proc) -> [$a]unit
    end
  end
```
