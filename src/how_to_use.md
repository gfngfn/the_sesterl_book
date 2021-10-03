# How to use

## Building a single source file

Invoke:

```console
$ sesterl build <source-file> -o <output-dir>
```

where `<source-file>` is the path to the source file you want to build (e.g. `trial/hello_world.sest`), and `<output-dir>` is the directory where Erlang source files will be generated (e.g. `trial/_generated`).


## Building with Rebar3

[*Rebar3*](https://github.com/erlang/rebar3) is a popular build system for Erlang programs. Sesterl can collaborate with Rebar3.

Based on a configuration file (i.e., `sesterl.yaml`), the following command will generate `rebar.config`:

```console
$ sesterl config ./
```

Then you can invoke the following command to compile Sesterl programs before Rebar3 compiles Erlang code:

```console
$ rebar3 sesterl compile
```

Here, `sesterl` is a name space of Rebar3 commands for compiling Sesterl programs, and is introduced by plugin [`rebar_sesterl`](https://github.com/gfngfn/rebar_sesterl_plugin).

Running unit tests (by using *EUnit*) can be done by the following:

```console
$ rebar3 sesterl test
```
