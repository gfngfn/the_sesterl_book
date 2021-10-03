## Overall syntax

How to read:

* a word enclosed by single quotation marks (e.g. `'let'` or `'('`):
  - a keyword token or a symbol token
* a word without quotations (e.g. `E` or `val-args`):
  - a metavariable of the (extended) BNF
* `(empty)`
  - the empty sequence of tokens
* `( DESCR )*`
  - a possibly empty finite repetition of `DESCR`
* `( DESCR )+`
  - equals `DESCR ( DESCR )*`
* `(empty)`
  - no token (i.e. a token sequence of length zero)
* `(DESCR1 | DESCR2)`
  - either `DESCR1` or `DESCR2`
* `( DESCR )?`
  - equals `((empty) | DESCR)`

```
n ::= (decimal or hexadecimal integer literals)
float-lit ::= (floating-point number literals)
bin-lit ::= (string literals enclosed by double quotation marks)
X, C ::= (capitalized identifiers)
x, t, k, l ::= (lowercased identifiers other than keywords)
$a ::= (lowercased identifiers preceded by a dollar sign)
?$a ::= (lowercased identifiers preceded by a question mark and a dollar sign)
-l ::= (lowercased identifiers preceded by a hyphen)
?l ::= (lowercased identifiers preceded by a question mark)

# source files:
source-file ::=
  | ('import' X)* 'module' X (':>' S) '=' 'struct' (open-spec)* (bind)* 'end'

# value expressions:
E ::=
  | '(' E ')'
  | E binary-operator E
  | (X '.')* x
  | (X '.')* C                                      # variant constructors
  | E '(' val-args ')'                              # function applications
  | 'let' bind-val-local 'in' E                     # local bindings
  | 'let' pattern '=' E 'in' E                      # local bindings by the pattern matching
  | 'fun' '(' val-params ')' '->' E 'end'           # pure abstractions
  | 'fun' '(' val-params ')' '->' 'act' P 'end'     # effectful abstractions
  | 'if' E 'then' E 'else' E                        # conditionals
  | 'case' E 'of' (pure-case)+ 'end'                # pattern-matching expressions
  | '{' '}'                                         # the unit value
  | '{' E (',' E)* (',')? '}'                       # tuples
  | '{' l '=' E (',' l '=' E)* (',')? '}'           # records
  | E '.' l                                         # record access
  | '{' E '|' l '=' E (',' l '=' E)* (',')? '}'     # record update
  | 'freeze' (X '.')* x '(' freeze-args ')'         # so-called (possibly partial) mfargs() in Erlang
  | 'freeze' '(' E ')' 'with' '(' freeze-args ')'   # addition of arguments to partial mfargs()
  | 'pack' M ':' S                                  # packed first-class modules
  | 'assert' E                                      # assertion for tests
  | E '::' E                                        # cons
  | '[' ']'                                         # nil
  | n
  | float-lit
  | bin-lit
  | 'true'
  | 'false'
  | ...

pure-case ::=
  | '|' pattern '->' E

# effectful computations:
P ::=
  | 'do' pattern '<-' P 'in' P                     # sequential compositions (i.e. so-called monadic binds)
  | 'receive' (effectful-case)+ after-branch 'end' # selective receive
  | E '(' val-args ')'                             # function applications
  | 'if' E 'then' P 'else' P                       # conditionals
  | 'case' E 'of' (effectful-case)+ 'end'          # pattern-matching expressions

effectful-case ::=
  | '|' pattern '->' P

after-branch ::=
  | (empty)
  | 'after' E '->' P

# sequences of arguments for function applications:
val-args ::=
  | E (',' val-args)?
  | val-labeled-args

val-labeled-args ::=
  | -l E (',' val-labeled-args)?
  | val-optional-args

val-optional-args ::=
  | ?l E (',' val-optional-args)?
  | (empty)

# patterns for the pattern matching:
pattern ::=
  | '_'                                     # wildcard
  | x                                       # variable binding
  | C                                       # constructors with no argument
  | C '(' pattern (',' pattern)* (',')? ')' # constructors with arguments
  | '{' '}'                                 # the unit pattern
  | '{' pattern (',' pattern)* (',')? '}'   # tuples
  | pattern '::' pattern                    # cons
  | '[' ']'                                 # nil
  | n
  | bin
  | 'true'
  | 'false'
  | ...

# types:
T ::=
  | $a                                       # type variables
  | (X '.')* t ty-args                       # applications of type constructors
  | 'fun' '(' ty-doms ')' '->' T             # function types
  | 'fun' '(' ty-doms ')' '->' '[' T ']' T   # action types
  | '{' T (',' T)* (',')? '}'                # product types
  | '{' l '=' T (',' l '=' T)* (',')? '}'    # record types
  | 'pack' S                                 # types for first-class modules

# sequences of type arguments:
ty-args ::=
  | ('<' ty-args-sub '>')?

ty-args-sub ::=
  | T (',' ty-args-sub)?
  | (empty)

# sequences of domain types:
ty-doms ::=
  | T (',' ty-doms)?
  | ty-labeled-doms

ty-labeled-doms ::=
  | -l T (',' ty-labeled-doms)?
  | ty-optional-doms
  | ?$a

ty-optinal-doms ::=
  | ?l T (',' ty-optional-doms)?
  | (empty)

# a kind:
K ::=
  | kd-base                                              # base kinds (i.e. order-0 kinds)
  | '(' kd-base (',' kd-base)* (',')? ')' '->' kd-base   # order-1 kinds

kd-base ::=
  | k      # named base kinds (currently only 'o' is provided)
  | kd-row # row kinds

kd-row ::=
  | '(' labels ')'

labels ::=
  | l ',' labels
  | l
  | (empty)

open-spec ::=
  | 'open' (X '.')* X

# module expressions:
M ::=
  | '(' M ')'
  | (X '.')* X
  | 'struct' (open-spec)* (bind)* 'end' # structures
  | 'fun' '(' X ':' S ')' '->' M        # functor abstractions
  | (X '.')* X '( M )'                  # functor applications
  | X ':>' S                            # coercion

# bindings (i.e. members of structures):
bind ::=
  | 'val' (bind-val-local | bind-val-ffi)
  | 'type' bind-ty
  | 'module' X (':>' S)? '=' M
  | 'signature' X '=' S
  | 'include' M

# signature expressions:
S ::=
  | '(' S ')'
  | (X '.')* X
  | 'sig' (open-spec)* (decl)* 'end' # structure signatures
  | 'fun' '(' X ':' S ')' '->' S     # functor signatures
  | S 'with' 'type' bind-ty

# declarations (i.e. members of structure signatures):
decl ::=
  | 'val' x ty-quant ':' T
  | 'type' t ('::' K)?
  | 'type' t '=' bind-ty
  | 'module' X ':' S
  | 'signature' X '=' S

bind-val-local ::=
  | bind-val-single                                  # non-recursive definitions
  | 'rec' bind-val-single ('and' bind-val-single)*   # (mutually) recursive definitions

bind-val-single ::=
  | x ty-quant '(' val-params ')' (':' T)? '=' E                   # function definitions
  | x ty-quant '(' val-params ')' (':' '[' T ']' T)? '=' 'act' P   # action definitions

bind-val-ffi ::=
  | x ty-quant ':' T '=' 'external' n ('+')? string-block  # FFI

bind-ty ::=
  | bind-ty-single ('and' bind-ty-single)*

bind-ty-single ::=
  | t ty-quant '=' ('|')? ctor-branch ('|' ctor-branch)*   # variant type definitions
  | t ty-quant '=' T                                       # type synonym definitions

ctor-branch ::=
  | C ('(' T (',' T)* ')')?   # a definition of a constructor and its parameter types

# comma-separated sequences of value parameters (for function definitions):
val-params ::=
  | pattern (':' T)? (',' val-params)?
  | val-labeled-params

# comma-separated labeled parameters:
val-labeled-params ::=
  | -l pattern (':' T)? (',' val-labeled-params)?
  | val-optional-params

# comma-separated labeled optional parameters (possibly with default expressions):
val-optional-params ::=
  | ?l pattern (':' T)? ('=' E)? (',' val-optional-params)?
  | (empty)

# sequences of universal quantifiers for type parameters and row parameters
ty-quant ::=
  | ('<' ty-params '>')?

ty-params ::=
  | $a ',' ty-params
  | $a
  | row-params

row-params ::=
  | ?$a '::' kd-row (',' row-params)?
  | (empty)
```
