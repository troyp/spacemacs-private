fn-arith.el -- Functional Arithmetic utilities for Emacs Lisp
-----

Split from [__fn.el__](https://github.com/troyp/fn.el).

------------------------------------------------------------

## Installation

Place `fn-arith.el` in any directory on your `load-path` and:

    (require 'fn-arith)

------------------------------------------------------------

### Function factories
* [fn-n*_](#partially-applied-arithmetic-functions) `(n)`
* [fn-_*n](#partially-applied-arithmetic-functions) `(n)`
* [fn-n+_](#partially-applied-arithmetic-functions) `(n)`
* [fn-_+n](#partially-applied-arithmetic-functions) `(n)`
* [fn-n-_](#partially-applied-arithmetic-functions) `(n)`
* [fn-_-n](#partially-applied-arithmetic-functions) `(n)`
* [fn-n/_](#partially-applied-arithmetic-functions) `(n)`
* [fn-_/n](#partially-applied-arithmetic-functions) `(n)`
* [fn-_^n](#partially-applied-arithmetic-functions) `(n)`
* [fn-n^_](#partially-applied-arithmetic-functions) `(n)`
* [fn-_%n](#partially-applied-arithmetic-functions) `(n)`
* [fn-n%_](#partially-applied-arithmetic-functions) `(n)`

------------------------------------------------------------

## Function Factories

### Partially applied arithmetic functions

Return partially applied arithmetic functions:

| factory fn    | generated fn |
|---------------|--------------|
| `fn-n*_ (n)`  | `(* N _)`    |
| `fn-_*n (n)`  | `(* _ N)`    |
| `fn-n+_ (n)`  | `(+ N _)`    |
| `fn-_+n (n)`  | `(+ _ N)`    |
| `fn-n-_ (n)`  | `(- N _)`    |
| `fn-_-n (n)`  | `(- _ N)`    |
| `fn-n/_ (n)`  | `(/ N _)`    |
| `fn-_/n (n)`  | `(/ _ N)`    |
| `fn-_^n (n)`  | `(expt _ N)` |
| `fn-n^_ (n)`  | `(expt N _)` |
| `fn-_%n (n)`  | `(mod _ N)`  |
| `fn-n%_ (n)`  | `(mod N _)`  |
