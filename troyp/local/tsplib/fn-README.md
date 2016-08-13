fn.el -- Functional utilities for Emacs Lisp.
-----

While [`cl`](https://www.gnu.org/software/emacs/manual/html_node/cl/) and
[`dash`](https://github.com/magnars/dash.el) provide a good foundation for
functional programming in Emacs Lisp, some things remain unavailable, 
inconvenient or verbose.

__fn.el__ provides a set of utilities for functional programming in Emacs Lisp.
It focuses on concision and readability and is the sister library to
[__ls.el__](https://github.com/troyp/ls.el).

------------------------------------------------------------

## Installation

Place `fn.el` in any directory on your `load-path` and:

    (require 'fn)

------------------------------------------------------------

## API

### Function creation: combinators and lambda forms
* [fn](#fn-rest-body) `(&rest body)`
* [fn:](#fn-rest-body) `(&rest body)`
* [fn-bindl](#fn-bindl-function-rest-largs) `(function &rest largs)`
* [fn-bindr](#fn-bindr-function-rest-rargs) `(function &rest rargs)`
* [fn-fork](#fn-fork-merge-function-rest-functions) `(merge-function &rest functions)`

### Other higher-order constructs
* [fn-call](#fn-call-function-rest-arguments) `(function &rest arguments)`
* [fn-case](#fn-case-expr-rest-cases) `(case-expr &rest cases)`
* [fn-alias](#fn-alias-bindings-rest-body) `(bindings &rest body)`
* [fn-setq](#fn-setq-rest-bindings) `(&rest bindings)`
* [fn-pos?](#fn-pos?-transform) `(transform)`
* [fn-neg?](#fn-neg?-transform) `(transform)`
* [fn-zero?](#fn-zero?-transform) `(transform)`

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

## Function Creation

#### fn `(&rest BODY)`

Return a function defined by BODY.

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

Return a function defined by BODY.

The definition BODY may use anaphoric parameters to refer to the arguments. For
a single-argument function, use `<>` or `it`. For a multiple-argument function,
use `<1>` to refer to the first argument, `<2>` to refer to the second, and so
on up to `<9>`.

If applied to a literal, creates a constant function, or equivalently, a thunk
(since it can be called with any number of arguments).

    (-map (fn (* <> <>)) (number-sequence 0 10))
    ;; (0 1 4 9 16 25 36 49 64 81 100)

    (-map (fn (/ (-sum <>)
                 (length <>)))
          '((3.0 4.0 5.0 5.0 10.0)
            (1.0 2.0 2.0 2.0)
            (1 5)))
    ;; (5.4 1.75 3)
    ;; find average of each list

    (-filter (fn (zerop (mod <> 3)))
             (number-sequence 1 10))
    ;; (3 6 9)
    

#### fn: `(&rest BODY)`

Return a function defined by (BODY).

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

Return a function defined by BODY.

The definition BODY may use the anaphoric parameters `<>`, `it` and `<1>` to refer to
the first argument, `<2>` to refer to the second, and so on up to `<9>`.

    (-map (fn: * <> <>) (number-sequence 0 10))
    ;; (0 1 4 9 16 25 36 49 64 81 100)

    (-filter (fn: > it 0)
            '(-5 2 0 0 3 -1 0 4))
    ;; (2 3 4)
    
#### fn-bindl `(function &rest largs)`

Partial application of FUNCTION to LARGS.

#### fn-bindr `(function &rest rargs)`

Right-hand partial application of FUNCTION to RARGS.

#### fn-fork `(merge-function &rest functions)`

Apply MERGE-FUNCTION to FUNCTIONS to produce a composed function.

    (fn-call (fn-fork 'list '+ '*) 2 3 5)
    ;; (10 30)
    ;; compute the sum and product
    ;; equivalent to: (fn-call (-juxt '+ '*) 2 3 5)

    (fn-call (fn-fork '/ '-sum 'length)
            '(2.0 0 2.0 3.0 5.0 1.0 8.0))
    ;; 3.0
    ;; compute the mean of a list

    (-map (fn-fork '+
            (fn (expt <> 5))
            (fn <>)
            (fn 1))
          '(1 2 10))
    ;; (3 35 100011)
    ;; compute the polynomial x^5+x+1 at the points 1, 2, 10

Notes:

* This is variadic analogue of J's `fork` phrasal form.
* It is also a generalization of dash.el's `-juxt` function, which is
equivalent to `fork` with the `list` function passed as the first argument.

#### fn-pos? `(transform)`

Return a predicate that returns t if TRANSFORM of its argument is positive.

    (-filter (fn-pos? 'car)
            '(( 3 .  0)
              (-2 .  5)
              ( 0 .  1)
              ( 9 . -3)))
    ;; ((3 . 0) (9 . -3))

#### fn-neg? `(transform)`

Return a predicate that returns t if TRANSFORM of its argument is negative.

    (-filter (fn-neg? 'car)
            '(( 2 . -2)
              (-2 .  3)
              ( 0 .  1)
              (-9 .  0)))
    ;; ((-2 . 3) (-9 . 0))

#### fn-zero? `(transform)`

Return a predicate that returns t if TRANSFORM of its argument is zero.

    (-filter (fn-zero? 'car)
            '(( 0 .  0)
              (-1 .  1)
              ( 0 .  1)
              ( 7 . -5)))
    ;; ((0 . 0) (0 . 1))


------------------------------------------------------------

## Other Higher-Order Constructs

#### fn-case `(expr &rest cases)`

Eval EXPR and choose value by sequentially testing predicates.

    (fn-case 77
      ((fn (> it 100))  'too-big)
      (oddp             'odd-number)
      (evenp            'even-number))
    odd-number

Ideal for fairly simple case expressions involving predicates.  If you only
need to test equality against values, use regular `case`.  If you need more
complex predicates, consider using the more powerful but verbose `pcase`.

#### fn-call `(FUNCTION &rest ARGUMENTS)`

Alias for `funcall`.

#### fn-alias `(bindings &rest body)`

Execute with temporary function definitions.

Each DEFN may be a either a symbol, in which case FUNC is bound to its
`symbol-function`, or an expression evaluating to a function value.

    (fn-alias ((seq    'number-sequence)
              (double  (fn (* 2 <>))))
      (--map (double it) (seq 1 10)))
    ;; (2 4 6 8 10 12 14 16 18 20)

#### fn-setq `(&rest bindings)`

Set the FUNCTION definition of each SYMBOL in a set of BINDINGS.

    (fn-setq my-increment (fn-bindl '+ 1)
             my-double    (fn-bindl '* 2))

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

------------------------------------------------------------

## TODO

* `fn-using-fn.el` -- Function-binding macro providing short names for __fn.el__ constructs.
* Infix expression macro -- curly bracket notation?
* `fn-defpartial`, `fn-defpartial-right` -- Defining named partially applied functions.
* `fn-alias*`, `fn-setq*`

------------------------------------------------------------
