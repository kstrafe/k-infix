#lang scribble/manual
@require[@for-label[k-infix
                    racket/base]]

@title{k-infix}
@author{kefin}

@defmodule[k-infix]

k-infix is a simple Haskell-like infix expression parser.

@section{Introduction}

@defform[($ terms ...) #:grammar [(terms expr)]]

@(require racket/sandbox scribble/example)
@(define evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))
@examples[#:eval evaluator
  (require k-infix)
  ($ 1 + 2)
  ($ 1 + 2 / (3 * 4))

  (code:comment "^ is right-associative: parses as (^ 2 (^ 3 4))")
  (define ^ expt)
  ($ 2 ^ 3 ^ 4)]

The default expression parser handles arbitrary functions.
@examples[#:eval evaluator
  (require k-infix)
  (define (my-fn x y) (+ x (exp y)))
  ($ my-fn 1 2)]

Note that to use arbitrary expressions as function arguments, you must surround them by parentheses.
@examples[#:eval evaluator
  ($ my-fn (1/2 + 1/2) 2)]

Otherwise, the operator will break the function application.
@examples[#:eval evaluator
  (eval:error ($ my-fn 1/2 + 1/2 2))]

Arbitrary types are supported. Here, booleans.
@examples[#:eval evaluator
  ($ 1 + 2 >= 3 and (5 + 2) / 2 < 5)]

@subsection{Importing standard bindings}
Functions such as @racket[modulo] are not bound to @racket[%] by default. To do this we can import @racket[k-infix/default] which provides @racket[$ default-parse-table bior bxor band != <=> << >> % ^ bnot]

@(set! evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))

Bitwise operators.
@examples[#:eval evaluator
  (require k-infix/default)
  ($ - sin 3)
  ($ bnot 4 % 32818 bxor 3102 bior 31293 band 3 >> 2 << 3)]


Comparison operators.
@examples[#:eval evaluator
  (require k-infix/default)
  ($ 1 != (2 <=> 2))]

Other operators are available from the default racket namespace and are listed in @secref["dpr"].

@subsection{Parser separators}

For expressions that require non-infix evaluation you can use a @racket['separator] (see @secref["plt"]).
~ is the default separator. A parser separator is always unary and with highest precedence.

@examples[#:eval evaluator
  ($ 1 + ~(if + 3 4))]

Not using the separator in this case would result in the parser manipulating the @racket[if].

@examples[#:eval evaluator
  (eval:error ($ 1 + (if + 3 4)))]

The separator is only active for the immediately following expression

@examples[#:eval evaluator
  ($ 1 + ~(if + 3 4) * (2 + 1))]

Here is a more advanced example.
@examples[#:eval evaluator
  ($ 1 + ~(lambda (x y) (modulo x y)) (~(if + 3 4) * (2 + 1)) 3)]

@section[#:tag "plt"]{Parse lookup table}
The parser uses a lookup table to determine the precedence and associativity of operators, you can define your own using @racket[create-lookup-table].
@defform[(create-lookup-table plt)
         #:grammar ([plt ((op prec assoc maybe-description) ...)]
                    [assoc left right separator]
                    [maybe-description (code:line) description])]{
  Returns a parse lookup table.
}

@defproc[(parse-lookup-table? [table hash?]) boolean?]{
  Checks if a table is a valid parse-lookup-table.
}

@examples[
  (require k-infix/custom)
  (let ([table
          (create-lookup-table (+ 0 left "addition operator")
                               (^ 1 right "power operator"))])
    (writeln table)
    (parse-lookup-table? table))]

@section{Custom parse rules}

@racket[$*] and @racket[$+] allow you to create custom parse rules.

@defproc[($* [plt parse-lookup-table?]) procedure?]{
  Creates a parser with plt as the sole lookup table.
}

@defproc[($+ [plt parse-lookup-table?]) procedure?]{
  Creates a parser as a union of plt and the default-parse-table.
}

Suppose we want to add the identity operator the the @racket[default-parse-table].

@(set! evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))

@examples[#:eval evaluator
  (require (for-syntax racket k-infix/custom))
  (define-syntax (my-$ stx) (($+ #hash((I  . (0 left)))) stx))
  (define (I x) x)
  (my-$ 1 + I 5 )]

However, using @racket[I] as a binary operator will make the code fail during runtime.

@examples[#:eval evaluator
  (eval:error (my-$ 1 I 5))]

To make an operator be both unary and binary you can use @racket[case-lambda].

@examples[#:eval evaluator
  (let ([I (case-lambda
             [(x)   x]
             [(x y) (+ x y)])])
    (my-$ 1 I I 5))]

@subsection{define-$ forms}

Here are more ergnomic forms for defining new parsers.

@defform[(define-$* name plt)]{
  Defines a parser with the given name and plt (see @secref["plt"]) as its lookup table.
}

@defform[(define-$+ name plt)]{
  Defines a parser with the given name and plt (see @secref["plt"]) unified with the default-parse-table as its lookup table.
}

@(set! evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))

@examples[#:eval evaluator
  (require k-infix/define)
  (define-$+ my-$ (I 0 left))
  (define (I x) x)
  (my-$ 1 + I 5 )]

@examples[#:eval evaluator
  (define-$* my-$2 (+ 1 left) (* 0 right))
  (my-$2 1 + 2 * 5 )]

You can overwrite any rule from the default table by specifying it in @racket[define-$+].

@examples[#:eval evaluator
  (define-$+ my-$3 (- 0 right))
  (my-$3 1 - 2 - 3)]

@section{Examples}
Here are some more examples to get you started.
@(define evaluator2
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))
@subsection{Lists}
Lists are written using functional form.
@examples[#:eval evaluator2 #:label #f
  (require k-infix k-infix/define)
  ($ list 1 (2 + 3) 9 (sin 10 + 1))]
One can add a parse-rule for `list-append` like so
@examples[#:eval evaluator2 #:label #f
  (define-$* $- (append 0 left))
  ($- list 1 (2 + 3) 9 (sin 10 + 1) append list 'a 'b 'c)]

Or if you prefer @racket[++]. Let's also throw in some cons.
@examples[#:eval evaluator2 #:label #f
  (define ++ append)
  (define-$+ $2 (++ 0 left) (cons -1 left))
  ($2 ~(if (symbol? 'a-symbol) 'it-is-a-symbol #f) cons list 1 (2 + 3) 9 (sin 10 + 1) ++ list 'a 'b 'c)]

Here we've assigned @racket[cons] a precedence of -1, meaning that it will apply after @racket[++].

@subsection{Unary operators vs function application}

Depending on the precedence, square root may come before or after other operators. Here we use high precedence so that the code is evaluated as @racket[(+ (sqrt 3) (* (sqrt 5) 2))].

@examples[#:eval evaluator2 #:label #f
  (define-$+ $3 (sqrt 20 left))
  ($3 sqrt 3 + sqrt 5 * 2)]

One can also use lower precedence, but this will have no effect:

@examples[#:eval evaluator2 #:label #f
  (define-$+ $4 (sqrt 0 left))
  ($4 sqrt 3 + sqrt 5 * 2)]

The reason for this is that unary operators precede binary operators, because this unifies unary operators with function application. The only difference between function application and unary operators are recursive:

@examples[#:eval evaluator2 #:label #f
  (define-$+ $5 (sqrt 0 left))
  ($5 sqrt sqrt 5)
  (eval:error ($5 sqr sqr 5))]

The latter fails because it is parsed to @racket[(sqr sqr 5)], and the former @racket[(sqrt (sqrt 5))].

@subsection{Lambdas}

Using lambdas in k-infix is not trivial.

@examples[#:eval evaluator2 #:label #f
  (define-$+ $6 (lambda 20 left))
  ($6 ~(x) lambda (x - 1) 10)]

This doesn't evaluate the lambda because the parser isn't that smart; it can't know if (x - 1) will return a function that may bind 10, so it defaults to creating a lambda which applies @racket[((- x 1) 10)].

To remedy this we can create a helper function.

@examples[#:eval evaluator2 #:label #f
  (define-$+ $7 (lambda 20 left) (!-> 19 left))
  (define (!-> x y) (x y))
  ($7 ~(x) lambda (x - 1) !-> 10)]

It's much more convenient to define functions than to use lambdas.

@subsection{Coin flipping}

Suppose you have N coins and K flips. You can flip an arbitrary coin. All coins start face-down, and a flip has a 50% chance of change the coin to face-up. If a coin is face-up, you can take have it.

What is the expected value of @racket[(N K)] coins and flips given that you waste any remaining flips on a single coin once all coins are flipped?

@examples[#:eval evaluator2 #:label #f
  (require k-infix/default memoize)
  (define/memo (x n k)
    (match `(,n ,k)
      [`(,_ 0) 0]
      [`(0 ,k) ($ 1/2 * x 0 (k - 1) + 1/2 * (-1 + x 1 (k - 1)))]
      [`(,n ,k) ($ 1/2 * (1 + x (n - 1) (k - 1)) + 1/2 * x n (k - 1))]))
  (x 1 1)
  (x 2 1)
  (x 2 2)
  (x 2 3)
  (exact->inexact (x 50 250))]

@section{Miscellaneous}

@subsection{Fetching a parser's lookup table}

The table for a parser can be retrieved by running the parser without any arguments.

@examples[#:eval evaluator
  (my-$2)]

@subsection[#:tag "dpr"]{Default parser rules}
Here are all operators, their precedence, and associativity respectively.
@examples[
  (require k-infix/default racket/pretty)
  (pretty-write default-parse-table)]
