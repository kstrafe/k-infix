#lang scribble/manual
@require[@for-label[k-infix
                    racket/base]]

@title{k-infix}
@author{kefin}

@defmodule[k-infix]

k-infix is a simple Haskell-like infix expression parser.

@section{Introduction}

@defform[($ terms ...) #:grammar [(terms expr)]]

@(require racket/sandbox scribble/eval)
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
  ($ bnot 4 % 32818 bxor 3102 bior 31293 band 3 >> 2 << 3)]


Comparison operators.
@examples[#:eval evaluator
  (require k-infix k-infix/default)
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

@(require racket/sandbox scribble/eval)
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

@subsection{Fetching a parser's lookup table}

The table for a parser can be retrieved by running the parser without any arguments.

@examples[#:eval evaluator
  (my-$2)]


@section[#:tag "dpr"]{Default parser rules}
Here are all operators, their precedence, and associativity respectively.
@examples[
  (require k-infix/default racket/pretty)
  (pretty-write default-parse-table)]
