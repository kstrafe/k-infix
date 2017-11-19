#lang scribble/manual
@require[@for-label[k-infix
                    racket/base]]

@title{k-infix}
@author{kefin}

k-infix is a simple Haskell-like infix expression parser.

k-infix makes writing infix expressions in Racket easy. It's extensible as one can add operators freely. k-infix supports prefix, postfix, and binary operators. Left and right -associativity are also supported.

@section{Quick start}

To get started we require the library and use @racket[$] to write expressions.

@examples[#:eval evaluator #:label #f
  (require k-infix)
  ($ 1 + 2)
  ($ 1 + 2 / (3 * 4))
  ($ 2 ^ 3 ^ 4)]

The default expression parser handles arbitrary functions.
@examples[#:eval evaluator #:label #f
  (define (my-fn x y) (+ x (exp y)))
  ($ my-fn 1 2 + 3)]

Note that to use arbitrary expressions as function arguments, you must surround them by parentheses.
@examples[#:eval evaluator #:label #f
  ($ my-fn (1/2 + 1/2) 2)]

Otherwise, the operator will break the function application.
@examples[#:eval evaluator #:label #f
  (eval:error ($ my-fn 1/2 + 1/2 2))]

Arbitrary types are supported. Here, booleans.
@examples[#:eval evaluator #:label #f
  ($ 1 + 2 >= 3 and (5 + 2) / 2 < 5)]

Use ~ to escape parsing.
@examples[#:eval evaluator #:label #f
  ($ ~(if + 1 2) - 3)]

@section{Custom parse rules}

We can add our own parse rules using @racket[define-$+], this generates a new parser.
For example, let's make @racket[sqrt] a unary prefix operator.

@examples[#:eval evaluator #:label #f
  (require k-infix/define)
  (define-$+ $1 (sqrt 200 left -100))
  ($1 sqrt sqrt 16)]

@para{An operator entry (@racket[(sqrt 200 left -100)]) is of the form @racket[(id precedence associativity unary-precedence maybe-description)].}

@para{In the default context where @racket[sqrt] is a function and not an operator, evaluation would fail because it parses the above expression to @racket[(sqrt sqrt 16)].}

@examples[#:eval evaluator #:label #f
  (eval:error ($ sqrt sqrt 16))]

The binary precedence is 200, this means that it will bind very strongly to whatever is to its right.

@examples[#:eval evaluator #:label #f
  ($1 sqrt 8 * 2)
  (* (sqrt 8) 2)]

If we want multiplication take precedence we need to turn the binary precedence down to be lower than multiplication (100):
@examples[#:eval evaluator #:label #f
  (define-$+ $2 (sqrt 99 left -100))
  ($2 sqrt 8 * 2)
  (sqrt (* 8 2))]

When it comes to unary postfix operators the second precedence is important. It specifies how strong the operator's affinity is to be binary. For instance.

@examples[#:eval evaluator #:label #f
  (define-$+ $3 (! 99 left -100) (sqrt 99 left -100)) (code:comment "-100, really does not like to be binary")
  (define (! n) (if (positive? n) (* n (! (sub1 n))) 1))
  ($3 8 ! + sqrt 2)]

In this example, once @racket[+] was seen, @racket[!] was demoted to a unary postfix operator, then, once @racket[sqrt] was seen, @racket[+] was kept a binary operator, since @racket[sqrt] has a lower postfix precedence.

We can augment the example by investigating unary precedences among themselves.

@examples[#:eval evaluator #:label #f
  (define-$+ $ (! 99 left -100) (sqrt 99 left -100))
  ($ sqrt 16 !)]

From this we see that @racket[sqrt] binds more tightly if the precedences are equal. What if we increase the precedence of @racket[!]?

@examples[#:eval evaluator #:label #f
  (define-$+ $ (! 100 left -100) (sqrt 99 left -100))
  ($ sqrt 16 !)]

Now that we have defined the precedence we want, we may want to combine more rules, however, we don't want to keep re-specifying these rules. What we can do is combine parsers.

@examples[#:eval evaluator #:label #f
  (module other racket
     (provide meta-parser)
     (require k-infix/define)
     (define-$+ meta-parser (! 100 left -100) (sqrt 99 left -100)))
  (begin-for-syntax (require 'other))
  (define-$+ $ #:parsers (meta-parser) (I 120 left -100))
  (define I identity)
  ($ sqrt I 16 !)]

Combining @racket[define-$] forms of the parser is cumbersome because the definition is always one phase too low to be used in the next @racket[define-$]. The next section will show you how to keep every parser on the same phase using a non-define form.

@section{Lower level parsing}
k-infix provides low-level parser-generator 'primitives'. These are found in @racket[k-infix/custom], and their result is a function taking a syntax object as input.

@examples[#:eval evaluator #:label #f
  (require k-infix/custom)
  ($+ (sqrt 100 left -100))]

The primitives are @racket[$*] and @racket[$+], both are analogous to @racket[define-$*] and @racket[define-$+] respectively except that they do not define the parser one +1 phase.

@examples[#:eval evaluator #:label #f
  (define $ ($+ (sqrt 100 left -100)))
  ($ #'(1 + sqrt 2))]

Combining parsers is easy using the @racket[#:parser] directive as a first argument followed by a list of parsers. The parsers are merged in order, with conflicts resolved by prioritizing the last parser.

@examples[#:eval evaluator #:label #f
  (define $1 ($* (sqrt 200 left -100)))
  (define $2 ($* (I 200 left -100)))
  (define $3 ($* (+ 100 left 0)))
  (define $ ($* #:parsers ($1 $2 $3)))
  ($ #'(1 I + sqrt 2))]

One can also add rules whilst adding parsers.

@examples[#:eval evaluator #:label #f
  (define $ ($* #:parsers ($) (- 100 left 0)))
  ($ #'(1 I + sqrt 2 - 3))]

@section{Reference}

@defmodule[k-infix]

@defform[#:literals (or and bior bxor band = != >= <= > < <=> << >> + - * / % ^
                        not bnot ~)
         ($ item ...)
         #:grammar
         [(item infix-expr (code:line ~ (expr)))
          (infix-expr
            (code:line or)
            (code:line and)
            (code:line bior)
            (code:line bxor)
            (code:line band)
            (code:line =)
            (code:line !=)
            (code:line >=)
            (code:line <=)
            (code:line >)
            (code:line <)
            (code:line <=>)
            (code:line <<)
            (code:line >>)
            (code:line +)
            (code:line -)
            (code:line *)
            (code:line /)
            (code:line %)
            (code:line ^)
            (code:line not)
            (code:line bnot)
                      )]]{
  Parse an infix expression. If no @racket[item] is provided,
  the parser returns its operator table. An expression preceded by
  literal ~ will not be parsed.
}

@deftogether[(@defthing[bior procedure?]
              @defthing[bxor procedure?]
              @defthing[band procedure?]
              @defthing[<< procedure?]
              @defthing[>> procedure?]
              @defthing[% procedure?]
              @defthing[^ procedure?]
              @defthing[bnot procedure?])]{
  Operator aliases for @racket[bitwise-ior], @racket[bitwise-xor], @racket[bitwise-and], @racket[arithmetic-shift] (left) @racket[arithmetic-shift] (right),
  @racket[modulo], @racket[expt], and @racket[bitwise-not] respectively.
}

@defproc[(!= [x number?] [y number]) boolean?]{
  Inequality test.
  Equivalent to @code{
    (lambda (x y) (not (= x y)))
  }
}

@defproc[(<=> [x number?] [y number?]) exact-integer?]{
  Three-way numeric comparator.
  Returns 1 if x > y,
          0 if x = y,
         -1 if x < y.
}

@subsection{Define forms}
@defmodule[k-infix/define]

@defform[#:literals (left right)
         (define-$* name parse-lookup-entry ...)
         #:grammar
         [(name id)
          (parse-lookup-entry (id prec associativity maybe-postfix-prec))
          (prec exact-integer?)
          (associativity left right)
          (maybe-postfix-prec exact-integer?)]]{
  @para{Defines a parser with the given name and parse-lookup-entry unified with the default-parse-table as its lookup table.}
  @para{@racket[prec] is the precedence of the operator. The higher this is the tigher it binds to the surrounding expressions.}
  @para{@racket[associativity] is the associativity of the operator. Left-associativity means that the operator will nest left as
  @racket[(+ (+ (+ 1 2) 3) 4)]. Right-associativity nests right @racket[(^ (^ (^ 3 4) 2) 1)].}
  @para{@racket[maybe-postfix-prec] determines the precedence in unary disputs. A unary dispute is where we have @racket[v1 op1 op2 v2], where @racket[v1 v2] are values and @racket[op1 op2] are operators. The operator with the highest @racket[postfix-prec] will become binary, the other unary.}
}

@defform[#:literals (left right)
         (define-$+ name parse-lookup-entry ...)
         #:grammar
         [(name id)
          (parse-lookup-entry (id prec associativity maybe-postfix-prec))
          (prec exact-integer?)
          (associativity left right)
          (maybe-postfix-prec (code:line) exact-integer?)]]{
  The same as @racket[define-$*] but merges the lookup table with the default lookup table from @racket[$].
}

@subsection{Custom forms}
@defmodule[k-infix/custom]

@defform*[#:literals (left right)
          (
           ($* parse-lookup-entry ...)
           ($* #:parsers (parser ...) parse-lookup-entry ...)
          )
         #:grammar
         [(name id)
          (parsers expr)
          (parse-lookup-entry (id prec associativity maybe-postfix-prec))
          (prec exact-integer?)
          (associativity left right)
          (maybe-postfix-prec (code:line) exact-integer?)]]{
  Creates a new parser using the parse lookup table.
  The second form merges previous parsers. Parse rules are overwritten by those that are defined last.
}

@defform*[#:literals (left right)
          (
           ($+ parse-lookup-entry ...)
           ($+ #:parsers (parser ...) parse-lookup-entry ...)
          )
         #:grammar
         [(name id)
          (parsers expr)
          (parse-lookup-entry (id prec associativity maybe-postfix-prec))
          (prec exact-integer?)
          (associativity left right)
          (maybe-postfix-prec (code:line) exact-integer?)]]{
  Same as @racket[$*] but merges in the default parse table.
}

@subsection{Default parser rules}
Here are all operators, their precedence, and associativity respectively.
@examples[
  (require k-infix racket/pretty)
  (pretty-write ($))]

@(require racket/sandbox scribble/example)
@(define evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))

@section{Examples}
Here are some more examples to get you started.
@(define evaluator2
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))

@subsection{Coin flipping}

Suppose you have N coins and K flips. You can flip an arbitrary coin. All coins start face-down, and a flip has a 50% chance of change the coin to face-up. If a coin is face-up, you can take have it.

What is the expected value of @racket[(N K)] coins and flips given that you waste any remaining flips on a single coin once all coins are flipped?

@examples[#:eval evaluator2 #:label #f
  (require k-infix memoize)
  (define/memo (x n k)
    (match* (n k)
      [(_ 0) 0]
      [(0 k) ($ 1/2 * x 0 (k - 1) + 1/2 * (-1 + x 1 (k - 1)))]
      [(n k) ($ 1/2 * (1 + x (n - 1) (k - 1)) + 1/2 * x n (k - 1))]))
  (x 1 1)
  (x 2 1)
  (x 2 2)
  (x 2 3)
  (exact->inexact (x 50 250))]
