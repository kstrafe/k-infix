#lang racket/base

(require "main.rkt" "define.rkt" rackunit syntax/parse/define (for-syntax racket/base))

(define-syntax-parser check-expr-results
  #:datum-literals (:)
  [(_ (v:expr : (x:expr ...+)) ...)
   #'(begin (check-equal? ($ x ...) v) ...)])
(define-syntax-parser check-expr-fails
  #:datum-literals (:)
  [(_ (v:expr : (x:expr ...+)) ...)
   #'(begin (check-exn v (lambda () ($ x ...))) ...)])

(define (f1 x) x)
(define (f2-1 x y) x)
(define (f2-2 x y) y)
(define (f3-1 x y z) x)
(define (f3-2 x y z) y)
(define (f3-3 x y z) z)

(check-expr-results
  (123    : (123))
  (-123   : (- 123))
  (3      : (1 + 2))
  (0      : (1 + 2 - 3))
  (0      : (1 + 2 - 3))
  (1      : (f1 1))
  (1      : (f2-1 1 2))
  (2      : (f2-2 1 2))
  (1      : (f3-1 1 2 3))
  (2      : (f3-2 1 2 3))
  (3      : (f3-3 1 2 3))
  (262144 : (4 ^ 3 ^ 2))
  (-6     : (- 1 - 2 - 3))
  (0      : (- 1 - 2 + 3))
  (3      : (- 1 - 2 + 3 * 2))
  (7/2    : (- 1 / 2 - 2 + 3 * 2))
  (-5/2   : (- 1 / 2 - 2 ^ 3 + 3 * 2))
  (-5/2   : (- 1 / 2 - 2 ^ f1 3 + 3 * 2))
  (-5/2   : (- 1 / 2 - 2 ^ f2-2 2 3 + 3 * 2))
  (-5/2   : (- 1 / 2 - 2 ^ f3-3 1 2 3 + 3 * 2))
  (4      : (2 ^ (3 - 1)))
  (4      : (2 ^ (2 * ((2 + 1) / 2) - 1)))
  (56     : ((5 bior 2) << 3))
  (21     : (5 bior 2 << 3))
  (21     : (5 bior (2 << 3)))
  (3      : ((5 bior 2) << 3 >> 4))
  (5      : (5 bior 2 << 3 >> 4))
  (7      : (5 bior 2 << (3 >> 4)))
  (3      : (bnot bnot 3))
  (#t     : (bnot 3 < 3))
  (1      : (2 <=> 0))
  (0      : (2 <=> 2))
  (-1     : (0 <=> 2))
  (1      : (2 <=> 0))
  (-1     : (2 <=> 2 ^ 10))
  (-1     : (0 <=> 2))
  (-9     : (- - - 3 * 3))
  (9      : (- - - - - - - - - - - - - - - - - - - - - - - - 3 * 3))
  (-9     : (- - - - - - - - - - - - - - - - - - - - - - - - - 3 * 3))
  (10     : (- - - - - - - - - - - - - - - - - - - - - - - - 3 * 3 + ~(if + 1 2) * cos 0))

  (0      : (- sin ~(if #t 0 1)))
  (-1     : (- cos ~(if #t 0 1)))
  (3      : (- ~(tan 0) + ~(if - 3 2)))
  (3      : (- ~(tan (if * 0 2)) + ~(if - 3 2)))
  (1      : (cos (sin 0)))
  (-2     : (- (1 + 1)))
  (-10    : (5 * - (1 + 1)))
  (3      : (5 + cos 0 * - (1 + 1)))
  (7      : (5 + cos 0 * - - (1 + 1)))
  (7      : (5 + cos 0 * - - (1 + 1)))
  (-45753584909922  : (2 * -(1 + 2) ^ -(2 + 5 * - (2 + 4))))
  (3      : (1 + * 2))
  (2      : (1 * + 2))
  (-2     : (- 1 * + 2))
  (1      : (- 1 + * 2))
  (-3     : (- 1 + * - 2))
  (1      : (- 1 + * - - 2))
  (-1/2   : (- 1 + * - / - 2))
  )

(define e? check-equal?)

(test-case "unary"
  ; naive non-tail-call factorial
  (define (! n)
    (cond
      [(< n 0) (error "negative argument")]
      [(zero? n) 1]
      [else (* n (! (sub1 n)))]))

  ; factorial ! has higher precedence than *
  (define-$+ $ (! 110 left -110))

  ; the algorithm doesn't really distinguish between postfix and prefix.
  (e? 24 ($ 4 !))
  (e? 24 ($ ! 4))

  (e? 2/3 ($ 1 - 3 /)) ; unary /x is 1/x

  (e? 4 ($ - 2 + 3 !))
  (e? -2 ($ - 3 ! + 4))
  (e? -18 ($ - 3 * 3 !)) ; -(3 * (3!))
  (e? 36 ($ - 3 ! * - 3 !))
  (e? 720 ($ ! 3 !))
  (e? 12 ($ 2 * 3 !)) ; 2 * (3!)
  (e? 24 ($ 2 * 3 ! * 2)) ; 2 * (3!) * 2
)
