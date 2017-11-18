#lang racket

(provide $)


(module+ test (require rackunit syntax/parse/define))

(require (for-syntax "custom.rkt"
                     "default-parse-table.rkt"))

(define-syntax ($ stx) (($* default-parse-table) stx))

(module+ test
  (define-syntax-parser check-expr-results
    #:datum-literals (:)
    [(_ (v:expr : (x:expr ...+)) ...)
     #'(begin (check-equal? ($ x ...) v) ...)])
  (define (f1 x) x)
  (define (f2-1 x y) x)
  (define (f2-2 x y) y)
  (define (f3-1 x y z) x)
  (define (f3-2 x y z) y)
  (define (f3-3 x y z) z)
  (define ^ expt)
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
    (4       : (2 ^ (3 - 1)))
    (4       : (2 ^ (2 * ((2 + 1) / 2) - 1)))
    ))
