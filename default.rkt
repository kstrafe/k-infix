#lang racket/base

(provide $
         default-parse-table
         bior bxor band
         != <=>
         << >> % ^
         bnot)

(require "main.rkt" "default-parse-table.rkt")

(define bior bitwise-ior)
(define bxor bitwise-xor)
(define band bitwise-and)
(define != (lambda (x y) (not (= x y))))
(define <=> (lambda (x y) (cond
                            ([> x y] 1)
                            ([= x y] 0)
                            ([< x y] -1))))
(define << arithmetic-shift)
(define >> (lambda (x y) (arithmetic-shift x (- y))))
(define % (lambda (x y) (modulo x y)))
(define ^ expt)
(define bnot bitwise-not)
