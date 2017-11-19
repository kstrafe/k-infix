#lang racket

(provide $
         bior bxor band
         != <=>
         << >> % ^
         bnot)

;; Infix operations in Racket
;
; # Simple example
;
; (require k-infix)
; ($ - 3 ^ 4 * 2)

(require (for-syntax "private/default-parse-table.rkt"
                     "private/primitive.rkt"))

(define-syntax ($ stx) ((-$* default-parse-table)
                        (datum->syntax stx (cdr (syntax-e stx)))))

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
(define % modulo)
(define ^ expt)
(define bnot bitwise-not)
