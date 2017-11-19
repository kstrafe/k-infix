#lang racket

(provide $)

;; Infix operations in Racket
;
; # Simple example
;
; (require k-infix)
; ($ - 3 ^ 4 * 2)
;
; # Custom parsing

(require (for-syntax "custom.rkt"
                     "default-parse-table.rkt"))

(define-syntax ($ stx) (($* default-parse-table) stx))
