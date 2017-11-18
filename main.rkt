#lang racket

(provide $)


(require (for-syntax "custom.rkt"
                     "default-parse-table.rkt"))

(define-syntax ($ stx) (($* default-parse-table) stx))
