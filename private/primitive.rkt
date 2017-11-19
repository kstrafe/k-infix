#lang racket/base

(provide -$* -$+)

(require racket/hash syntax/parse
         "default-parse-table.rkt"
         "helper.rkt")

(define (-$* plt)
  (case-lambda
    [() plt]
    [(stx)
     (syntax-parse stx
       [() (datum->syntax stx plt)]
       [(terms ...+)
        #:with result (parse stx (attribute terms) plt)
        (datum->syntax stx (attribute result))])]))

(define (-$+ plt)
  (-$* (hash-union default-parse-table plt
                  #:combine (lambda (x y) y))))

