#lang racket/base

(provide $* $+ create-lookup-table parse-lookup-table?)

(require syntax/parse syntax/parse/define
         racket/bool racket/list racket/hash
         (for-syntax racket/base)
         "default-parse-table.rkt"
         "private/helper.rkt"
         "private/logger.rkt")

(define ($* plt)
  (lambda (stx)
    (syntax-parse stx
      [(_) (datum->syntax stx plt)]
      [(_ terms ...+)
       #:with result (parse stx (attribute terms) plt)
       (datum->syntax stx (attribute result))]
      )))

(define ($+ plt)
  ($* (hash-union default-parse-table plt
                  #:combine (lambda (x y) y))))

(define (parse-lookup-table? table)
  (and
    (andmap symbol? (hash-keys table))
    (andmap entry? (hash-values table))))
