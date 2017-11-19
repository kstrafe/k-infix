#lang racket/base

(provide $* $+)

(require syntax/parse/define racket/hash
         (for-syntax racket/base "private/parse-lookup-entry.rkt")
         "private/helper.rkt"
         "private/primitive.rkt")

(define-syntax-parser $*
  [(_ #:parsers ~! (parser ...) entry:parse-lookup-entry ...)
   #'(-$* (hash-union #:combine (lambda (x y) y)
            (parser) ...
            (create-lookup-table (entry.op
                                  entry.prec
                                  entry.assoc
                                  entry.unary-prec
                                  entry.desc) ...)))]
  [(_ entry:parse-lookup-entry ...)
    #'(-$* (create-lookup-table (entry.op
                                 entry.prec
                                 entry.assoc
                                 entry.unary-prec
                                 entry.desc) ...))])

(define-syntax-parser $+
  [(_ #:parsers ~! (parser ...) entry:parse-lookup-entry ...)
   #'(-$+ (hash-union #:combine (lambda (x y) y)
            (parser) ...
            (create-lookup-table (entry.op
                                  entry.prec
                                  entry.assoc
                                  entry.unary-prec
                                  entry.desc) ...)))]
  [(_ entry:parse-lookup-entry ...)
    #'(-$+ (create-lookup-table (entry.op
                                 entry.prec
                                 entry.assoc
                                 entry.unary-prec
                                 entry.desc) ...))])
