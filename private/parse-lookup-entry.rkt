#lang racket/base

(provide parse-lookup-entry)

(require syntax/parse)

(define-syntax-class associativity
  #:datum-literals (left right)
  (pattern (~or left right)))

(define-syntax-class parse-lookup-entry
  (pattern (op:id prec:exact-integer assoc:associativity
                  (~optional unary-prec:exact-integer #:defaults ([unary-prec #'prec]))
                  (~optional desc:string #:defaults ([desc #'""])))))
