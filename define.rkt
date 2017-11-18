#lang racket/base

(provide define-$* define-$+)

(require syntax/parse/define
         (for-syntax racket/base "custom.rkt"))

(define-syntax-parser define-$*
  [(_ name:id (args:expr ...+) ...)
    #'(define-syntax (name stx) (($* (create-lookup-table (args ...) ...)) stx))])

(define-syntax-parser define-$+
  [(_ name:id (args:expr ...+) ...)
    #'(define-syntax (name stx) (($+ (create-lookup-table (args ...) ...)) stx))])

