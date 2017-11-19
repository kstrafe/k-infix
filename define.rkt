#lang racket/base

(provide define-$* define-$+)

(require syntax/parse/define
         (for-syntax racket/base
                     "custom.rkt"
                     "private/parse-lookup-entry.rkt"))

; TODO clean up this code, it should be macro-ified
(define-syntax-parser define-$*
  [(_ name:id #:parsers ~! (parser ...) entry:parse-lookup-entry ...)
    #'(define-syntax (name stx) (($* #:parsers (parser ...)
                                     (entry.op
                                      entry.prec
                                      entry.assoc
                                      entry.unary-prec) ...)
                                 (datum->syntax stx (cdr (syntax-e stx)))))]
  [(_ name:id entry:parse-lookup-entry ...)
    #'(define-syntax (name stx) (($* (entry.op
                                      entry.prec
                                      entry.assoc
                                      entry.unary-prec) ...)
                                 (datum->syntax stx (cdr (syntax-e stx)))))])

(define-syntax-parser define-$+
  [(_ name:id #:parsers ~! (parser ...) entry:parse-lookup-entry ...)
    #'(define-syntax (name stx) (($+ #:parsers (parser ...)
                                     (entry.op
                                      entry.prec
                                      entry.assoc
                                      entry.unary-prec) ...)
                                 (datum->syntax stx (cdr (syntax-e stx)))))]
  [(_ name:id entry:parse-lookup-entry ...)
    #'(define-syntax (name stx) (($+ (entry.op
                                      entry.prec
                                      entry.assoc
                                      entry.unary-prec) ...)
                                 (datum->syntax stx (cdr (syntax-e stx)))))])
