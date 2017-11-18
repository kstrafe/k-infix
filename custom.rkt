#lang racket/base

(provide $* $+ create-lookup-table parse-lookup-table?)

(require syntax/parse syntax/parse/define
         racket/bool racket/list racket/hash
         (for-syntax racket/base)
         "default-parse-table.rkt"
         "private/helper.rkt"
         "private/logger.rkt")

(define (drop-right* lst n)
  (if (> n (length lst))
    '()
    (drop-right lst n)))

(define (take-right* lst n)
  (if (> n (length lst))
    lst
    (take-right lst n)))

(define ($* plt)
  (lambda (stx)
    (syntax-parse stx
      [(_) (datum->syntax stx plt)]
      [(_ terms ...+)
       #:with anything
       (let-values
         ([(outstack opstack prevop? prevunary? postunary? prevsepar?)
           (for/fold ([outstack   null]
                      [opstack    null]
                      [prevop?    #t]   ; Has there been an operator before us? Start with #t, so all first operators become unary
                      [prevunary? #f]   ; Are we in unary-mode? We're parsing an fn-call or value coming after a unary
                      [postunary? #f]   ; Used to indicate that the initial unary list is created, so we can just append
                      [prevsepar? #f])  ; Did we encounter a ~? If so, store it here, then we know to protect the next parse item
                     ([item (attribute terms)])
             (trce prevop? prevunary? prevsepar?)
             (dbug outstack opstack)
             (let ([open (syntax-e item)])
               (when (and prevsepar? (not (list? open)))
                 (raise-syntax-error 'k-infix "Unable to apply ~ to non-list item: " item))
               (cond
                 [(list? open)
                  (if prevsepar?
                    (shunt item outstack opstack prevop? prevunary? #f)
                    (shunt (datum->syntax stx `($ ,@open)) outstack opstack prevop? prevunary? prevsepar?))]
                 [(hash-has-key? plt open)
                   (let* ([ref (hash-ref plt open)]
                          [prec (car ref)]
                          [assoc (cadr ref)])
                     (if (symbol=? assoc 'separator)
                       (begin
                              (values outstack opstack #t prevunary? #f #t))
                       (begin
                              (operate item prec assoc outstack opstack plt prevop? prevunary? postunary? prevsepar?))))]
                 [else
                   (crit "shunting")
                   (shunt item outstack opstack prevop? prevunary? prevsepar?)]))
              )])
         ; Pop consecutive elements from the operator stack
         (car (for/fold ([outstack* outstack])
                        ([item opstack])
             (trce "Pero")
             (append (drop-right* outstack* 2)
                     (list (cons item (take-right* outstack* 2)))))))
       (crit (syntax->datum (attribute anything)))
       (datum->syntax stx (syntax->datum #'anything))])))

(define ($+ plt)
  ($* (hash-union default-parse-table plt
                  #:combine (lambda (x y) y))))

(define (parse-lookup-table? table)
  (and
    (andmap symbol? (hash-keys table))
    (andmap entry? (hash-values table))))

