#lang racket/base

(provide $* $+ create-lookup-table parse-lookup-table?)

(require syntax/parse syntax/parse/define racket/bool racket/hash
         (for-syntax racket/base)
         "default-parse-table.rkt"
         "private/helper.rkt")

(define ($* plt)
  (lambda (stx)
    (syntax-parse stx
      [(_) (datum->syntax stx plt)]
      [(_ terms ...+)
       #:with anything
       (let ([l plt])
         (let-values ([(outstack opstack prevop? prevunary? prevsepar?)
               (for/fold ([outstack   null]
                          [opstack    null]
                          [prevop?    #t]
                          [prevunary? #f]
                          [prevsepar? #f])
                         ([item (attribute terms)])
                 ; (trce prevunary?)
                 ; (dbug outstack opstack)
                 (let ([open (syntax-e item)])
                   (cond
                     [(list? open)
                      ; (displayln "interesting, let's put a $ in front of it")
                      (if prevsepar?
                        (shunt item outstack opstack prevop? prevunary? #f)
                        (shunt (datum->syntax stx `($ ,@open)) outstack opstack prevop? prevunary? prevsepar?))]
                     [(hash-has-key? l open)
                       (let* ([ref (hash-ref l open)]
                              [prec (car ref)]
                              [assoc (cadr ref)])
                         ; (displayln `("calling operate with" ,open))
                         ; discriminating type
                         (if (symbol=? assoc 'separator)
                           (begin ; (displayln "is separator")
                                  ; Push the remaining exprs into the last element
                                  (values outstack opstack #t #f #t)
                                  )
                           (begin ; (displayln "not separator")
                                  (operate item prec assoc outstack opstack l prevop?))))]
                     [else
                       ; (crit "shunting")
                       (shunt item outstack opstack prevop? prevunary? prevsepar?)]))
                  )])
           ; (warn* `("fout:" ,outstack))
           ; (warn* `("fops:" ,opstack))
             (car (for/fold ([outstack* outstack])
                            ([item opstack])
                 ; (trce item)
                 ; (info (drop-last-two outstack*))
                 ; (info (append (take-last-two outstack*)))
                 ;(list (cons item (take-last-two outstack*)))))
                 (append (drop-last-two outstack*) (list (cons item (take-last-two outstack*))))
               ))))

       ; (trce (attribute anything))
       (datum->syntax stx (syntax->datum #'anything))])))

(define ($+ plt)
  ($* (hash-union default-parse-table plt
                  #:combine (lambda (x y) y))))

(define (parse-lookup-table? table)
  (and
    (andmap symbol? (hash-keys table))
    (andmap entry? (hash-values table))))

