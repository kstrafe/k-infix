#lang racket

(provide create-lookup-table entry? parse)

(require syntax/parse syntax/parse/define "logger.rkt")

(module+ test (require rackunit syntax/parse/define))

; Append to the last element inside a list, if said element is not a list, turn it into one.
; If the input list is empty then add an empty list into it.
(define (append-last lst item)
  (if (empty? lst)
    (list (list item))
    (if (list? (last lst))
      (append (drop-right lst 1) (list (append (last lst) (list item))))
      (append (drop-right lst 1) (list (append (list (last lst)) (list item)))))))

(module+ test
  (define-syntax-parser checker
    #:datum-literals (:)
    [(_ checker:expr fn:expr (result:expr : arg:expr ...) ...)
     #'(begin
        (checker (fn arg ...) result) ...)])
  (checker check-equal? append-last
    ['((a))       : '() 'a]
    ['((()))      : '() '()]
    ['((() a))    : '((())) 'a]
    ['((a))       : '(()) 'a]
    ['((a b))     : '((a)) 'b]
    ['((a b) (c)) : '((a b) ()) 'c]))

(begin-for-syntax
  (define-syntax-class lr
    #:datum-literals (left right)
    (pattern (~or left right))))

(define-syntax-parser create-lookup-table
  [(_ (op:id prec:exact-integer assoc:lr
             (~optional unary-prec:exact-integer #:defaults ([unary-prec #'prec]))
             (~optional desc:expr #:defaults ([desc #'"no description given"]))) ...)
   #'(make-immutable-hash '((op . (prec assoc unary-prec unary-assoc desc)) ...))])

(define (entry? lst)
  (and
    (list? lst)
    (or
      (= (length lst) 3)
      (= (length lst) 2))
    (and
      (number? (car lst))
      (not (negative? (car lst))))
    (or
      (symbol=? 'left (cadr lst))
      (symbol=? 'right (cadr lst)))
    (if (= (length lst) 3)
      (string? (caddr lst))
      #t)))

(define-syntax (for/fold-let stx)
  (syntax-parse stx
    [(_ ([entry value] ...) others ... finalizer)
     #'(let-values ([(entry ...)
          (for/fold ([entry value] ...)
                    others ...)])
          finalizer)]))

(define (parse stx terms plt)
  (for/fold-let
            ([outstack null]
             [opstack  null]
             [unary?   #t]
             [escape?  #f])
            ([item terms])
    (let* ([content (syntax-e item)])
      (trce outstack opstack unary? escape? content)
      (info "==========")
      (cond
        [(list? content)
         (express content item outstack opstack unary? escape? stx content plt)]
        [(and (symbol? content) (symbol=? content '~))
         (values outstack opstack unary? #t)]
        [(hash-has-key? plt content)
         (operate content item outstack opstack unary? escape? plt)]
        [else (shunt content item outstack opstack unary? escape?)]
      )
    )
    (begin
      (warn outstack opstack)
      (for/fold-let ([out outstack])
                    ([ops opstack])
        (if (cadr ops)
          (unary-out out (car ops))
          (binary-out out (car ops)))
        (begin
          (dbug out)
          (datum->syntax stx (car out)))
      ))))

(define (drop-right* lst n)
  (if (> n (length lst))
    '()
    (drop-right lst n)))

(define (take-right* lst n)
  (if (> n (length lst))
    lst
    (take-right lst n)))

(define (unary-out lst op)
  (crit lst)
  (append (drop-right* lst 1) (list (cons op (take-right* lst 1)))))

(define (binary-out lst op)
  (erro lst)
  (append (drop-right* lst 2) (list (cons op (take-right* lst 2)))))

(define (express val item out ops unary? escape? stx terms plt)
  (if escape?
    (shunt val item out ops unary? escape?)
    (shunt val (parse stx terms plt) out ops unary? escape?)))

; Process an operator
(define (operate val item out ops unary? escape? plt)
  (if (or (empty? ops))
    (values out (cons (list item unary?) ops) #t #f)
    (let* ([val*       (hash-ref plt val)]
           [val-unary? unary?]
           [val-prec   (car val*)]
           [val-precu  (caddr val*)]
           [val-assoc  (cadr val*)]
           [top+       (car ops)]
           [top*       (car top+)]
           [top        (hash-ref plt (syntax-e (car top+)))]
           [top-unary? (cadr top+)]
           [top-prec   (car top)]
           [top-precu  (caddr top)]
           [top-assoc  (cadr top)])
      (trce top-precu val-prec)
      (if (and (< top-precu val-precu) val-unary?)
        (operate val item (unary-out out top*) (cdr ops) #f escape? plt)
        (if (and (not unary?) (>= top-prec val-prec) (symbol=? val-assoc 'left))
          (operate val item (if top-unary? (unary-out out top*) (binary-out out top*)) (cdr ops) #f escape? plt)
          (values out (cons (list item unary?) ops) #t #f))))))

; Process a non-operator
(define (shunt value item out ops unary? escape?)
  (values
    (if unary?
      (append out (list item))
      (append-last out item))
    ops #f #f))
