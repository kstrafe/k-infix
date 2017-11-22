#lang racket

(provide create-lookup-table entry? parse)

(require syntax/parse syntax/parse/define)

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
   #'(make-immutable-hash '((op . (prec assoc unary-prec desc)) ...))])

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
             [prevop?   #t]
             [escape?  #f])
            ([item terms])
    (define content (syntax-e item))
    (cond
      [(list? content)
       (express content item outstack opstack prevop? escape? stx content plt)]
      [(and (symbol? content) (symbol=? content '~))
       (values outstack opstack prevop? #t)]
      [(hash-has-key? plt content)
       (operate content item outstack opstack prevop? escape? plt)]
      [else (shunt content item outstack opstack prevop? escape?)]
    )
    (begin
      (for/fold-let ([out outstack])
                    ([ops opstack])
        (if (cadr ops)
          (unary-out out (car ops))
          (binary-out out (car ops)))
        (begin
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
  (append (drop-right* lst 1) (list (cons op (take-right* lst 1)))))

(define (binary-out lst op)
  (append (drop-right* lst 2) (list (cons op (take-right* lst 2)))))

(define (express val item out ops prevop? escape? stx terms plt)
  (if escape?
    (shunt val item out ops prevop? escape?)
    (shunt val (parse stx terms plt) out ops prevop? escape?)))

; Process an operator
(define (operate val item out ops prevop? escape? plt)
  (if (or (empty? ops))
    (values out (cons (list item prevop?) ops) #t #f)
    (let* ([val*       (hash-ref plt val)]
           [val-prevop? prevop?]
           [val-prec   (car val*)]
           [val-precu  (caddr val*)]
           [val-assoc  (cadr val*)]
           [top+       (car ops)]
           [top*       (car top+)]
           [top        (hash-ref plt (syntax-e (car top+)))]
           [top-prevop? (cadr top+)]
           [top-prec   (car top)]
           [top-precu  (caddr top)]
           [top-assoc  (cadr top)])
      (if (and (< top-precu val-precu) val-prevop?)
        (operate val item (unary-out out top*) (cdr ops) #f escape? plt)
        (if (and (not prevop?) (>= top-prec val-prec) (symbol=? val-assoc 'left))
          (operate val item (if top-prevop? (unary-out out top*) (binary-out out top*)) (cdr ops) #f escape? plt)
          (values out (cons (list item prevop?) ops) #t #f))))))

; Process a non-operator
(define (shunt value item out ops prevop? escape?)
  (values
    (if prevop?
      (append out (list item))
      (append-last out item))
    ops #f #f))
