#lang racket

(provide create-lookup-table entry? operate shunt)

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

; Append to the right-most, deepest list inside a tree (nested lists)
(define (append-deep-right lst item)
  (if (list? lst)
    (if (empty? lst)
      (list item)
      (if (list? (last lst))
        (append (drop-right lst 1) (list (append-deep-right (last lst) item)))
        (append lst (list item))))
    (error "append-deep-right: not a list")))

(module+ test
  (checker check-equal? append-deep-right
    ['(a)         : '()      'a]
    ['(a b)       : '(a)     'b]
    ['(a (b))     : '(a ())  'b]
    ['(a (b c))   : '(a (b)) 'c]))

(begin-for-syntax
  (define-syntax-class lr
    #:datum-literals (left right separator)
    (pattern (~or left right separator))))

(define-syntax-parser create-lookup-table
  [(_ (op:id prec:exact-integer assoc:lr (~optional desc:expr #:defaults ([desc #'"no description given"]))) ...)
   #'(make-immutable-hash '((op . (prec assoc desc)) ...))])

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

; Process an operator
(define (operate op prec assoc outstack opstack lookup prevop? postunary? prevunary? prevsepar?)
  (let ([c #'(lambda x
              (if (procedure? (car x))
                (apply (car x) (cdr x))
                (if (= (length x) 1)
                  (car x)
                  (error "non-procedures can not have arguments"))))])
    (if prevop?
      (values (append-deep-right (append-last (append outstack '(())) op)
                                 (list c))
              opstack #f #t #t #f)
      (if (and prevunary? postunary?)
        (values (append-deep-right (append-deep-right outstack op) (list c)) opstack #f #t #t #f)
        (if (not (empty? opstack))
          (let* ([ref (hash-ref lookup (syntax-e (car opstack)))]
                 [prec* (car ref)])
            (if (and (>= prec* prec) (symbol=? 'left assoc))
              (operate op prec assoc
                       (append (drop-right outstack 2) (list (cons (car opstack) (take-right outstack 2))))
                       (cdr opstack)
                       lookup
                       prevop?
                       prevunary?
                       postunary?
                       prevsepar?)
              (values outstack (cons op opstack) #t #f #f #f)))
          (values outstack (cons op opstack) #t #f #f #f))))))

; Process a non-operator
(define (shunt value outstack opstack prevop? prevunary? prevsepar?)
  (cond
    [prevunary? (values (append-deep-right outstack value) opstack #f #t #f #f)]
    [prevop?    (values (append outstack (list value))     opstack #f #f #f #f)]
    [else       (values (append-last outstack value)       opstack #f #f #f #f)]))
