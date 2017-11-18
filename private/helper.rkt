#lang racket

(provide create-lookup-table drop-last-two entry? operate shunt take-last-two)


(require syntax/parse syntax/parse/define)

;; Append to the last list inside lst: so (append-last '((a) (b c)) 'd) => '((a) (b (c d)))
(define (append-last lst item)
  ; (dbug `(,lst ,item))
  (if (and (list? lst) (not (empty? lst)))
    (append (not-last lst) (list (append-last (last lst) item)))
    (list lst item))
  )

(define (append-last* lst item)
  (if (list? (last lst))
    (append (drop-last lst) (list (append (last lst) (list item))))
    (append (drop-last lst) (list (append (list (last lst)) (list item))))
  ))

(begin-for-syntax
  (define-syntax-class lr
    #:datum-literals (left right separator)
    (pattern (~or left right separator))))


(define-syntax-parser create-lookup-table
  [(_ (op:id prec:exact-integer assoc:lr (~optional desc:expr #:defaults ([desc #'"no description given"]))) ...)
   #'(make-immutable-hash '((op . (prec assoc desc)) ...))])

(define (drop-last lst)
  (match (length lst)
    [0 null]
    [else (take lst (- (length lst) 1))]))

(define (drop-last-two lst)
  (match (length lst)
    [0 null]
    [1 null]
    [else (take lst (- (length lst) 2))]))

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

(define (not-last lst)
  (if (>= (length lst) 1)
    (take lst (sub1 (length lst)))
    lst))

(define (operate op prec assoc outstack opstack lookup prevop?)
  (if prevop? ; Then this operator is unary, and we just push it to the stack
    (values outstack (cons op opstack) #f #t #f)
    (if (not (empty? opstack))
      (let* ([ref (hash-ref lookup (syntax-e (car opstack)))]
             [prec* (car ref)])
        (if (and (>= prec* prec) (symbol=? 'left assoc))
          (begin ; (displayln "higher operator found")
                 (operate op prec assoc
                          (append (drop-last-two outstack) (list (cons (car opstack) (take-last-two outstack))))
                          (cdr opstack)
                          lookup
                          prevop?))
          (begin ; (displayln "not higher operator")
                 (values outstack (cons op opstack) #t #f #f))))
      (begin
        ; (displayln "opstack is empty, we can only push it on there")
        (values outstack (cons op opstack) #t #f #f)))))

(define (shunt value outstack opstack prevop? prevunary? prevsepar?)
  ; (erro value outstack opstack)
  (cond
    [prevop? (values (append outstack (list value)) opstack #f #f #f)]
    [prevunary? (values (append outstack (list (list (car opstack) value))) (cdr opstack) #f #f #f)]
    [else (values (append-last* outstack value) opstack #f #f #f)]
    ))

(define (take-last-two lst)
  (match (length lst)
    [0 lst]
    [1 lst]
    [else (drop lst (- (length lst) 2))]))

