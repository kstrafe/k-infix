#lang racket

;;;; Provides loggers to standard error ports. The loggers automatically write the expressions and their results.
;;;; This means executing `(trce (+ 1 2 3))` writes `(trce (+ 1 2 3) = 6)`.
;;;; # Loggers #
;;;; There are 7 loggers: trce (trace), dbug (debug), info (info), warn (warning), erro (error), crit (critical), ftal (fatal).
;;;; # Appendages #
;;;; Appending a star (*) to the logger name prevents writing the expression: `(trce* (+ 1 2 3))` writes `(trce* _ = 6)`.
;;;; Appending a caret (^) to the logger name prevents writing the expression: and includes a timestamp and source location:  `(trce^ (+ 1 2 3))` writes `(trce^ 2017-11-10T07:59:09 ("/home/me/archive.rkt" 170 9)) _ = 6)`.
;;;; Appending a plus (+) to the logger name and includes a timestamp and source location:  `(trce+ (+ 1 2 3))` writes `(trce^ 2017-11-10T07:59:09 ("/home/me/archive.rkt" 170 9)) (+ 1 2 3) = 6)`.

;;;; # Color #
;;;; Change the color by using (logger-color arg), arg : (or/c 'automatic #f #t). Automatic checks if a tty is attached.

;;;; The output can be read using `read`. It is printed using `pretty-write`. The datetime format is in
;;;; iso-8601.

(provide logger-color
         trce  dbug  info  warn  erro  crit  ftal
         trce* dbug* info* warn* erro* crit* ftal*
         trce^ dbug^ info^ warn^ erro^ crit^ ftal^
         trce+ dbug+ info+ warn+ erro+ crit+ ftal+)

(require racket/date racket/syntax (for-syntax racket/syntax syntax/parse))

(define use-color? 'automatic)
(define logger-color (case-lambda
                       [() use-color?]
                       [(x) (set! use-color? x)]))

(define enabled? #f)

(define (color-print sym str)
  (display
    (string-append
      (if use-color?
        (match sym
          ['trce "\e[0;35m"]
          ['dbug "\e[0;36m"]
          ['info "\e[0;32m"]
          ['warn "\e[0;33m"]
          ['erro "\e[0;31m"]
          ['crit "\e[1;31m"]
          ['ftal "\e[43m\e[1;91m"]
          [else ""])
        "")
      str
      (if use-color? "\e[0m" ""))
    (current-error-port))
  (flush-output (current-error-port)))

(define (get-time)
  (let ([old-format (date-display-format)])
    (date-display-format 'iso-8601)
    (let ([return (string->symbol (date->string (current-date) #t))])
      (date-display-format old-format)
      return)))

(define (get-source stx)
  (match (syntax-source stx)
    ('readline-input 'terminal)
    (#f 'unknown)  ; #f in the case of running racket -e
    (final (if (path? final) (path->string final) 'unknown))))

(define-syntax (base stx)
  (syntax-parse stx
    [(_ basename:id name:id)
      #'(begin
        (when enabled?
          (let ([out (open-output-string)])
            (pretty-write `(name ,(get-time) (,(get-source #'name) ,(syntax-line #'name) ,(syntax-column #'name))) out)
            (color-print 'basename (get-output-string out)))))]
    [(_ basename:id name:id expr:expr ...+)
      #'(begin
        (when enabled?
          (let ([result expr]
                [out (open-output-string)])
            (pretty-write `(name expr = ,result) out)
            (color-print 'basename (get-output-string out))
            result) ...))]))

(define-syntax (base* stx)
  (syntax-parse stx
    [(_ basename:id name:id expr:expr)
      #'(begin
        (when enabled?
          (let ([result expr]
                [out (open-output-string)])
            (pretty-write `(name _ = ,result) out)
            (color-print 'basename (get-output-string out))
            result)))]))

(define-syntax (base^ stx)
  (syntax-parse stx
    [(_ basename:id name:id expr:expr)
      #'(begin
        (when enabled?
          (let ([result expr]
                [out (open-output-string)])
            (pretty-write `(name ,(get-time) (,(get-source #'expr) ,(syntax-line #'expr) ,(syntax-column #'expr)) _ = ,result) out)
            (color-print 'basename (get-output-string out))
            result)))]))

(define-syntax (base+ stx)
  (syntax-parse stx
    [(_ basename:id name:id expr:expr)
      #'(begin
        (when enabled?
          (let ([result expr]
                [out (open-output-string)])
            (pretty-write `(name ,(get-time) (,(get-source #'expr) ,(syntax-line #'expr) ,(syntax-column #'expr)) expr = ,result) out)
            (color-print 'basename (get-output-string out))
            result)))]))

(define-syntax (make-loggers stx)
  (syntax-parse stx
    [(_ name:id ...+)
      (let ([loggers (syntax-e #'(name ...))])
        (with-syntax* ([(no-expr ...) (for/list ([name-e (syntax-e #'(name ...))]) (format-id stx "~a*" name-e))]
                       [(date/src-no-expr ...) (for/list ([name-e (syntax-e #'(name ...))]) (format-id stx "~a^" name-e))]
                       [(verbose ...) (for/list ([name-e (syntax-e #'(name ...))]) (format-id stx "~a+" name-e))])
          (syntax-protect
            #'(begin
              (begin
                (define-syntax (name (... stx))
                  (syntax-parse (... stx)
                    (... [(call:id whatever:expr ...)
                      #'(base call name whatever ...)])))

                (define-syntax (no-expr (... stx))
                  (syntax-parse (... stx)
                    (... [(call:id whatever:expr ...)
                      (with-syntax ([caller-id (datum->syntax #'call 'name #'call)])
                        #'(base* caller-id no-expr whatever ...))])))

                (define-syntax (date/src-no-expr (... stx))
                  (syntax-parse (... stx)
                    (... [(call:id whatever:expr ...)
                      (with-syntax ([caller-id (datum->syntax #'call 'name #'call)])
                        #'(base^ caller-id date/src-no-expr whatever ...))])))

                (define-syntax (verbose (... stx))
                  (syntax-parse (... stx)
                    (... [(call:id whatever:expr ...)
                      (with-syntax ([caller-id (datum->syntax #'call 'name #'call)])
                        #'(base+ caller-id verbose whatever ...))])))) ...))))]))

(make-loggers trce dbug info warn erro crit ftal)
