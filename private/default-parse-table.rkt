#lang racket/base

(provide default-parse-table)

(require "helper.rkt")

(define default-parse-table
  (create-lookup-table
    (or 0 left "boolean or")
    (and 10 left "boolean and")
    (bior 20 left "bitwise ior")
    (bxor 30 left "bitwise xor")
    (band 40 left "bitwise and")
    (= 50 left "equality comparison")
    (!= 50 left "inequality comparison")
    (>= 60 left "greater-than or equal comparison")
    (<= 60 left "less-than or equal comparison")
    (> 60 left "greater-than comparison")
    (< 60 left "less-than comparison")
    (<=> 70 left "three-way comparison")
    (<< 80 left "left binary shift")
    (>> 80 left "right binary shift")
    (+ 90 left 100 "addition")
    (- 90 left 100 "subtraction")
    (* 100 left 100 "multiplication")
    (/ 100 left 100 "division")
    (% 100 left 100 "modulo")
    (^ 110 right "power")
    (not 110 right "boolean not")
    (bnot 110 right "bitwise not")
    ; (~ 0 separator "prevent parsing of subform")
    ))
