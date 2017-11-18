#lang racket/base

(provide default-parse-table)

(require "private/helper.rkt")

(define default-parse-table
  (create-lookup-table
    (or 0 left "boolean or")
    (and 1 left "boolean and")
    (bor 2 left "bitwise ior")
    (bxor 3 left "bitwise xor")
    (band 4 left "bitwise and")
    (= 5 left "equality comparison")
    (!= 5 left "inequality comparison")
    (>= 6 left "greater-than or equal comparison")
    (<= 6 left "less-than or equal comparison")
    (> 6 left "greater-than comparison")
    (< 6 left "less-than comparison")
    (<=> 7 left "three-way comparison")
    (<< 8 left "left binary shift")
    (>> 8 left "right binary shift")
    (+ 9 left "addition")
    (- 9 left "subtraction")
    (* 10 left "multiplication")
    (/ 10 left "division")
    (% 10 left "modulo")
    (^ 11 right "power")
    (not 11 right "boolean not")
    (bnot 11 right "bitwise not")
    (~ 0 separator "prevent parsing of subform")
    ))
