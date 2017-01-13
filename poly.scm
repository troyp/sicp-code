;#lang scheme

;; We define a polynomial as a variable and a set of terms.

;; A variable is just a symbol, so we can use these defns from sect.2.3.2...
(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (same-variable? x)
       (same-variable? y)
       (eq? x y)))

