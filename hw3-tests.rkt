;; Chloe Hu
;; xinyuh22
#lang racket
(provide (all-defined-out))

(require rackunit)
(require "hw3.rkt")

;; test for null expression
(check-equal? (diff 'x '()) '())

;; test for derivatives on zero
(check-equal? (diff 'x 0) 0)

;; test for expression with incomplete operator or operand
(check-equal? (diff 'x '(+)) '() "derivatives of expression with missing operand")
(check-equal? (diff 'x '(x 4)) '() "derivatives of expression with missing operator")

;; test for constant
(check-equal? (diff 'x 4) 0 "derivatives of constant should be zero")
(check-equal? (diff 'x 20) 0 "derivatives of constant should be zero")

;; test for simple variables
(check-equal? (diff 'x 'x) 1 "derivatives of same variable")
(check-equal? (diff 'x '(* 2 x)) '(+ (* 2 1) (* x 0))
              "derivatives of same variable times some constant")
(check-equal? (diff 'y '(* 2 x)) '(+ (* 2 0) (* x 0))
              "derivatives of different variable")
(check-equal? (diff 'x '(* 2 y)) '(+ (* 2 0) (* y 0))
              "derivatives of different variable")

;; test for sum of an arbitrary number of terms
(check-equal? (diff 'x '(+ (* 2 x) 4)) '(+ (+ 0 (+ (* 2 1) (* x 0))) 0)
              "derivatives of the sum of 2: variable and constant with nested product")
(check-equal? (diff 'x '(+ x (* x x))) '(+ (+ 0 1) (+ (* x 1) (* x 1)))
              "derivatives of the sum of two variables with nested product")
(check-equal? (diff 'x '(+ (* 3 x) (* 4 y) (* 6 (expt x 3)))) '(+ (+ (+ 0 (+ (* 3 1) (* x 0))) (+ (* 4 0) (* y 0))) (+ (* 6 (* 3 (expt x 2))) (* (expt x 3) 0)))
              "derivatives of the sum of 3: elements with nested product and exponent")

;; test for derivatives of product of two terms
(check-equal? (diff 'x '(* x x)) '(+ (* x 1) (* x 1))
              "derivatives of product of two terms with no nested sublist")
(check-equal? (diff 'x '(* (expt x 4) x)) '(+ (* (expt x 4) 1) (* x (* 4 (expt x 3))))
              "derivatives of product of two terms with nested exponent")
(check-equal? (diff 'x '(* (+ (* 4 x) 3) x)) '(+ (* (+ (* 4 x) 3) 1) (* x (+ (+ 0 (+ (* 4 1) (* x 0))) 0)))
              "derivatives of product of two terms with nested sum")
(check-equal? (diff 'x '(* (* x x) x)) '(+ (* (* x x) 1) (* x (+ (* x 1) (* x 1))))
              "derivatives of product of two terms with nested product")

;; test for exponent
(check-equal? (diff 'x '(expt x 4)) '(* 4 (expt x 3))
              "derivatives of single exponent term")
(check-equal? (diff 'x '(expt x 0)) 0
              "derivatives of exponent with power of zero")
(check-equal? (diff 'x '(expt x -2)) '(* -2 (expt x -3))
              "derivatives of exponent with negative power")
