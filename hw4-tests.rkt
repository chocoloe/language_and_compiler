;; Chloe Hu
;; xinyuh22

#lang racket
(provide (all-defined-out))

(require rackunit)
(require "hw4.rkt")

;; tests for q1
;; (check-equal? red-blue #<procedure:red-blue>)
(check-equal? (car (red-blue)) "red")
(check-equal? (car ((cdr (red-blue)))) "blue")
(check-equal? (car ((cdr ((cdr (red-blue)))))) "red")
(check-equal? (car ((cdr ((cdr ((cdr (red-blue)))))))) "blue")

;; tests for q2
;; below are two functions that will be used to help test the take function
; infinite stream of 1's
(define ones (lambda () (cons 1 ones)))

; natural numbers
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(check-equal? (take nats 4) '(1 2 3 4))
(check-equal? (take nats 0) '())
(check-equal? (take nats 6) '(1 2 3 4 5 6))
(check-equal? (take ones 0) '())
(check-equal? (take ones 5) '(1 1 1 1 1))
(check-equal? (take red-blue 6) '("red" "blue" "red" "blue" "red" "blue"))


;; tests for q3
(check-equal? (combm 1 0) 1)
(check-equal? (combm 0 0) 1)
(check-equal? (combm 1 1) 1)
(check-equal? (combm 4 2) 6)
