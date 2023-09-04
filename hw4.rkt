;; Chloe Hu
;; xinyuh22

#lang racket
(provide (all-defined-out))

;; Q1
(define (red-blue)
  (letrec ([red (lambda () (cons "blue" blue))]
           [blue (lambda () (cons "red" red))])
    (cons "red" (lambda () (red)))))

;; Q2
(define (take st n)
  (if (zero? n)
      '()
      (cons (car (st))
            (take (cdr (st)) (sub1 n)))))
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Q3
(define (combm n k)
  (letrec ([memo '()]
           [f (lambda (n k)
                (letrec ([fact (lambda (n)
                                 (if (< n 2)
                                     1
                                     (* n (fact (- n 1)))))]
                         [ans (assoc '(n k) memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= k 0) (= k n))
                                         1
                                         (/ (fact n) (* (fact k) (fact (- n k)))))])
                        (begin
                          (set! memo (cons (cons '(n k) new-ans) memo))
                          new-ans)))))])
    (f n k)))