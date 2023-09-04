;; Chloe Hu
;; xinyuh22
#lang racket
(provide (all-defined-out))

;(define (diff-sum x E)
(define (diff-sum x E)
  (define (iter E acc)
    (cond [(null? E) acc]
          [else (iter (cdr E) (list '+ acc (diff x (car E))))]))
  (iter E 0))
  
; (* x y)
(define (diff-product x E)
  (let ((E1 (car E))
        (E2 (cadr E)))
     (list '+
        (list '* E1 (diff x E2))
        (list '* E2 (diff x E1)))))

; (expt x y)
(define (diff-expt x E)
  (let ((base (car E))
       (power (cadr E)))
    (cond [(equal? power 0) 0]
          [(false? (equal? base x)) 0]
          [else (list '* power (list 'expt base (- power 1)))])))

;; derivative of constant
(define (diff-constant x E) 0)

;; derivative of a simple variable
(define (diff-variable x E)
  (if (eq? x E)
      1
      0))

;; Dispatch Table of supported operators.
 (define diff-dispatch
   (list (list '+ diff-sum)
         (list '* diff-product)
         (list 'expt diff-expt)
         ))

;; extract operator from the expression
(define (get-operator E)
  (if (null? E)
      '()
      (car E)))

;; extract operand from the expression
(define (get-operand E)
  (if (null? E)
      '()
      (cdr E)))

;; Differentiate expression E w.r.t. x.
(define (diff x E)
  (cond [(number? E) (diff-constant x E)]
        [(equal? E x) 1]
        [(symbol? E) (diff-variable x E)]
        [else
           (let ([operator (get-operator E)]
                 [operand (get-operand E)])
             (let ([diff-func (assoc operator diff-dispatch)])
                (if (or (false? diff-func) (null? operand))
                    '()
                    (apply (cadr diff-func) x (list operand)))))]))