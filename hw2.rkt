;; Chloe Hu
;; xinyuh22
#lang racket
(provide (all-defined-out))

;; Part II
;; Question 1
;; length function: calculate the length of the list
;; version 1
(define (lengtht lst)
  (len lst 0))

;; helper function
(define (len lst acc)
  (if (null? lst)
      acc
      (len (cdr lst) (+ acc 1))))

;; version 2
(define (lengtht2 lst)
  (letrec ([aux (lambda (lst acc)
                  (if (null? lst)
                      acc
                      (aux (cdr lst) (+ acc 1))))])
    (aux lst 0)))

;; Question 2
;; (poly x coeff)
;; compute the value of polynomial given coefficients
(define (poly x coeff)
  (letrec ([aux (lambda (coeff acc n)
                  (if (null? coeff)
                      acc
                      (aux (cdr coeff) (+ acc (* (car coeff) (expt x n))) (+ n 1))))])
    (aux coeff 0 0)))

;; Question 3
;; apply a list of function to a number
(define (apply-all funcs num)
  (define (apply func)
    (func num))
  (map apply funcs))

;; Question 4
;; test if all elements satisfy the given predicate
(define (all-are pred)
  (lambda (lst)
    (cond [(null? lst) #t]
          [(equal? (pred (car lst)) #f) #f]
          [else ((all-are pred) (cdr lst))])))

;; Part III
;; Question 1
;; construct an expression tree
(define (make-expr left-op operator right-op)
  (list left-op operator right-op))

;; extract operator
(define (operator lst)
  (if (null? lst)
      lst
      (cadr lst)))

;; extract left operand
(define (left-op lst)
  (if (null? lst)
      lst
      (car lst)))

;; extract right operand
(define (right-op lst)
  (if (null? lst)
      lst
      (caddr lst)))

;; Question 2
;; preorder traversal of the expr-tree
(define (preorder expr-tree)
  (cond [(null? expr-tree) expr-tree]
        [(number? expr-tree) (list expr-tree)]
        [else (append (list (operator expr-tree))
              (preorder (left-op expr-tree))
              (preorder (right-op expr-tree)))]))

;; inorder traversal of the expr-tree
(define (inorder expr-tree)
  (cond [(null? expr-tree) expr-tree]
        [(number? expr-tree) (list expr-tree)]
        [else (append (inorder (left-op expr-tree))
              (list (operator expr-tree))
              (inorder (right-op expr-tree)))]))

;; postorder traversal of the expr-tree
(define (postorder expr-tree)
  (cond [(null? expr-tree) '()]
        [(number? expr-tree) (list expr-tree)]
        [else (append (postorder (left-op expr-tree))
                  (postorder (right-op expr-tree))
                  (list (operator expr-tree)))]))

;; Question 3
;; evaluate the value of the expr-tree
(define (eval-tree expr-tree)
  (cond [(null? expr-tree) 0]
        [(number? expr-tree) expr-tree]
        [(equal? (operator expr-tree) '+) (+ (eval-left expr-tree) (eval-right expr-tree))]
        [(equal? (operator expr-tree) '-) (- (eval-left expr-tree) (eval-right expr-tree))]
        [(equal? (operator expr-tree) '*) (* (eval-left expr-tree) (eval-right expr-tree))]
        [(equal? (operator expr-tree) '/) (/ (eval-left expr-tree) (eval-right expr-tree))]))

;; below are two helper function
(define (eval-left expr-tree)
  (eval-tree (left-op expr-tree)))

(define (eval-right expr-tree)
  (eval-tree (right-op expr-tree)))

;; Question 4
;; map all the leave nodes of the expr-tree with the given function
(define (map-leaves f expr-tree)
  (cond
    ((null? expr-tree) '())
    ((number? expr-tree) (f expr-tree))
    (else (make-expr (map-leaves f (left-op expr-tree))
                     (operator expr-tree)
                     (map-leaves f (right-op expr-tree))))))
