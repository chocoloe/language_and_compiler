;; Name: Chloe Hu
;; Netid: xinyuh22
#lang racket
(provide (all-defined-out))

;; Question 1
;; comb (n k)
;;calculate number of combinations
(define (comb n k)
    (if (or (= k 0) (= k n))
        1
        (/ (fact n) (* (fact k) (fact (- n k))))))

;; helper function
(define (fact n)
    (if (< n 2)
        1
        (* n (fact (- n 1)))))

;; Question 2
;; zip
;; return two lists that have alternating values from the original list
(define (zip list1 list2)
  (cond [(null? list1) list2]
        [(null? list2) list1]
        [else (cons (car list1) (cons (car list2) (zip (cdr list1) (cdr list2))))]))

;; Question 3
;; unzip function
;; reverse step of zip
(define (unzip lst)
  (makelist lst '() '()))

;; helper function
(define (makelist lst lst1 lst2)
  (cond [(null? lst) (list (reverse lst1) (reverse lst2))]
        [(null? (cdr lst)) (list (cons (car lst) lst1) lst2)]
        [else (makelist (cddr lst) (cons (car lst) lst1) (cons (car (cdr lst)) lst2))]))

;; Question 4
;; expand function
;; expand copies of an element in the list
(define (expand lst)
  (cond [(null? lst) '()]
        [(not (list? (car lst))) (cons (car lst) (expand (cdr lst)))]
        [else (append (expander (car lst)) (expand (cdr lst)))]))

;; helper function
(define (expander lst)
  (cond [(= (car lst) 0) '()]
        [else (append (cdr lst) (expander (cons (- (car lst) 1) (cdr lst))))]))

;; Question 5
;; (a)
;; value function: get the value of the node
(define (value node)
  (if (null? node)
      '()
      (car node)))

;; left function: get the left subtree of the node
(define (left node)
  (if (null? node)
      '()
      (cadr node)))

;; right function: get the right subtree of the node
(define (right node)
  (if (null? node)
      '()
      (caddr node)))

;; (b)
;; size function: get the size of the tree
(define (size tree)
  (if (null? tree)
      0
      (+ 1 (size (left tree)) (size (right tree)))))

;; (c)
;; contains function: check if an element exists in the tree
(define (contains item tree)
  (cond [(null? tree) #f]
        [(equal? item (value tree)) #t]
        [else (or (contains item (left tree)) (contains item (right tree)))]))

;; (d)
;; leaves function: calculate the number of leaves in the tree
(define (leaves tree)
  (cond [(null? tree) '()]
        [(null? (and (left tree) (right tree))) (cons (value tree) '())]
        [else (append (leaves (left tree)) (leaves (right tree)))]))

;; (e)
;; isBST function: check if the tree is a valid BST
(define (isBST tree)
  (BST tree -inf.0 +inf.0))

;; helper function
(define (BST tree min max)
  (cond [(null? tree) #t]
        [(or (not (< min (value tree))) (not (> max (value tree)))) #f]
        [else (and (BST (left tree) min (value tree)) (BST (right tree) (value tree) max))]))
