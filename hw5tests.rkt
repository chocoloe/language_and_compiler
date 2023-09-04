;; Chloe Hu
;; xinyuh22
;; CSE413 23sp, Programming Languages & Implementation
;; Homework 5 tests

#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework.

; Note we have provided [only] 3 tests, but you can't
; run them until do some of the assignment.
; You will want to add more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"


   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (munit))) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list)) (munit) "rack -> mupl test")

   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (munit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   (check-equal? (eval-exp (mlet "x" (int 17) (add (var "x") (int 1)))) (int 18) "mlet and add test")
   (check-equal? (eval-exp (mlet "x" (int 17) (var "x"))) (int 17) "mlet test")


   (check-equal? (eval-exp (isgreater (int 2) (int 3))) (int 0) "isgreater test 1")
   (check-equal? (eval-exp (isgreater (int 2) (int 3))) (int 0) "isgreater test 2")

   (check-equal? (eval-exp (ifnz (int 0) (int 1) (int 0))) (int 0) "ifnz test")
   (check-equal? (eval-exp (mlet "x" (int 17) (fun "f" "y" (add (var "x") (var "y")))))
                 (closure (list (cons "x" (int 17))) (fun "f" "y" (add (var "x") (var "y")))) "func and mlet test")
   (check-equal? (eval-exp (first (apair (int 1) (int 0)))) (int 1) "first and apair test")
   (check-equal? (eval-exp (second (apair (int 1) (int 0)))) (int 0) "first and apair test")

   (check-equal? (eval-exp (ismunit (closure '() (fun #f "x" (munit))))) (int 0) "ismunit test")

   (check-equal? (eval-exp (ifmunit (int 0) (int 1) (int 2))) (int 1) "ifmunit test")

   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1))) (var "x"))) (int 1) "mlet* test")
   
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")

   (check-equal? (call (var "mupl-filter")
      (fun #f "x" (isgreater (var "x") (int 3)))
      (apair (apair (int 1) (int 4)) (apair (int 2) (int 5)))) (apair (int 4) (int 0)) (apair (int 5) (int 0)) (munit))

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")
   
))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
