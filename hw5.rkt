;; Chloe Hu
;; xinyuh22
;; CSE413 23sp, Programming Languages & Implementation
;; Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string)      #:transparent) ;; a variable, e.g., (var "foo")
(struct int  (num)         #:transparent) ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)       #:transparent) ;; add two expressions
(struct isgreater (e1 e2)  #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3)    #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body)  #:transparent) ;; a local binding (let var = e in body)
(struct apair   (e1 e2)    #:transparent) ;; make a new pair
(struct first   (e)        #:transparent) ;; get first part of a pair
(struct second  (e)        #:transparent) ;; get second part of a pair
(struct munit   ()         #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)        #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun)  #:transparent)

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rlist)
  (cond [(null? rlist) (munit)] 
        [(null? (cdr rlist)) (apair (car rlist) (munit))]
        [#t (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))

(define (mupllist->racketlist mlist)  
  (cond [(equal? mlist (munit)) '()]
        [(equal? (apair-e2 mlist) (munit)) (cons (apair-e1 mlist) null)]
        [#t (append (list (apair-e1 mlist)) (mupllist->racketlist (apair-e2 mlist)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(closure? e) e]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e1 e) env)])
           (if (> (int-num v1) (int-num v2))
               (int 1)
               (int 0)))]
        [(ifnz? e)
         (if (zero? (int-num (ifnz-e1 e)))
             (eval-under-env (ifnz-e3 e) env)
             (eval-under-env (ifnz-e2 e) env))]
        [(fun? e) (closure env 
                           (fun (fun-nameopt e) 
                                 (fun-formal e) (fun-body e)))]
        ;; (mlet s e1 e2)
        [(mlet? e)
         (eval-under-env (mlet-body e) (append env (list (cons (mlet-var e) (eval-under-env (mlet-e e) env)))))]
        ;; (call (closure (list (cons "x" (int 17))) (fun "f" "y" (add (var "x") (var "y")))) (int 1))
        ;; (struct call (funexp actual)       #:transparent)
        [(call? e)
         ;; e1: closure
         ;; e2: argument
         (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (if (closure? e1)
               ;; expression
               (eval-under-env (fun-body (closure-fun e1))
                               ;; the env
                               (if (fun-nameopt (closure-fun e1))        
                                   (append (list (cons (fun-nameopt (closure-fun e1)) e1)                                              
                                                 (cons (fun-formal (closure-fun e1)) e2)) 
                                           (closure-env e1))
                                   (append (list (cons (fun-formal(closure-fun e1)) e2)) 
                                           (closure-env e1))))
               (error "e1 is not a closure")))]
        [(apair? e)
         (apair (eval-under-env(apair-e1 e) env)  (if (munit? (apair-e2 e))
                                                                 (munit)
                                                                 (eval-under-env (apair-e2 e) env)))]
        [(first? e)
         (if (apair? (eval-under-env (first-e e) env))
                        (apair-e1 (eval-under-env(first-e e) env))
                        (error "Not a pair"))]
        [(second? e)
         (if (apair? (eval-under-env (second-e e) env))
                         (apair-e2 (eval-under-env(second-e e) env))
                         (error "Not a pair"))]
        [(ismunit? e)
         (if (munit? (eval-under-env (ismunit-e e) env))
                          (int 1)
                          (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; (call (closure (list (cons "x" (int 17))) (fun "f" "y" (add (var "x") (var "y")))) (int 1))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3)
  (if (equal? (ismunit e1) (int 0))
             e3
             e2))

(define (mlet* bs e2)
  (if (null? (cdr bs))
      (mlet (car (car bs)) (cdr (car bs)) e2)
      (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))))

(define (ifeq e1 e2 e3 e4)
  (if (equal? (int-num e1) (int-num e2))
      e3
      e4))

;; Problem 4

(define mupl-filter2
  (fun #f "f"
       (fun #f "lst"
            (if (ismunit (var "lst"))
                (munit)
                (ifnz (call (var "f") (first (var "lst")))
                      (apair (first (var "lst")) (call (var "mupl-filter") (var "f") (second (var "lst"))))
                      (call (var "mupl-filter") (var "f") (second (var "lst"))))))))

(define mupl-filter
  (fun #f "b" 
       (fun "c" "lst"
           (ifmunit (var "lst")
                    (munit)
                    (ifnz (call (var "b") (first (var "lst")))
                          (apair (call (var "b") (first (var "lst"))) (call (var "c") (second (var "d"))))
                          (call (var "b") (second (var "lst"))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun #f "i"
             (fun "c" "d"
                  (call (call (var "filter")
                              (fun #f "x"
                                   (if (isgreater (var "x") (var "i"))
                                       (var "x")
                                       (munit))))
                              (var "d"))))))


;; Challenge Problem (extra credit)

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
