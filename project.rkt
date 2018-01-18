;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Data types
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct var  (string)    #:transparent)

;; Operations
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg  (num)    #:transparent)

;; Branching and conditions
(struct islthan (e1 e2)    #:transparent)
(struct ifzero (e1 e2 e3)    #:transparent)
(struct ifgthan (e1 e2 e3 e4)    #:transparent)

(struct mlet (s e1 e2)    #:transparent)

;; Pair related
(struct apair (e1 e2)    #:transparent)
(struct first (e) #:transparent)
(struct second (e) #:transparent)

;; Function calls
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call

;; Null value handling
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)



;; Problem 1

(define (racketlist->numexlist xs)
  (cond [(null? xs) (munit)]
        [else
          (apair (car xs) (racketlist->numexlist (cdr xs)))]))

(define (numexlist->racketlist xs)
  (cond [(munit? xs) null]
        [else
          (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]))




;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(eq? (car (car env)) str) (cdr (car env))]
        [else (envlookup (cdr env) str)]
        ))

; Do NOT change the two cases given to you.
; DO add more cases for other kinds of NUMEX expressions.
; We will test eval-under-env by calling it directly even though
; "in real life" it would be a helper function of eval-exp.
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
             (error "NUMEX addition applied to non-number")))]
        [(mult? e)
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (* (int-num v1)
                     (int-num v2)))
             (error "NUMEX addition applied to non-number")))]
        [(islthan? e)
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (< (int-num v1)
                    (int-num v2))
               (int 1)
               (int 0))
             (error "NUMEX islthan applied to non-number")))]
        [(ifzero? e)
         (let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
             (if (eq? (int-num v1) 0)
               (eval-under-env (ifzero-e2 e) env)
               (eval-under-env (ifzero-e3 e) env))
             (error "Bad argument for ifzero")
             ))]
        [(first? e)
         (let ([v1 (eval-under-env (first-e e) env)])
           (if (apair? v1)
             (apair-e1 v1)
             (error ("NUMEX is not apair"))
             ))]
        [(second? e)
         (let ([v1 (eval-under-env (second-e e) env)])
           (if (apair? v1)
             (apair-e2 v1)
             (error ("NUMEX is not apair"))
             ))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(ifgthan? e)
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgthan-e3 e) env)
               (eval-under-env (ifgthan-e4 e) env))
             (error "NUMEX islthan applied to non-number")))]
        [(neg? e)
         (let ([v (eval-under-env (neg-num e) env)])
           (if (int? v)
             (int (- 0 (int-num v)))
             (error "NUMEX negation applied to non-number")))]
        [(int? e)
         (let ([v (int-num e)])
           (if (integer? v)
             e
             (error "NUMXEX int applied to non-number")))]
        [(mlet? e)
         (let ([value (eval-under-env (mlet-e1 e) env)])
           (eval-under-env (mlet-e2 e) (cons (cons (mlet-s e) value) env)))]
        [(munit? e)
         (munit)]


        ; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (mlet* bs e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-map "CHANGE")

(define numex-mapAddN
  (mlet "map" numex-map
        "CHANGE (notice map is now in NUMEX scope)"))

;; Challenge Problem

;(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
;(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
;(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
;(define (eval-exp-c e)
; (eval-under-env-c (compute-free-vars e) null))
