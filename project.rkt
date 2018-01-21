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
        [(call? e)
         (let ([clos (eval-under-env (call-funexp e) env)])
           (if (closure? clos)
             (let ([function (closure-fun clos)]
                   [actual (eval-under-env (call-actual e) env)])
               (eval-under-env (fun-body function) (cons (cons (fun-formal function) actual)
                                                         (cons (cons (fun-nameopt function) clos) (closure-env clos)))))
             (error "NUMEX closure expected")
             ))]

        [(ismunit? e)
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (if (munit? v1)
             (int 1)
             (int 0)))]

        [(fun? e)
         (closure env e)]
        [(closure? e) e]
        [(munit? e)
         (munit)]


        ; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

; Problem 3

(define (ifmunit e1 e2 e3) (
                            cond [(eq? (munit? e1) #t) e2]
                            [else e3]
                            ))

(define (mlet* bs e2)
  (cond [(null? bs) (mlet "result" (munit) e2)]
        [else (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))]))

(define (ifeq e1 e2 e3 e4) (ifgthan e1 e2 e4 (ifgthan e2 e1 e4 e3)))
(define (ifneq e1 e2 e3 e4) (ifgthan e1 e2 e3 (ifgthan e2 e1 e3 e4)))

; Problem 4

(define numex-map (fun null "function-to-applied" (fun "maplist" "list"
                                                       (ifeq (ismunit (var "list")) (int 1) (munit)
                                                             (apair (call (var "function-to-applied")
                                                                          (first (var "list")))
                                                                    (call (var "maplist")
                                                                          (second (var "list"))))))))


(define numex-mapAddN
  (mlet "map" numex-map
        (fun null "i" (fun null "list" (call (call (var "map")
                                                   (fun null "operand" (add (var "i") (var "operand"))))
                                             (var "list"))))))

; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

; We will test this function directly, so it must do
; as described in the assignment
; Do NOT share code with eval-under-env because that will make grading
(define (getCommonEnv env set)
  (if (equal? env null) null
      (if (set-member? set (car (car env)))
          (cons (car env) (getCommonEnv (cdr env) set))
          (getCommonEnv (cdr env) set))))

(define (compute-free-vars-handler e)
   (cond [(var? e) (cons e (set (var-string e)))]
        [(int? e) (cons e (set))]
        [(munit? e) (cons e (set))]
        [(add? e)
         (let ([v1 (compute-free-vars-handler (add-e1 e))]
               [v2 (compute-free-vars-handler (add-e2 e))])
           (cons (add (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        [(mult? e)
         (let ([v1 (compute-free-vars-handler (mult-e1 e))]
               [v2 (compute-free-vars-handler (mult-e2 e))])
           (cons (mult (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        [(neg? e)
         (let ([v1 (compute-free-vars-handler (neg-num e))])
           (cons (neg (car v1)) (cdr v1)))]
        [(islthan? e)
        (let ([v1 (compute-free-vars-handler (islthan-e1 e))]
               [v2 (compute-free-vars-handler (islthan-e2 e))])
           (cons (islthan (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        [(ifzero? e)
         (let ([v1 (compute-free-vars-handler (ifzero-e1 e))]
               [v2 (compute-free-vars-handler (ifzero-e2 e))]
               [v3 (compute-free-vars-handler (ifzero-e3 e))])
           (cons (ifzero (car v1) (car v2) (car v3)) (set-union (cdr v1) (cdr v2) (cdr v3))))]
        [(ifgthan? e)
          (let ([v1 (compute-free-vars-handler (ifgthan-e1 e))]
               [v2 (compute-free-vars-handler (ifgthan-e2 e))]
               [v3 (compute-free-vars-handler (ifgthan-e3 e))]
               [v4 (compute-free-vars-handler (ifgthan-e4 e))])
           (cons (ifgthan (car v1) (car v2) (car v3) (car v4)) (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4))))]
        [(mlet? e)
         (let ([v1 (compute-free-vars-handler (mlet-e1 e))]
               [v2 (compute-free-vars-handler (mlet-e2 e))])
            (cons (mlet (mlet-s e) (car v1) (car v2)) (set-union (set-remove (cdr v2) (mlet-s e)) (cdr v1))))]
        [(apair? e)
         (let ([v1 (compute-free-vars-handler (apair-e1 e))]
               [v2 (compute-free-vars-handler (apair-e2 e))])
           (cons (apair (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        [(first? e)
         (let ([v1 (compute-free-vars-handler (first-e e))])
           (cons (first (car v1)) (cdr v1)))]
        [(second? e)
          (let ([v1 (compute-free-vars-handler (second-e e))])
           (cons (second (car v1)) (cdr v1)))]
        [(ismunit? e)
          (let ([v1 (compute-free-vars-handler (ismunit-e e))])
           (cons (neg (car v1)) (cdr v1)))]

        [(fun? e)
          (let ([cfvf (compute-free-vars-handler (fun-body e))])
            (let ([free-var-set (set-remove (set-remove (cdr cfvf) (fun-formal e)) (fun-nameopt e))])
               (cons (fun-challenge (fun-nameopt e) (fun-formal e) (car cfvf) free-var-set) free-var-set)))]
        [(call? e)
         (let ([va (compute-free-vars-handler (call-actual e))]
               [vf (compute-free-vars-handler (call-funexp e))])
           (cons (call (car vf) (car va)) (set-union (cdr vf) (cdr va))))]

        [#t (error (format "bad NUMEX expression: ~v" e))]))

(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (+ (int-num v1)
                     (int-num v2)))
             (error "NUMEX addition applied to non-number")))]
        [(mult? e)
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (* (int-num v1)
                     (int-num v2)))
             (error "NUMEX addition applied to non-number")))]
        [(islthan? e)
         (let ([v1 (eval-under-env-c (islthan-e1 e) env)]
               [v2 (eval-under-env-c (islthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (< (int-num v1)
                    (int-num v2))
               (int 1)
               (int 0))
             (error "NUMEX islthan applied to non-number")))]
        [(ifzero? e)
         (let ([v1 (eval-under-env-c (ifzero-e1 e) env)])
           (if (int? v1)
             (if (eq? (int-num v1) 0)
               (eval-under-env-c (ifzero-e2 e) env)
               (eval-under-env-c (ifzero-e3 e) env))
             (error "Bad argument for ifzero")
             ))]
        [(first? e)
         (let ([v1 (eval-under-env-c (first-e e) env)])
           (if (apair? v1)
             (apair-e1 v1)
             (error ("NUMEX is not apair"))
             ))]
        [(second? e)
         (let ([v1 (eval-under-env-c (second-e e) env)])
           (if (apair? v1)
             (apair-e2 v1)
             (error ("NUMEX is not apair"))
             ))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(ifgthan? e)
         (let ([v1 (eval-under-env-c (ifgthan-e1 e) env)]
               [v2 (eval-under-env-c (ifgthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (> (int-num v1) (int-num v2))
               (eval-under-env-c (ifgthan-e3 e) env)
               (eval-under-env-c (ifgthan-e4 e) env))
             (error "NUMEX islthan applied to non-number")))]
        [(neg? e)
         (let ([v (eval-under-env-c (neg-num e) env)])
           (if (int? v)
             (int (- 0 (int-num v)))
             (error "NUMEX negation applied to non-number")))]
        [(int? e)
         (let ([v (int-num e)])
           (if (integer? v)
             e
             (error "NUMXEX int applied to non-number")))]
        [(mlet? e)
         (let ([value (eval-under-env-c (mlet-e1 e) env)])
           (eval-under-env-c (mlet-e2 e) (cons (cons (mlet-s e) value) env)))]
        [(call? e)
         (let ([clos (eval-under-env-c (call-funexp e) env)])
           (if (closure? clos)
             (let ([function (closure-fun clos)]
                   [actual (eval-under-env-c (call-actual e) env)])
               (eval-under-env-c (fun-body function) (cons (cons (fun-formal function) actual)
                                                           (cons (cons (fun-nameopt function) clos) (closure-env clos)))))
             (error "NUMEX closure expected")
             ))]

        [(ismunit? e)
         (let ([v1 (eval-under-env-c (ismunit-e e) env)])
           (if (munit? v1)
             (int 1)
             (int 0)))]

        [(fun-challenge? e)
         (if (and (or (string? (fun-challenge-nameopt e)) (null? (fun-challenge-nameopt e))) (string? (fun-challenge-formal e)))
           (closure (getCommonEnv env (fun-challenge-freevars e))  e)
           (error "NUMEX function name and parameter name must be string"))]
        [(closure? e) e]
        [(munit? e)
         (munit)]


        ; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

(define (compute-free-vars e)
  (car (compute-free-vars-handler e)))

; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
