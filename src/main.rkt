;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct var (string)    #:transparent)  ;; a variable
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg (e)       #:transparent)  ;; negate an expression



(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (s e1 e2)             #:transparent) ;; a let expression


(struct munit   ()            #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)           #:transparent) ;; if e1 is unit then 1 else 0
(struct islthan (e1 e2)       #:transparent) ;; if e1 is less than e2 1 else 0
(struct ifzero (e1 e2 e3)     #:transparent) ;; if e1 is zero then evaluate e2 else e3
(struct ifgthan (e1 e2 e3 e4) #:transparent) ;; if e1 is greater than e2 then evaluate to e3 else e4

(struct apair (e1 e2)         #:transparent) ;; if e1 is greater than e2 then evaluate to e3 else e4
(struct first (p)             #:transparent) ;; first of a pair
(struct second (p)            #:transparent) ;; second of a pair


;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

(define (racketlist->numexlist xs)
  (cond
    [(empty? xs) (munit)]
    [else (apair (first xs) ((racketlist->numexlist (rest xs))))]))


(define (numexlist->racketlist xs)
  (cond
   [(ismunit xs) empty]
   [else (cons (first xs) (numexlist->racketlist (second xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond
    [(null? env) (error "unbound variable during evaluation" str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e)
         (let ([v1 (int-num e)])
           (if (integer? v1)
               (int v1)
               (error "invalid type for NUMEX int")))]
        [(var? e)
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
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "NUMEX multiplication applied to non-number")))]
        [(neg? e)
         (let ([v1 (eval-under-env (neg-e e) env)])
           (if (int? v1)
               (- (0 v1))
               (error "NUMEX negation applied to non-number")))]
        [(fun? e)
         (closure (env e))]
        [(islthan? e)
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (< (int-num v1) (int-num v2)) (int 1) (int 0))
               (error "operands are of wrong type")))]
        [(ifzero? e)
         (let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
               (if (zero? (int-num v1))
                   (eval-under-env (ifzero-e2 e) env)
                   (eval-under-env (ifzero-e2 e)))
               (error "Wrong type givn")))]
        [(ifgthan? e)
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (int? v1)
               (if (int? v2)
                   (if (> (v1 v2))
                       (eval-under-env (ifgthan-e3 e) env)
                       (eval-under-env (ifgthan-e4 e) env))
                   (error "Right side operand is not of type NUMEX integer"))
               (error "Left side operand is not of type NUMEX integer")))]
        [(mlet? e)
         (eval-under-env (mlet-e2 e) (cons env (cons (mlet-s e) (eval-under-env (mlet-e1 e) env))))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
             (let ([f (closure-fun v1)]
                   [env1 (closure-env v1)]
                   [formal-actual (cons (fun-formal (closure-fun v1)) v2)])
               (if (null? (fun-nameopt f))
                   (eval-under-env (fun-body f) (cons env1 formal-actual))
                   (eval-under-env (fun-body f) (cons env1 (cons (fun-nameopt f) formal-actual))))
               )
             (error "Not a closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e)
         (let ([v1 (eval-under-env (first-p e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "not a pair")))]
        [(second? e)
         (let ([v1 (eval-under-env (second-p e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "not a pair")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3)
  (if (and (NUMEX-exp? e1) (NUMEX-exp? e2) (NUMEX-exp? e3))
  (ifzero (add (int -1) (ismunit e1)) e2 e3)
  (error "NUMEX ifmunit macro need all input be NUMEX expression")
  ))

(define (NUMEX-exp? input)
  (or (var? input)(int? input)(add? input)(mult? input)
      (neg? input)(fun? input)(islthan? input)(ifzero? input)
      (ifgthan? input)(call? input)(mlet? input)(apair? input)
      (first? input)(second? input)(munit? input)(ismunit? input)
      (closure? input)))


(define (mlet* bs e2)
  (cond
    [(not(list? bs))(error "NUMEX mlet* macro need a list input")]
    [(not (NUMEX-exp? e2))(error "NUMEX mlet* macro need NUMEX expression as second input")]
    [(null? bs) e2]
    [(not (pair? (car bs)))(error "NUMEX mlet* macro need a list of PAIRS input")]
    [(not (string? (car (car bs)))) (error "NUMEX mlet* macro input list of pair should have a string head")]
    [(not (NUMEX-exp? (cdr (car bs))))(error "NUMEX mlet* macro input list of pair should have a NUMEX expression tail")]
    [#t (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))]))

(define (ifeq e1 e2 e3 e4)
  (cond
    [(not (and (NUMEX-exp? e1)(NUMEX-exp? e2)(NUMEX-exp? e3)(NUMEX-exp? e4)))(error "NUMEX ifeq macro inputs should all be NUMEX expression")]
    [#t (ifzero (add (neg e1) e2) e3 e4)]))

;; Problem 4

(define numex-map
  (fun null "input-fun"
       (fun "mapper" "input-list" (ifzero (ismunit (var "input-list"))
                                          (apair (call (var "input-fun") (first  (var "input-list")))
                                                 (call (var "mapper")    (second (var "input-list"))))(munit)))))

(define numex-mapAddN
  (mlet "map" numex-map
        (fun null "inc" (call (var "map" ) (fun null "num" (add (var "inc")(var "num")))))))

;; Challenge Problem

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