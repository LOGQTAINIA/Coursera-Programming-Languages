;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist rktlist)
  (if (null? rktlist) (aunit)
      (apair (car rktlist)
                   (racketlist->mupllist (cdr rktlist)))))

(define (mupllist->racketlist mpllist)
  (if (aunit? mpllist) null
      (cons (apair-e1 mpllist)
                  (mupllist->racketlist (apair-e2 mpllist)))))
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
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([new-var (cons (mlet-var e)
                              (eval-under-env (mlet-e e) env))])
           (eval-under-env (mlet-body e) (cons new-var env)))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([cls (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cls)
               (let* ([fun (closure-fun cls)]
                      [para (cons (fun-formal fun)
                                  (eval-under-env (call-actual e) env))]
                      [funenv (if (fun-nameopt fun)
                                  (cons (cons (fun-nameopt fun) cls)
                                        (closure-env cls))
                                  (closure-env cls))])
                 (eval-under-env (fun-body fun) (cons para funenv)))
               (error "MUPL call apllied to non-fun")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)
                           (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([pr (eval-under-env (fst-e e) env)])
           (if (apair? pr)
               (apair-e1 pr)
               (error "MUPL fst apllied to non-apair")))]
        [(snd? e)
         (let ([pr (eval-under-env (snd-e e) env)])
           (if (apair? pr)
               (apair-e2 pr)
               (error "MUPL snd apllied to non-apair")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet (car (car lstlst))
                  (cdr (car lstlst))
                  (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "fn"
    (fun "aux" "lst"
      (ifaunit (var "lst")
               (aunit)
               (apair (call (var "fn") (fst (var "lst")))
                      (call (var "aux") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "c" (call (var "map")
                          (fun #f "x" (add (var "x") (var "c")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) 
  (struct res (e fv))
  (define (f e)
    (cond [(var? e) (res e (set (var-string e)))]
          [(int? e) (res e (set))]
          [(aunit? e) (res e (set))]
          [(add? e) (let ([r1 (f (add-e1 e))]
                          [r2 (f (add-e2 e))])
                      (res (add (res-e r1) (res-e r2))
                           (set-union (res-fv r1) (res-fv r2))))]
          [(ifgreater? e) (let ([r1 (f (ifgreater-e1 e))]
                                [r2 (f (ifgreater-e2 e))]
                                [r3 (f (ifgreater-e3 e))]
                                [r4 (f (ifgreater-e4 e))])
                            (res (ifgreater (res-e r1) (res-e r2)
                                            (res-e r3) (res-e r4))
                                 (set-union (res-fv r1) (res-fv r2)
                                            (res-fv r3) (res-fv r4))))]
          [(mlet? e) (let ([r1 (f (mlet-e e))]
                           [r2 (f (mlet-body e))])
                       (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                            (set-union (res-fv r1) (set-remove (res-fv r2) (mlet-var e)))))]
          [(fun? e) (let* ([r (f (fun-body e))]
                           [fv (set-remove (res-fv r) (fun-formal e))]
                           [fv (if (fun-nameopt e)
                                   (set-remove fv (fun-nameopt e))
                                   fv)])
                      (res (fun-challenge (fun-nameopt e) (fun-formal e)
                                          (res-e r) fv)
                           fv))]
          [(call? e) (let ([r1 (f (call-funexp e))]
                           [r2 (f (call-actual e))])
                       (res (call (res-e r1) (res-e r2))
                            (set-union (res-fv r1) (res-fv r2))))]
          [(isaunit? e) (let ([r (f (isaunit-e e))])
                          (res (isaunit (res-e r)) (res-fv r)))]
          [(apair? e) (let ([r1 (f (apair-e1 e))]
                            [r2 (f (apair-e2 e))])
                        (res (apair (res-e r1) (res-e r2))
                             (set-union (res-fv r1) (res-fv r2))))]
          [(fst? e) (let ([r (f (fst-e e))])
                      (res (fst (res-e r)) (res-fv r)))]
          [(snd? e) (let ([r (f (snd-e e))])
                     (res (snd (res-e r)) (res-fv r)))]))
  (res-e (f e)))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s))))
                  e)][(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([new-var (cons (mlet-var e)
                              (eval-under-env-c (mlet-e e) env))])
           (eval-under-env-c (mlet-body e) (cons new-var env)))]
        [(call? e)
         (let ([cls (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? cls)
               (let* ([fun (closure-fun cls)]
                      [para (cons (fun-challenge-formal fun)
                                  (eval-under-env-c (call-actual e) env))]
                      [funenv (if (fun-challenge-nameopt fun)
                                  (cons (cons (fun-challenge-nameopt fun) cls)
                                        (closure-env cls))
                                  (closure-env cls))])
                 (eval-under-env-c (fun-challenge-body fun) (cons para funenv)))
               (error "MUPL call apllied to non-fun")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env)
                           (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([pr (eval-under-env-c (fst-e e) env)])
           (if (apair? pr)
               (apair-e1 pr)
               (error "MUPL fst apllied to non-apair")))]
        [(snd? e)
         (let ([pr (eval-under-env-c (snd-e e) env)])
           (if (apair? pr)
               (apair-e2 pr)
               (error "MUPL snd apllied to non-apair")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
