
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; problem 1
(define (sequence low high stride) 
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; problem 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix))
       xs))

;; problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([pos (remainder n (length xs))])
              (car (list-tail xs pos)))]))

;; problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0)
                                    (* x -1)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; problem 6
(define dan-then-dog
  (letrec ([f (lambda () (cons "dan.jpg" g))]
           [g (lambda () (cons "dog.jpg" f))])
    f))

;; problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (st) (cons (cons 0 (car (st)))
                                 (lambda () (f (cdr (st))))))])
    (lambda () (f s))))

;; problem 8
(define (cycle-lists xs ys)
  (letrec ([aux (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
           [f (lambda (n) (cons (aux n)
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; problem 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [aux (lambda (n) (if (>= n len)
                                #f
                                (letrec ([ele (vector-ref vec n)])
                                  (if (and (pair? ele) (equal? v (car ele)))
                                      ele
                                      (aux (+ n 1))))))])
    (aux 0)))

;; problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [slot 0]
           [helper (lambda (v) (let ([ele-xs (assoc v xs)])
                                  (if ele-xs
                                      (begin 
                                        (vector-set! cache slot ele-xs)
                                        (set! slot (remainder (+ slot 1) n))
                                        ;;(print cache)
                                      ele-xs)
                                      ele-xs)))])
    (lambda (v) (let ([ele-cache (vector-assoc v cache)])
                  (if ele-cache 
                      ele-cache
                      (helper v))))))

;; problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([max-val e1])
        (letrec ([f (lambda (tk) 
                      (if (>= (tk) max-val)
                          #t
                          (f tk)))])
          (f (lambda () e2))))]))