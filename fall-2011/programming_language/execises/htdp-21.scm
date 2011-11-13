#lang racket

; for execise 21.1.1. 
; tabulate : func x n -> lon
(define (tabulate func n)
  (cond 
    ((= n 0) (list (func 0)))
    (else 
     (cons (func n)
           (tabulate func (- n 1))))))

; testing. 
(tabulate sin 10)
(tabulate sqrt 10)

; for execise 21.1.2. 
; fold : (listof X) func start -> (listof X)
(define (fold alon func start) 
  (cond 
    ((empty? alon) start)
    (else (func (car alon)
                (fold (cdr alon) func start)))))
; test. 
(fold '(1 2 34 5) + 0)
(fold '(3 4 5 10) * 1)
(fold '(20 1000) + 0)

;; function produce a function. 
(define (add x)
  (local ((define (x-adder y) (+ x y)))
    x-adder))

;; defining a function to do multiple functions onto 
;; a list. 
(define (fold1 op)
  (local ((define (func alon start)
            (if (empty? alon)
                start
                (op (car alon) 
                   (func (cdr alon) start)))))
    func))

;; testing
(define fold1-sum (fold1 +))
(define fold1-mul (fold1 *))
(fold1-sum '(10 20 30) 0)
(fold1-mul '(10 20 30) 1)
          
