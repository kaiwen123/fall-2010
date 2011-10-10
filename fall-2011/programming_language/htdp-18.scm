#lang racket
;; example of local expression. 

(local ((define (f x) (+ x 10))
        (define (g alon) 
          (cond 
            ((null? alon) null)
            (else (cons (f (car alon)) (g (cdr alon)))))))
  (g (list 1 2 3)))

;; another example of local expression. 
(local ((define x 10)
        (define y (* x 3)))
  (* y y))
