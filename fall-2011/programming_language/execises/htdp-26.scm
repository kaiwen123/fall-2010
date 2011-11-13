#lang racket

;; tabulate-div which generates all the divisors of 
;; a number starting from 1 to the number itself. 
(define (tabulate-div n)
  (let ((d n))
    (letrec ((func
              (lambda (d n)
                (if (= d 1)
                    1 
                    (if (= (remainder n d) 0)
                        (cons (list d) (func (- d 1) n))
                        (func (- d 1) n))))))
      (func d n))))

(tabulate-div 10)
(tabulate-div 100)
      

;; generate the greatest common divisor. 
;; gcd : m x n -> N
(define (gcd m n)
  (cond 
    ((= m n) m)
    ((> m n) ; get the largest divisor of n that can divide m. 
     (let ((d n))
       (letrec ((func1
                (lambda (x y);x is number and y is divisor. 
                  (if (and (= (remainder x y) 0) (= (remainder m y) 0))
                      y
                      (func1 x (- y 1))))))
         (func1 n d))))
    (else ; get the largest divisor of m that can divide n. 
     (let ((d m))
       (letrec ((func2 
                (lambda (x y);x is number and y is divisor. 
                  (if (and (= (remainder x y) 0) (= (remainder n y) 0))
                      y
                      (func2 x (- y 1))))))
         (func2 m d))))))

(gcd 10 20)
(gcd 12 18)
(gcd 101135853 45014640)
(gcd 18 24)
            