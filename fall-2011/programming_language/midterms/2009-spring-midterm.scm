#lang eopl 

;;; 2010 spring midterm example, programming languages. 

;; 1. recursive number sum. 
;; sumNumbers : lst -> number 
;; calcuate the sum of numbers of a given list. 
;; e.g. (sumNumbers '(a b 10 20)) should yield 30.
(define (sumNumbers lst)
  (if (null? lst)
      0
      (let ((fst (car lst))
            (rst (cdr lst)))
        (cond 
          ((pair? fst)
           (+ (sumNumbers fst) (sumNumbers rst)))
          ((number? fst)
           (+ fst (sumNumbers rst)))
          (else (sumNumbers rst))))))

;;; test examples. 
(display (sumNumbers '(a)))
(newline)
(display (sumNumbers '(2 56 x (1 1))))
(newline)
(display (sumNumbers '(((a)) -2 (2 (a) (-1 0 1)))))
(newline)
(display (sumNumbers '(a (1 2 3) (((10) 11) 9))))


;;2. higher order function. 
;; 
(define (VAG f i)
  (lambda lst
    (apply f (cons i lst))))

;;; test examples
((VAG + 0) 1 3 5)
((VAG * 1) 1 3 5)
((VAG append '()) '(1) '(a (b) c) '())
((VAG * 100))

;;;3. explain the mysterious scheme code. 
(define (mystery tse te ee)
  (display tse)
  (cond 
    (tse te)
    (else ee)))

(define (midterm n)
  ;;(if (zero? n) 0 (midterm (- n 1))))
  (mystery (zero? n) 0 (midterm (- n 1))))

(midterm 2)