#lang racket

;; make-even : n -> (listof number)
(define (make-even n)
  (* 2 n))
(define (make-odd n)
  (+ (* 2 n) 1))
(define (make-arith n d)
  (+ n d))
(define (make-geom n m)
  (/ n m))

;; make a series of even numbers. 
(define (series-even n)
  (display (make-even n))
  (display " ")
  (if (= 0 n)
      (make-even n)
      (+ (make-even n)
           (series-even (- n 1)))))

(define (series-odd n)
  (display (make-odd n))
  (display " ")
  (if (= 0 n)      
      (make-odd n)
      (+ (make-odd n)
           (series-odd (- n 1)))))

(define (series-arith n d)
  (display (make-arith n d))
  (display " ")
  (if (<= n 0)
      n
      (+ (make-arith n d)
         (series-arith (- n d) d))))

(define (series-geom n m)
  (display (make-geom n m))
  (display " ")
  (if (< n 1)
      (make-geom n m)
      (+ (make-geom n m)
         (series-geom (/ n m) m))))

(series-even 10)
(series-odd 10)
(series-arith 100 10)
(series-geom 100 2)

;; taylor series.
;; factorial function. 
(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))

;; n controls how many elements to calc. 
;; m controls exponential.
(define (taylor-series n m)
  (display (/ (expt m n) (! n)))
  (display " ")
  (if (= n 0)
      1
      (+ (/ (expt m n) (! n))
         (taylor-series (- n 1) m))))
; test taylor series. 
(taylor-series 5 1)