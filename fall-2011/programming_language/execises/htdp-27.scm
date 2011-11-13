#lang racket

(require htdp/draw)
(require lang/posn)
; (require lang/htdp-intermediate)
; (require lang/htdp-advanced)

(start 500 500)

;; sierpinski: posn posn posn -> true 
;; to draw a sierpinski triangle down at a b c. 
;; assuming it is large enough. 
(define (sierpinski a b c)
  (cond 
    ((too-small? a b c) true)
    (else 
     (local ((define a-b (mid-point a b))
             (define a-c (mid-point a c))
             (define b-c (mid-point b c)))
       (and 
        (draw-triangle a b c)
        (sierpinski a a-b a-c)
        (sierpinski b a-b b-c)
        (sierpinski c a-c b-c))))))

;; mid-point : posn posn -> posn
;; to compute the mid point of two given points.
(define (mid-point a b)
  (let ((mid
         (lambda (x y)
           (/ (+ x y) 2))))
    (make-posn
     (mid (posn-x a) (posn-x b))
     (mid (posn-y a) (posn-y b)))))

;; too-small : posn posn posn -> bool 
;; to test if the triangle identified by the three points
;; are too small.
(define (too-small? a b c)
  (let ((threshold 10))
    (not (or (<= (abs (- (posn-x a) (posn-x b))) threshold)
             (<= (abs (- (posn-x a) (posn-x c))) threshold)
             (<= (abs (- (posn-x b) (posn-x c))) threshold)
             (<= (abs (- (posn-y a) (posn-y b))) threshold)
             (<= (abs (- (posn-y a) (posn-y c))) threshold)
             (<= (abs (- (posn-y b) (posn-y c))) threshold)))))

;; draw-triangle : posn posn posn -> true 
;; to draw a triangle using the three give points. 
(define (draw-triangle a b c)
  (and (draw-solid-line a b 'blue)
       (draw-solid-line a c 'green)
       (draw-solid-line b c 'black)))

;; testing the program. 
;; (draw-solid-line (make-posn 10 20) (make-posn 100 200))
(define A (make-posn 200 0))
(define B (make-posn 27 300))
(define C (make-posn 373 300))

;; (sierpinski (make-posn 50 50) (make-posn 200 200) (make-posn 70 500))
;; (sierpinski A B C)

;; binary search : 