#lang racket
(require htdp/draw)
(require lang/htdp-intermediate)
; (require htdp/image)

(start 500 500)
(make-posn 10 20)
(posn-x (make-posn 10 20))

(define (distance-to-0 a-posn)
  (sqrt 
   (+ (sqr (posn-x a-posn))
      (sqr (posn-y a-posn)))))

; tests. 
(distance-to-0 (make-posn 3 4))
(distance-to-0 (make-posn 2 10))
(distance-to-0 (make-posn (* 2 3) (* 2 4)))
(distance-to-0 (make-posn 12 (- 6 1)))

;; drawing figures.
(display (draw-solid-line (make-posn 2 3) (make-posn 3 4)))

;; dimensions of traffic light    [curriculum1aa-Z-G-18.gif]
(define WIDTH 500)
(define HEIGHT 500)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

;; the positions of the bulbs 
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

;; draw the light with the red bulb turned on
(start WIDTH HEIGHT)
(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)
