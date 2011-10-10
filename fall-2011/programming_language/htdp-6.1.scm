#lang slideshow
(require lang/htdp-intermediate)
(require slideshow/flash)

(define series
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))