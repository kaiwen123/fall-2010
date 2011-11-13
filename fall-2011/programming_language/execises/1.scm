#lang racket
; Calculate the length of a list. 
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

; remove the first occurance. 

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first (cdr los)))))))


(define count-nodes
    (lambda (s)
      (if (number? s)
          1
          (+ 1 (count-nodes (cadr s)) (count-nodes (caddr s))))))

(define (count-nodes1 s)
  (if (number? s)
      1
      (+ 1 (count-nodes1 (cadr s)) (count-nodes1 (caddr s)))))
