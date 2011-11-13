#lang racket
; a function to extract sub list of numbers according to 
; the given relation operation, e.g. > < etc. 
; sublist : rel-op x N x nlist -> sublist 
(define (sublist rel-op N nlist)
  (if (empty? nlist) 
      empty
      (if (rel-op (car nlist) N)
          (cons 
           (car nlist)
           (sublist rel-op N (cdr nlist)))
          (sublist rel-op N (cdr nlist)))))

; test of the program.
(sublist = 10 empty)
(sublist < 10 '(1 2 3 10 23 400))
(sublist > 10 '(1 2 3 10 23 400))
(sublist = 10 '(1 2 3 10 23 400))
(sublist < 10 (cons 2 (cons 11 (cons 5 (cons 100 empty)))))