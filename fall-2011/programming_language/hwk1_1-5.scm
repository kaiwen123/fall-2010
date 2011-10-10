#lang racket
;;
;; This file contains questions 1-5 of homework 1. 
;; please refer to file named hwk1_6.scm for homework 6.
;; 
;; 
; homework1-1. EOPL3 exercise 1.12. 
; subst : sym x sym x s-list -> s-list. 
; by using lambda, we incorporate the definition of
; subst-in-s-exp in to the definition of subst. 
; s-list :: () 
; s-list :: (s-exp . s-list) 
; s-exp :: symbol | s-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons 
         (letrec ((subst-in-s-exp
                   (lambda (new1 old1 sexp)
                     (if (symbol? sexp)
                         (if (eqv? sexp old1) new1 sexp)
                         (subst new1 old1 sexp)))))
                   (subst-in-s-exp new old (car slist)))
           (subst new old (cdr slist))))))

; test subst. 
(display "\nQuestion 1.(EOPL3 exercise 1.12)--->>>>\n")
(define x '((b c) (b () d)))
; before. 
x
; after. 
(subst 'a 'b x)


; homework1-2, EOPL3 Exercise 1.20. 
; count-occurances : sym x slist -> int.
; Count occurance of a symbol in a list. 
; s-list :: () 
; s-list :: (s-exp . s-list) 
; s-exp :: symbol | s-list
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+
         (if (symbol? (car slist))
            (if (equal? s (car slist))
                1
                0)
            (count-occurrences s (car slist)))        
         (count-occurrences s (cdr slist))))))
    ; test for count-occurences
(display "\nQuestion 2.(EOPL3 exercise 1.20)--->>>>\n")
(define y '(a b c c a a (a b) () (a)))
y
(count-occurrences 'a y)
(count-occurrences 'b y)
(count-occurrences 'c y)

; homework1-3, EOPL3 Exercise 1.28. 
; merge : lon x lon -> slon
; This procedure merge two lists of sorted numbers and 
; return a sorted list of number included in lon1 and lon2. 
; first detect if either of the lists is null. 
(define merge
  (lambda (lon1 lon2)
    (if (null? lon1)
        lon2
        (if (null? lon2)
            lon1
            (cons
             (if (<= (car lon1) (car lon2))
                 (car lon1)
                 (car lon2))
             (if (<= (car lon1) (car lon2))
                 (merge (cdr lon1) lon2)
                 (merge lon1 (cdr lon2))))))))
; test for merging. 
(display "\nQuestion 3.(EOPL3 exercise 1.28)--->>>>\n")
(define lon1 '(1 4 7 16 21 29))
(define lon2 '(1 2 6 7 8 19 22))
(merge lon1 lon2)
(merge '(35 62 81 90 91) '(3 83 85 90))

; homework1-4 Exercise 4. 
; compose : proc1 x proc2 -> proc3 
; This procedure will compose two procedures into one, with the following
; rule: (compose p1 p2) = (p1 (p2 x)). 
; This exercise is related to s-exp, 
(define ((compose p1 p2) x)
  (display (p1 (p2 x)))
  (newline))

; test for compose. 
(display "\nQuestion 4 --->>>>\n")
(define x4 '(a b c d e f))
((compose car cdr) '(a b c d e))
((compose cdr cdr) '(a b c d))
((compose car car) '((a b c) a b c d))
((compose car car) '((a b c) a b c d))

; homework1-5, Exercise 5. 
; car&cdr : s x slist x err -> s-exp | sym. 
; produce a procedure that takes a list with same
; structure as the slist and returns the value in 
; the same position as teh leftmost occurence of
; s in slist. If it doesn't occur in the slist, 
; then errvalue is returned. 
(define errmsg 'fail)

; procedure to generate car and cdr list.
(define car&cdr1
  (lambda (s slist err)
    (if (null? slist) 
        (list err)
        (if (symbol? (car slist)) 
            (if (eqv? s (car slist))
                '(car)
                (append
                 '(cdr)
                  (car&cdr1 s (cdr slist) err)))
            (or 
             (append 
              '(car)
              (car&cdr1 s (car slist) err))
             (append
              '(cdr)
              (car&cdr1 s (cdr slist) err)))))))

(define contains-error
  (lambda (slist)
    (if (null? slist)
        #f
        (if (symbol? (car slist))
            (if (eqv? (car slist) errmsg)
                #t
                (contains-error (cdr slist)))
            (or
             (contains-error (car slist))
             (contains-error (cdr slist)))))))
    
(define car&cdr2
  (lambda (slist)
    (if (contains-error slist)
        errmsg
        (if (null? (cdr slist))
            (car slist)
            (if (symbol? slist)
                slist ; only on symbol. 
                (append
                 '(compose)
                 (list (car slist))
                 ;(list (cadr slist))
                 (list (car&cdr2 (cdr slist)))))))))

(define car&cdr
  (lambda (s slist err)
    (display (car&cdr2 (reverse (car&cdr1 s slist err))))
    (newline)))
          
; test for question 5.
(display "\nQuestion 5 --->>>>\n")
; test of the compose function. 
(car&cdr 'a '(a b c) 'fail)
(car&cdr 'c '(a b c) 'fail)
(car&cdr 'dog '(cat lion (fish dog ()) pig) 'fail)
(car&cdr 'a '(b c) 'fail)