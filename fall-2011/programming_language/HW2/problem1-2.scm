#lang eopl

(require rnrs/eval-6)

;; This file contains problem1-2 of homework2. 
;; Author Shumin Guo (U00617724)
;; Revision version 1.0 10/12/2011. 

;; This file implements the Map ADT as specified in 
;; problem1-1. 

;; map : -> empty-map
;; yeilds the empty map. 
(define (map) 
  '())

;; clear : map -> map
;; takes a map m and yields the empty map. 
(define (clear map)
  '())

;; containsKey : key x map -> boolean
(define (containsKey key map)
  (if (isEmpty map)
      #f
      (if (eqv? (caar map) key)
          #t
          (containsKey key (cdr map)))))

;; containsValue : map x value -> boolean
;; Takes a map and a value v as input and checks if 
;; the value v is bound to some key.
(define (containsValue value map)
  (if (null? map)
      #f
      (if (equal? (cadar map) value) 
          #t
          (containsValue value (cdr map)))))

;; equals : map1 x map2 -> boolean
;; takes two maps m and n as input and checks if 
;; the two has similar bindings. 
;; first, two maps should have the same size, 
;; if not, they are not equal. 
;; then, two same sized maps, we pick map1, and 
;; for each key in the map, check if it also exists 
;; map2 and have a same value. 
(define (equals map1 map2)
  (if (not (= (length map1) (length map2)))
      #f
      (letrec ((containsPair? 
                (lambda (key value map)
                  (if (isEmpty map)
                      #f
                      (if (and (eqv? (caar map) key) (eqv? (cadar map) value))
                          #t
                          (containsPair? key value (cdr map))))))
               (checkEqual
                (lambda (m1 m2)
                  (if (isEmpty m1)
                      #t
                      (and (containsPair? (caar m1) (cadar m1) m2)
                           (checkEqual (cdr m1) m2))))))
        (checkEqual map1 map2))))

       
;; get : key x map -> value
;; takes map map and key key as input and output
;; the binding of key, value as output. 
(define (get key map)
  (if (isEmpty map) 
      (report-no-key-found key)
      (if (eqv? (caar map) key) 
          (cadar map)
          (get (cdr map) key))))

;; isEmpty : map -> boolean
;; takes a map map as input and checks if it is empty.
;; true if map is empty and false otherwise. 
(define (isEmpty map)
  (null? map))

;; put : key x value x map -> map
;; takes a map, a key and a value as input
;; and generate a new map similar to m except that 
;; key is bound to value. 
(define (put key value map)
  (if (isEmpty map)
      (cons (list key value) '())
      (if (eqv? (caar map) key)
          (cons (list key value) (cdr map))
          (cons (car map) (put key value (cdr map))))))

;; remove : key x map -> map
;; takes a map and a key as input and remove 
;; the binding of key from the map. 
(define (remove key map)
  (if (isEmpty map)
      (report-no-key-found key)
      (if (eqv? (caar map) key)
          (cdr map)
          (cons (car map) (remove key (cdr map))))))

;; size : map -> number
;; takes a map as input and yeilds the number of 
;; key-value pairs in the map as the output. 
(define (size map)
  (if (isEmpty map)
      0
      (+ 1 (size (cdr map)))))

;; auxillary functions
(define report-no-key-found 
  (lambda (errmsg)
    (eopl:error "Key ~s not found." errmsg)))

;; test of the map functions. 
;; an function for the testing purposes. 
(define (test execa expected)
  (display execa)
  (display " vs. ")
  (display expected)
  (display " ... ")
  (if (equal? execa expected)
      (display "passed\n")
      (display "failed\n")))

;; test cases. 
(test (map) '())
(test (clear (map)) '())
(test (put 'key 'value (map)) '((key value)))
(test (clear (put 'key 'value (map))) '())
(test (put 'key2 'value2 (put 'key1 'value1 (map))) '((key1 value1) (key2 value2)))
(test (containsKey 'key (put 'key 'value (map))) #t)
(test (containsValue 'value (put 'key 'value (map))) #t)
(test (equals (map) (map)) #t)
(test (equals (put 'key 'value (map)) (put 'key 'value (map))) #t)
(test (equals (put 'key 'value1 (map)) (put 'key 'value2 (map))) #f)
(test (get 'key (put 'key 'value (map))) 'value)
(test (isEmpty (map)) #t)
(test (remove 'key (put 'key 'value (map))) '())
(test (remove 'key (put 'key 'value (put 'key1 'value1 (map)))) '((key1 value1)))
(test (size (put 'key 'value (map))) 1)
(test (size (remove 'key (put 'key 'value (map)))) 0)