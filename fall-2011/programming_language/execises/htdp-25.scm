#lang racket

;; the quick sort algorithm. 
(define (quick-sort alon)
  (if (empty? alon)
      empty
      (append (quick-sort (smaller-items (cdr alon) (car alon)))
              (equal-items alon (car alon))
              (quick-sort (larger-items (cdr alon) (car alon))))))


;; items that are equal to the pivot. 
(define (equal-items alon num)
  (if (empty? alon)
      empty
      (if (= (car alon) num)
          (append (list (car alon)) (equal-items (cdr alon) num))
          (equal-items (cdr alon) num))))

;; items that are smaller than the pivot. 
(define (smaller-items alon num)
  (if (empty? alon) 
      empty
      (if (< (car alon) num)
          (append (list (car alon)) (smaller-items (cdr alon) num))
          (smaller-items (cdr alon) num))))

;; items that are larger than pivot. 
(define (larger-items alon num)
  (if (empty? alon)
      empty
      (if (> (car alon) num)
          (append (list (car alon)) (larger-items (cdr alon) num))
          (larger-items (cdr alon) num))))

(quick-sort '(10 3 10 29 3 57 2))

;; a new version of quick-sort using function abstraction. 

(define (qsort alon)
  (if (empty? alon)
      empty
      (append (qsort (filter-list < alon (car alon)))
              (filter-list = alon (car alon))
              (qsort (filter-list > alon (car alon))))))

(define (filter-list rel-op alon num)
  (if (empty? alon)
      empty
      (if (rel-op (car alon) num)
          (append 
           (list (car alon)) 
           (filter-list rel-op (cdr alon) num))
          (filter-list rel-op (cdr alon) num))))

(qsort '(10 29 3 57 2 100 5 10 8 3 2))

;; a version of gerneral quick sort that sorts the list of numbers 
;; qsort1 : predicte x alon -> sorted alon. 
;(define (qsort1 predicate alon)
;  (if (empty? alon)
;      empty
;      (append (qsort1 