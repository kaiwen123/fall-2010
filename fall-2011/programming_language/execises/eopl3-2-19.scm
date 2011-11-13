#lang eopl

;;; binary tree representation with data structures. 
;;; Bintree ::= () | (Int Bintree Bintree)
(define (num->bintree n)
  (list n '() '()))

(display "\nnum->bintree ... ")
(display (num->bintree 10))

(define (cur-element btree)
  (car btree))

(display "\ncur-element ... ")
(display (cur-element (num->bintree 10)))

(define (insert-to-left n btree)
  (let ((root (car btree))
        (left-tree (cadr btree))
        (right-tree (caddr btree)))
    (if (null? left-tree)
        (list root (num->bintree n) right-tree)
        (list root (list n left-tree '()) right-tree))))

(display "\ninsert-to-left ... ")
(display (insert-to-left 11 (num->bintree 10)))

(define (insert-to-right n btree)
  (let ((root (car btree))
        (left-tree (cadr btree))
        (right-tree (caddr btree)))
    (if (null? right-tree)
        (list root left-tree (num->bintree n))
        (list root left-tree (list n '() right-tree)))))

(display "\ninsert-to-right ... ")
(display (insert-to-right 11 (insert-to-left 12 (num->bintree 10))))

(display "\ncompound-insertion test.... ")
(display (insert-to-right 13 (insert-to-left 14 (insert-to-left 15 (insert-to-right 11 (insert-to-left 12 (num->bintree 10)))))))

(display "\n another compound test. ... ")
(display (insert-to-left 15 (insert-to-right 14 (insert-to-left 12 (num->bintree 13)))))

(define (move-to-left btree)
  (cadr btree))

(define (move-to-right btree)
  (caddr btree))

(define t1 (insert-to-right 14 (insert-to-left 12 (num->bintree 13))))
(display "\nmove-to-left ... ")
(display (move-to-left t1))
(display "\nmove-to-right ... ")
(display (move-to-right t1))

(define (at-leaf? btree)
  (null? btree))

(display "\n at-leaf? ... ")
(display (at-leaf? (move-to-left (move-to-right t1))))


;;; execise 2.21. 
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val symbol?)
   (saved-env environment?)))

;; red and blue tree data structure. 
(define-datatype rbtree rbtree?
  (leaf-node 
   (lfnode number?))
  (red-node 
   (ltree rbtree?)
   (rtree rbtree?))
  (empty-blue-node)
  (non-empty-blue-node
   (subtrees (list-of rbtree?))))