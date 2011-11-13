#lang eopl

;;; this is 2010 spring midterm for programming languages. 
(define (countNumbersSymbols lst)
  (if (null? lst)
      (list 0 0)
      (letrec ((count
                (lambda (predicate lst)
                  (if (null? lst)
                      0
                      (let ((fst (car lst)) (rst (cdr lst)))
                        (if (pair? fst) 
                            (+ (count predicate fst) (count predicate rst))
                            (if (predicate fst) 
                                (+ 1 (count predicate rst))
                                (count predicate rst))))))))
        (list (count number? lst) (count symbol? lst)))))

;; tests. 
(countNumbersSymbols '((())))
(countNumbersSymbols '(a))
(countNumbersSymbols '(2 56 x (1 1)))
(countNumbersSymbols '(((a)) -2 (2 (a) (-1 0 1))))
(countNumbersSymbols '(a b c d 8 9 0))

;;; 2. binary tree. 
(define-datatype bintree bintree? 
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (bintree-to-list btree)
  (cases bintree btree
    (leaf-node (num) (list 'leaf-node num))
    (interior-node (key left right)
                   (list 'interior-node key (bintree-to-list left) (bintree-to-list right)))))

;; test for the bintree definition. 
(display (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))))
(newline)
(display (bintree-to-list (interior-node 'a (interior-node 'b (leaf-node 5) (leaf-node 3)) (interior-node 'c (leaf-node 6) (leaf-node 4)))))


;;; 3. parse-expression for lc-exp. 
