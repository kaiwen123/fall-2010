#lang racket

(require srfi/41) 

;; a filter function that filters lists with given 
;; predicate. 
;; filter : pred x seq -> seq-new
;; example: (filter even? '(1 2 3 4)) will yeild '(2 4).
(define (filter pred seq)
  (cond 
    ((null? seq) '())
    ((pred (car seq)) 
     (cons (car seq) (filter pred (cdr seq))))
    (else (filter pred (cdr seq)))))

(define (accumulate op init seq )
  (if (null? seq)
      init 
      (op (car seq) (accumulate op init (cdr seq)))))
;; test case for accumulate. 
(display (accumulate + 0 '(1 2 3)))
(newline)
(display (accumulate * 1 '(1 2 3 4)))
(newline )

;;; interval of numbers. 
;; nint : low x high -> seq
(define (nint low high)
  (if (= low high) 
      (list low)
      (cons low (nint (+ low 1) high))))
(display (nint 2 10))
(newline)
;; combine accumulate and interval sequences. 
(display (accumulate + 0 (nint 0 100)))
(newline)

;; calculating the fibonacci number. 
(define (fib n)
  (let ((fa 1) (fb 1))
    (letrec ((fibb
              (lambda (f1 f2 count)
                (if (= 1 count) (list f1)
                    (cons f1 (fibb f2 (+ f1 f2) (- count 1)))))))
      (fibb 1 1 n))))
;; test case. 
(display (fib 10))
(newline)

(display "Testing for fibonacci numbers. ")
;; even fibonacci number sequences. 
(define (even-fib n)
  (accumulate cons '()
              (filter even? 
                      (map +
                           (nint 0 n)))))
;; test. 
(display (even-fib 10))

;;;;;;;;;;;;;;;;; streams ;;;;;;;;;;;;;;;;
(define stream-zeros
  (stream-cons 0 ((lambda () stream-zeros))))
;; test 
stream-zeros
(newline)

(define (numbers-from n)
  (stream-cons n
        ((lambda () (numbers-from (+ 1 n))))))

;; test numbers-from
(numbers-from 10)
(newline)

(define stream-numbers (numbers-from 0))
(newline)

(define the-empty-stream '())

(define (stream-interval low high)
  (if (> low high) the-empty-stream
      (stream-cons low 
                   (stream-interval (+ low 1) high))))

;; test for stream-intervals.
(display (stream-car (stream-cdr (stream-interval 3 5))))
(newline)

;;; stream-filter : pred x stream -> list
(define stream-filter1
  (lambda (pred s)
    (cond 
      ((pred (stream-car s))
       (stream-cons (stream-car s)
                    (stream-filter1 pred (stream-cdr s))))
      (else (stream-filter1 pred (stream-cdr s))))))

;;; first-n : n x s -> list
;;; get the first n elements of the stream as a list.
(define first-n
  (lambda (n s)
    (let ((res '()))
      (if (= 1 n)
          (list (stream-car s))
          (append res (cons (stream-car s) (first-n (- n 1) (stream-cdr s))))))))

;; testing for stream fitler.
(display "Testing for get the first n element from stream.\n")
(first-n 10 (stream-filter1 even? (stream-interval 2 100)))

(first-n 10 (stream-interval 1 1000))

;;; fibonacci numbers. 
(define (fibgen a b)
  (stream-cons a 
        ((lambda () (fibgen b (+ a b))))))

(define fibs (fibgen 0 1))

;;;;; even integers ;;;;;; 
(define even-integers
  (lambda (n)
    (stream-cons n
                 (even-integers (+ 2 n)))))
  
(first-n 20 (even-integers 0))

;(define (multiples ints m n)
;  (let ((fst (stream-car ints))
;        (rst (stream-cdr ints)))
;  (if (or (equal? (remainder fst m) 0) 
;          (equal? (remainder fst n) 0))
;      (stream-cons fst (multiples rst m n)))))

(define (isMultiple m n)
  (lambda (num)
    (if (or (equal? (remainder num m) 0) (equal? (remainder num n) 0)) 
        #t
        #f
        )))
;((isMultiple 7 3) 14)
;gets stream as input and checks if it's isMultiple, and continues
(define (multiples st m n )
  (stream-filter (isMultiple m n) st))

(define (iseven)
  (lambda (m)
    (even? m)))

(define (evens st)
  (stream-filter (iseven) st))

(first-n 30 (evens (numbers-from 0)))
    
(first-n 10 (multiples (numbers-from 0) 3 7))

;;;;;;;;;;;; integers. ;;;;;;;;;;;;;;;
(display "Integer streams......")
(define (integers n)
  (stream-cons n (integers (+ n 1))))
(first-n 20 (integers 0))
(newline)

(display "Even integers......")
(define even-ints 
  (stream-filter even? (integers 0)))
(first-n 20 even-ints)
(newline)

(display "Integer partial sum streams......")
;(define (partial-sums stream)
;  (stream-filter
;   (lambda (n)
;     (letrec ((ff
;               (lambda (n1)
;                 (cond 
;                   ((or (equal? n1 0) (equal? n1 1)) n1)
;                   (else (+ n1 (ff (- n1 1))))))))
;       (ff n))) stream))
(define (partial-sums stream)
  (stream-cons 
   (stream-car stream)
   (partial-sums 
    (stream-cons 
     (+ (stream-car stream) (stream-car (stream-cdr stream))) 
     (stream-cdr (stream-cdr stream))))))
                
(first-n 10 (partial-sums (integers 0)))
(newline)

;;; factorial streams and partial sum streams.
(display "Factorial streams and partial sum streams......\n")
(define (factgen a b) ;; factorials. 
  (stream-cons a (factgen (* a b) (+ b 1))))
(define (ps a b) ;; partial sums. 
  (stream-cons a (ps (+ a b) (+ b 1))))
(define (fibn a b) ;; fibonacci number streams. 
  (stream-cons a (fibn b (+ a b))))
 
(first-n 10 (factgen 1 1))
(first-n 10 (ps 0 1))
(first-n 10 (fibn 0 1))

;(display (run
;            "letrec mult(x) =
;                             proc (y) 
;                                  if zero?(y) then 0
;                                  else -(((mult x) -(y,1)), -(0,x))
;              in 
;              letrec fact(n) = if zero?(n) then 1
;                               else ((mult (fact -(n,1))) n)
;                  in (fact 4)
;
;              "))

;9.18
;all you need is to change in classes.scm
;
;;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
;  ;; Page: 345
;  #|(define merge-method-envs
;    (lambda (super-m-env new-m-env)
;      (append new-m-env super-m-env)))|#
;  (define merge-method-envs
;    (lambda (super-m-env new-m-env)
;      (begin (display (remove-duplicates (append super-m-env new-m-env))) 
;             (remove-duplicates (append super-m-env new-m-env)))))
;  (define remove-duplicates
;    (lambda (menv)
;      (if (null? menv) '()
;      (let ((m-name (caar menv)) 
;             (rest (cdr menv)) 
;             (rest-names (map (lambda (x) (car x)) (cdr menv)))
;             )
;         (if (memq m-name rest-names)
;             (remove-duplicates rest)
;             (append (list (car menv)) (remove-duplicates rest)))
;      ))))
;
; 
;;Test cases at top.scm
; (display (run
;            "class c1 extends object
;              field i
;              
;              method initialize(x) set i = x 
;              method m1 () i
;             
;             class c2 extends c1
;             method m1 () 2
;            let o = new c1(20) in send o m1()"))
;
;
;====================================================
;====================================================
;====================================================
;
;Solution to 9.16
;
;changes required in classes.scm and interp.scm
;
;In classes.scm
;
;;; find-method : Sym * Sym -> Method
;  ;; Page: 345
;  #|(define find-method
;    (lambda (c-name name)
;      (let ((m-env (class->method-env (lookup-class c-name))))
;        (let ((maybe-pair (assq name m-env)))
;          (if (pair? maybe-pair) (cadr maybe-pair)
;            (report-method-not-found name))))))
;|#
;  (define find-method
;    (lambda (c-name name size)
;      (let ((m-env (class->method-env (lookup-class c-name))))
;        (let ((maybe-pair (assq name m-env)))
;          (if (pair? maybe-pair) 
;              (search-in-menv name size m-env)
;            (report-method-not-found name))
;          
;          
;          ))))
;(define search-in-menv 
;  (lambda (name size menv)
;    (if (null? menv)
;        (report-method-not-found name)
;    (let ((m-name (caar menv)) (meth (cadar menv)))
;      (if (equal? m-name name)
;          (cases method meth
;            (a-method (vars body s-name f-names)
;                    (if (equal? (length vars) size)
;                        meth
;                        (search-in-menv name size (cdr menv)
;                        ))))
;          (search-in-menv name size (cdr menv)
;                        ))
;                    
;    ))))
;    
;    
;  (define report-method-not-found
;    (lambda (name)
;      (eopl:error 'find-method "unknown method ~s" name)))
;  
;  ;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
;  ;; Page: 345
;
;  ===>
;
;Changes in interp.scm - just passing size as the last parameter
;
;        ;; new cases for CLASSES language
;
;        (new-object-exp (class-name rands)
;          (let ((args (values-of-exps rands env))
;                (obj (new-object class-name)))
;            (apply-method
;              (find-method class-name 'initialize (length rands))
;              obj
;              args)
;            obj))
;
;        (self-exp ()
;          (apply-env env '%self))
;
;        (method-call-exp (obj-exp method-name rands)
;          (let ((args (values-of-exps rands env))
;                (obj (value-of obj-exp env)))
;            (apply-method
;              (find-method (object->class-name obj) method-name (length rands))
;              obj
;              args)))
;      
;        (super-call-exp (method-name rands)
;          (let ((args (values-of-exps rands env))
;                (obj (apply-env env '%self)))
;            (apply-method
;              (find-method (apply-env env '%super) method-name (length rands))
;              obj
;              args)))        
;        )))
;
;  ;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
;
;==============>
;Test cases top.scm
;
;
;(display (run
;            "class c1 extends object
;              field i
;              method initialize() 1
;              method initialize(x) set i = x 
;              method m1 () i
;              method m1 (i)i
;              method m1 (i,j) list(i,j)
;            let o = new c1(20) in send o m1()"))
;
;
;=====================>
;
;
;
;Good luck Guys!!!!
;
;30 - Closure and Streams
;20 - Attribute Grammars
;30 - Classes
;20 - (wp/datatype) :)