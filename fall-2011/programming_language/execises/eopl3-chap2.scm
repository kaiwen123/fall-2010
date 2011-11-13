#lang eopl
(require racket/list)
;; a recursive definition of natural numbers. 

;; initial number 0.
(define (zero)
  0)

;; test if number is zero. 
(define (is-zero? n)
  (= n (zero)))

;; successor : number -> number
(define (successor n)
  (+ n 1))

;; predecessor : number -> number 
(define (predecessor n)
  (- n 1))

;; plus 
(define (plus x y)
  (if (is-zero? x)
      y
      (successor (plus (predecessor x) y))))

;; minus
(define (minus x y)
  (if (is-zero? y)
      x
      (predecessor (minus x (predecessor y)))))

;; multiply 
(define (multiply x y)
  (if (is-zero? y)
      0
      (plus x (multiply x (minus y 1)))))

;; test multiply 
(multiply 10 100)

;; test minus
(minus 100 10)

;; test the plus function. 
(plus 10 20)


;; =================================
;; natural number representation using 
;; counting. -- unary represenation. 
;; =================================
(define zero1
  (lambda () '()))

(define is-zero1?
  (lambda (n) 
    (null? n)))

(define successor1 
  (lambda (n) 
    (cons #t n)))

(define predecessor1
  (lambda (n) 
    (cdr n)))


;; execise 2.1 
;; bigit representation of natural number. 
(define zero2
  (lambda () '()))

;; test if give number is zero. 
(define is-zero2?
  (lambda (n) 
    (null? n)))

(define N 16)

;; change from base 10 to base N for number. 
(define tobignum
  (lambda (n)
    (if (= 0 n)
        (zero2) 
        (cons 
         (remainder n N) 
         (tobignum (/ (- n (remainder n N)) N))))))

;; transform from bignumber to decimal presentation. 
;; frombignum : alon -> num
(define (frombignum alon)
  (let ((e 0))
    (letrec  
        ((func
          (lambda (e alon)
            (if (null? alon) 
                0
                (+
                 (* (car alon) (expt N e))
                 (func (+ e 1) (cdr alon)))))))
         (func e alon))))

;; the following two functions used an intermediate 
;; representation of a number to be decimal. Then 
;; translate them back after calculations.
(define successor2 
  (lambda (alon)
    (tobignum
    (successor (frombignum alon)))))
(define predecessor2
  (lambda (alon)
    (tobignum
     (predecessor (frombignum alon)))))

;; factorial : n -> n
(define (factorial n) 
  (let ((alon (tobignum n)))
    (letrec ((func 
              (lambda (m)
                (if (is-zero2? m)
                    1
                    (* (frombignum m) (func (predecessor2 m)))))))
      (func alon))))

;(define successor2
;  (lambda (alon)
;    (if (is-zero2? alon) 
;        '(1)
;        (if (>= (+ (car alon) 1) N)
;            (cons '() (+ (cadr alon) 1) (cddr alon))
;            (cons (+ (car alon) 1) (cdr alon))))))
;
;(define predecessor2 
;  (lambda (alon)
;    (if (and 
;         (= (length alon) 1)
;         (= (car alon) 1))
;        '()
;        (if (>= (- (car alon) 1) N)
;            (cons '() (- (cadr alon) 1) (cddr alon))
;            (cons (- (car alon) 1) (cdr alon))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Env representation using a flat list. 
;;; eg. '(extend-env a 100 empty-env), '(empty-env)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment implementations. 
(define (empty-env)
  (list 'empty-env))

;;extend-env : env x var x val -> env
(define (extend-env var val env)
  (append (list 'extend-env var val) env '()))

(define (apply-env var env)
  (cond 
      ((eqv? env 'empty-env)
       (report-no-binding-found var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cdddr env)))
         (if (eqv? var saved-var)
             saved-val
             (begin (display saved-env)
                    (apply-env var saved-env)))))
      (else
       (report-invalid-env env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding found for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    ((eopl:error 'apply-env "Bad environment: ~s" env))))
    
;; test env
(empty-env)
(extend-env 'a '100 (empty-env))
(apply-env 'm (extend-env 'n '2000 (extend-env 'm '100 (empty-env))))


;; execise 2.5. a-list or association-list representation. 
(define empty-env1 
  (lambda () '()))

(empty-env1)

(define extend-env1
  (lambda (var val env)
    (cons (list var val) env)))

(extend-env1 'a '1000 (empty-env1))

(define apply-env1 
  (lambda (var env)
    (if (null? env)
        (report-no-binding-found)
        (if (eqv? (caar env) var)
            (cdar env)
            (apply-env1 var (cdr env))))))

(define empty-env1?
  (lambda (env)
    (if (null? env)
	#t
	#f)))

;; test the empty-env1. 
(empty-env1? (empty-env1))

;; has-binding? : env x var -> bool
;; test if var has binding in env. 
(define (has-binding? var env)
  (if (empty-env1? env) 
      #f
      (or (if (eqv? (caar env) var)
	      #t 
	      (has-binding? var (cdr env))))))

;; test bining of variable. 
(has-binding? 'a (extend-env1 'a '100 (empty-env1)))

;; extend-env* which puts variables and values into different lists.
;; all the values will be unique values. 
;; this function extend the existing env by inserting a new var/val
;; pair, if the var already exist in the provided env, then, simply
;; update the value of the existing var, or if the var does not exist
;; , add it into the env. 
(define (update-env var val env)
  (if (null? env) 
      (cons (list var val) '())
      (if (eqv? (caar env) var) 
	  (cons (list var val) (cdr env))
	  (cons (car env) (update-env var val (cdr env))))))
		

(update-env 'c '100 '((a 10) (b 12) (c 23) (d 34)))

(define (extend-env* alovar aloval env) 
  (letrec ((func
	    (lambda (alor alol) 
	      (if (or (null? alor) (null? alol)) 
		  env
		  (begin
		    (set! env (update-env (car alor) (car alol) env))
		    (func (cdr alor) (cdr alol)))))))
    (func alovar aloval)))

;; test the program above. 
(extend-env* '(a b c d m a ) '(10 12 20 80 87 76) '((a 100) (b 38)))

;; execise 2.11. 
;; extend-env* using the ribcage representation. 
(define (extend-env** vars vals env) 
  (if (null? vars) 
      env
      (cons (list vars vals) env)))

;; test the rigcage implementations.
(extend-env** 
 '(m) '(98) 
 (extend-env** 
  '(a b c) '(19 28 39) 
  '(((m n) (8 9)))))
 

;; procedural representation of the environment.
(define empty-env2 
  (lambda () 
    (lambda (search-var) 
      (report-no-binding-found search-var))))
;; ((empty-env2) 'a)

(define extend-env2
  (lambda (var val env)
    (lambda (search-var)
      (if (eqv? search-var var)
          val
          (apply-env2 env search-var)))))

(define apply-env2 
  (lambda (env search-var)
    (env search-var)))

(apply-env2 (extend-env2 'a '100 (empty-env)) 'a)
;; execise 2.13. extend-env to return two functions. 
;; add one more observer empty-env? to the env data structure
;; so each constructor must return two functions, one for
;; empty-env? and other for apply-env. 
(define empty-env3
  (lambda ()
    (vector 
     (lambda (env)
       #t)
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda (search-var search-val)
       #f))))
       
(define extend-env3
  (lambda (var val env)
    (vector 
     (lambda (env)
       #f)
     (lambda (search-var)
       (if (equal? var search-var)
           val
           (apply-env3 search-var)))
     (lambda (search-var search-val)
       (if (and (eqv? var search-var)
                (eqv? val search-val))
           #t
           (has-binding3? env search-var search-val))))))

(define apply-env3
  (lambda (env search-var)
    ((vector-ref env 1) search-var)))

(define empty-env3?
  (lambda (env)
    ((vector-ref env 0) env)))

;; execise 2.14.
(define has-binding3?
  (lambda (env var val)
    ((vector-ref env 2) var val)))

;; execise 2.4 specification of stack data structure. 
;; operations supported by stack. 
;; empty-stack, push, pop, top, empty-stack?. 
;; (empty-stack) = 
;; (push element stack) -> extended stack. 
;; (pop stack) -> reduced stack. 
;; (top stack) -> element
;; (empty-stack?) -> boolean. 
;; constructors: empty-stack, push and pop. 
;; observers: top
(define empty-stack
  (lambda ()
    '()))

(define empty-stack? 
  (lambda (stack)
    (null? stack)))

(define push 
  (lambda (element stack)
    (cons element stack)))

(define pop
  (lambda (stack)
    (if (empty-stack? stack)
	(report-stack-empty-error) 
	(begin 
	  (display (car stack))
	  (set! stack (cdr stack))))))

(define top
  (lambda (stack) 
    (if (empty-stack? stack)
	(report-stack-empty-error)
	(car stack))))

(define report-stack-empty-error
  (lambda ()
    (eopl:error "Stack empty")))

;; test over the stack implementations. 
(define stack (empty-stack))
(define stack1 (push 'b (push 'a stack)))
(display (pop stack1))
(display (top stack1))
(empty-stack? stack1)
(display (top stack1))

;; top is the only observer for this data structure, and the others 
;; are constructors which means they can change the content of stack.

;; execise 2.12 implement the stack using procedural representation. 
;; empty-stack --> constructor. 
;; push1 --> constructor.
;; push1(element stack) --> element[Stack] --> new stack created. 
;; pop1 --> observer.  --> no new stack created. 
;; (top (empty-stack)) = error. 
(define empty-stack1
  (lambda () 
    (lambda (msg)
      (report-empty-stack-error msg))))

;;; report stack errors. 
(define report-empty-stack-error
  (lambda (msg)
    (eopl:error "Empty stack error ~s~" msg)))

;; push --> constructor. 
;; (top push(ele stack)) = ele. 
;; push1 should return a function that can be used 
;; by two observers, so we need to identifier each
;; observer using some information. 
(define push1
  (lambda (element stack)
    (lambda (msg)
      (cond 
        ((equal? msg "pop") stack)
        ((equal? msg "top") element)
        (else 
         (eopl:error "error stack operation."))))))

;; pop --constructor.
(define pop1
  (lambda (stack msg)
    (stack msg)))

;; top --> observer. 
(define top1
  (lambda (stack msg)
    (stack msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; even or odd number. 
(letrec ((even?
	 (lambda (n) 
	   (if (= n 0)
	       #t
	       (odd? (- n 1)))))
	 (odd? 
	  (lambda (n)
	    (if (= n 0)
		#f
		(even? (- n 1))))))
  (even? 101))

;; occurs free of variable. 
;(define occurs-free?
;  (lambda (search-var exp)
;    (cases lc-exp exp
;      (var-exp (va)
   
;;; execise 2.15. 
;;; this execise implements the lambda calculas expression 
;;; lc-exp ::= Identifier. 
;;; lc-exp ::= (lambda (Identifier) lc-exp)
;;; lc-exp ::= (lc-exp lc-exp)
;; constructors.
(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp 
   (var identifier?))
  (lambda-exp 
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp 
   (rator lc-exp?)
   (rand lc-exp?)))

(define var-exp->var
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (else (display "error")))))

(newline)
(display "testing for lc-exp extractors.\n")
(display (var-exp->var (var-exp 'var-exp->var.test)))
(newline)

(define lambda-exp->bound-var 
  (lambda (exp)
    (cases lc-exp exp
      (lambda-exp (bound-var body) bound-var)
      (else (display "lambda-exp->bound-var error")))))

;;(display (lambda-exp->bound-var (lambda-exp ('a 'b))))

(define lambda-exp->body
  (lambda (exp)
    (cases lc-exp exp
      (lambda-exp (bound-var body) body)
      (else (display "lambda-exp->body error")))))

(define app-exp->rator 
  (lambda (exp)
    (cases lc-exp exp
      (app-exp (rator rand) rator)
      (else (display "app-exp->rator error")))))

(define app-exp->rand
  (lambda (exp)
    (cases lc-exp exp
      (app-exp (rator rand) rand)
      (else (display "app-exp->rator error")))))

;;; execise 2.18. 
;; number->sequence : n -> sequence. 
(define (num->seq n)
  (list n '() '()))

(define (current-element ns)
  (car ns))

(define (move-to-left ns)
  (let ((num (car ns))
        (pre-list (cadr ns))
        (post-list (caddr ns)))
    (list (car pre-list) (cdr pre-list)
          (cons num post-list))))

(define (move-to-right ns)
  (let ((num (car ns))
        (pre-list (cadr ns))
        (post-list (caddr ns)))
    (list (car post-list) (cons num pre-list)
          (cdr post-list))))

(define (insert-to-left n ns)
  (let ((num (car ns))
        (pre-list (cadr ns))
        (post-list (caddr ns)))
    (if (not (equal? n (car pre-list)))
        (list num (cons n pre-list) post-list)
        (display "Error.... element exists."))))

(define (insert-to-right n ns)
  (let ((num (car ns))
        (pre-list (cadr ns))
        (post-list (caddr ns)))
    (if (not (equal? n (car post-list)))
        (list num pre-list (cons n post-list))
        (display "Error.... element exists."))))

(define (at-left-end? n ns)
  (let ((left-end (cadr ns)))
    (letrec ((check-member
              (lambda (n alon)
                (if (null? alon)
                    #f
                    (if (equal? n (car alon))
                        #t
                        (check-member n (cdr alon)))))))
      (check-member n left-end))))

(define (at-right-end? n ns)
  (let ((right-end (caddr ns)))
    (letrec ((check-member
              (lambda (n alon)
                (if (null? alon)
                    #f
                    (if (equal? n (car alon))
                        #t
                        (check-member n (cdr alon)))))))
      (check-member n right-end))))

;;; testing. 

(display "\nnum->seq...  ")
(display (num->seq 7))

(display "\nOriginal seq...  ")
(display '(6 (5 4 3 2 1) (8 9 10)))


(display "\ncurrent-element...  ")
(display (current-element '(6 (5 4 3 2 1) (8 9 10))))

(display "\nmove-to-left...  ")
(display (move-to-left '(6 (5 4 3 2 1) (8 9 10))))

(display "\nmove-to-right...  ")
(display (move-to-right '(6 (5 4 3 2 1) (8 9 10))))

(display "\ninsert-to-right...  ")
(display (insert-to-right 100 '(6 (5 4 3 2 1) (8 9 10))))

(display "\ninsert-to-right...  ")
(display (insert-to-right 999 '(6 (5 4 3 2 1) (8 9 10))))

(display "\nat-right-end?...  ")
(display (at-right-end? 999 '(6 (5 4 3 2 1) (8 9 10))))

(display "\nat-left-end?...  ")
(display (at-left-end? 1 '(6 (5 4 3 2 1) (8 9 10))))
