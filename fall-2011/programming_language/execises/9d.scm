#lang racket
;; Simple interpreter of section 3.5 -- 
;; simplified from online code 3-5.scm
;; modified to do dynamic binding/scoping (exercise 3.30)

;;;;;;;;;;;;;;;; top level and tests ;;;;;;;;;;;;;;;;

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)   
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     app-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define read-eval-print 
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm))   ; wrapped to avoid load
                                        ; dependency 
    (sllgen:make-stream-parser 
      the-lexical-spec
      the-grammar)))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env))))))

(define eval-expression 
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp) ;\new4
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (let-exp (ids rands body)  ;\new3
          (eval-expression body (extend-env ids (eval-rands rands env) env)))
;; we do not need to store the creation environment anymore:
      (proc-exp (ids body)
          (closure ids body))
;; but we *do* need to pass the environment at application time:
      (app-exp (rator rands)
          (apply-procval (eval-expression rator env)
                                (eval-rands rands env) env))
      )))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;; alternative definition:  (cvarela 2003/09/25)

(define eval-rand eval-expression)


(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      )))

;; alternative definition allowing a variable number of arguments for
;; primitive operators:  (gunduz 2003/09/29)

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (apply + args))
      (subtract-prim () (apply - args))
      (mult-prim  () (apply * args))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      )))

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))
               
;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; rib-cage data type representation ;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((position (rib-find-position sym syms)))
          (if (number? position)
              (vector-ref vals position)
              (apply-env env sym)))))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;;;;;;;;;;;;;;;;;;;;; Procedure Values ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Procedural Representation ;;;;;;;;;;;;;;;;;

(define closure
  (lambda (ids body)
    (lambda (args env)
      (eval-expression body (extend-env ids args env)))))

(define apply-procval
  (lambda (proc args env)
    (if (not (procedure? proc))
	(eopl:error 'apply-procval 
		    "Attempt to apply non-procedure ~s" proc)
	(proc args env))))

;; e.g., in defined language:

(run
"
  (proc (x,y) +(x,y) 2 5)
")

(run
"
  let f = proc (y,z) +(y,-(z,5))
  in  (f 2 28)
")

;;
;; Exercise: why does not the following "curry" work with dynamic
;; binding?
;;
;(run
;"
;  let curry = proc (f) proc (x) proc (y) (f x y)
;       plus = proc (x,y) +(x,y)
;  in (((curry plus) 2) 3)
;")

(run
"
  let a = 3
  in let p = proc (x) +(x,a)
          a = 5
      in *(a,(p 2))
")

(run
"
  let makemult = proc (maker,x)
                            if x
                            then +(4,(maker maker sub1(x)))
                            else 0
  in let times4 = proc (x) (makemult makemult x)
      in (times4 3)
")

;; Exercise 3.21

(run
"
let fact = proc (maker,x)
                            if  x
                            then *(x,(maker maker sub1(x)))
                            else 1
  in let factorial = proc (x) (fact fact x)
      in (factorial 3)
")

;; Exercise 3.22

(run
"
let evenR = proc(even1,odd1,x)
                  if x
                  then (odd1 odd1 even1 sub1(x))
                  else 1
     oddR = proc(odd1,even1,x)
                  if x
                  then (even1 even1 odd1 sub1(x))
                  else 0
in let even =proc(x) (evenR evenR oddR x)
        odd = proc(x) (oddR oddR evenR x)
    in (even 6)
")

;; Exercise: can we do better than above to define recursive
;; procedures with dynamic binding?  Redefine factorial, and odd/even
;; using dynamic binding.







