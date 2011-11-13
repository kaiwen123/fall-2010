#lang eopl

;; occurs free. 
(define occurs-free?
  (lambda (var exp)
    (cond 
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and 
        (not (eqv? var (car (cadr exp))))
        (occurs-free? var (caddr exp))))
      (else 
       (or 
        (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))))

;;; p is the environment. 

(value-of (const-exp (num)) p) 
= n

(value-of (if-exp (exp1 exp2 exp3)) p)
= (if (expval->val (value-of exp1 p))
      (value-of exp2 p)
      (value-of exp3 p))

(value-of (var-exp (var) p))
= (apply-env p var)

(value-of (let-exp (var exp1 body)) p)
= (value-of body [var=(value-of exp1 p) p])

(value-of (diff-exp (exp1 exp2)) p) 
= (num-var (- (expval-num (value-of exp1 p) )
              (expval-num (value-of exp2 p))))

(value-of (zero?-exp exp) p)
= (if (= (expval->num exp p) 0)
      #t
      #f)

(value-of (proc-exp vars body) p)
= (proc-val vars body p)

(value-of 