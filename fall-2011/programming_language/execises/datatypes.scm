#lang racket

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))
  
;; page 48: alternate definition
(define-datatype s-list-alt s-list-alt?
  (an-s-list
   (sexps (list-of s-exp?))))

;; For exercises 2.24-2.25
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;;; an expressed value is either a number, a boolean or a procval.
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?)))

(define-datatype program program? 
  (a-program 
   (exp1 expression?)))
;;; expression data type. 
(define-datatype expression expression? 
  (const-exp 
   (num number?))
  (diff-exp 
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp 
   (exp1 expression?))
  (if-exp 
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp 
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (proc-exp 
   (var identifier?)
   (body expression?))
  (call-exp 
   (rator expression?)
   (rand expression?))
  (letrec-exp 
   (p-name identifier?)
   (b-var identifier?)
   (p-body expression?)
   (letrec-body expression?)))

;; proc? : SchemeVal -> Bool
;; procedure : Exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   ;; in LEXADDR, bound variables are replaced by %nameless-vars, so
   ;; there is no need to declare bound variables.
   ;; (bvar symbol?)
   (body expression?)
   ;; and the closure contains a nameless environment
   (env nameless-environment?)))