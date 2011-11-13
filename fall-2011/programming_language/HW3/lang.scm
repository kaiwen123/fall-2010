#lang eopl
;;
;; Author : Shumin Guo (U00617724)
;; Date : 11/08/2011. 
;; Notes: Homework 3 of programming languages. 
;;
;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

;; see p384 EOPL3
(define expr-scanner
  '((whitespace (whitespace) skip)
    (identifier ((or "i" "j" "k")) symbol)))

(define expr-grammar
  '((expr (term (arbno "+" term)) an-expr)
    (term (factor (arbno "*" factor)) a-term)
    (factor (var) var-factor)
    (factor ("(" expr ")") expr-factor)
    (var (identifier) a-var)))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

;; see p387 EOPL3
(sllgen:make-define-datatypes expr-scanner expr-grammar)

(define scan&parse
  (sllgen:make-string-parser expr-scanner expr-grammar))

(define just-scan
  (sllgen:make-string-scanner expr-scanner expr-grammar))  

;;;;;;;;;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;;;;;;;;;

;;; empty-env : '()
(define empty-env
  (lambda () '()))

;;; extend-env sym x val x env -> env
(define extend-env 
  (lambda (sym val env)
    (cons (list sym val) env)))

(define apply-env 
  (lambda (var env)
    (if (null? env)
        (eopl:error "empty-env")
        (let ((fst (car env)) (rst (cdr env)))
          (if (eqv? var (car fst))
              (cadr fst)
              (apply-env var rst))))))          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; run ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This part implements an interpreter that first builds abstract syntax
;;; tree from the given grammar, then use scheme to interpret the abstract
;;; syntax tree.
;;; Usually, we need to pass an env to the evaluation program, but to
;;; make it simple here, I define env as a global variable it will used 
;;; by the value-of-var function only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define the initial environment. 
(define env 
  (extend-env 'k 8
              (extend-env 'j 4
                          (extend-env 'i 2 (empty-env)))))

;; running the program. run : string -> ExpVal. 
(define run 
  (lambda (string)
    (let ((ast (scan&parse string)))
      (begin
        (display (string-append "RUN: " string " -> "))
        (display (value-of-expr ast))
        (newline)))))

;; value-of-expr : exp -> ExpVal. 
(define value-of-expr 
  (lambda (exp) 
    (cases expr exp
      (an-expr (t1 t-lst)
               (begin 
                 (+ 
                  (value-of-term t1)
                  (if (null? t-lst)
                      0
                      (letrec ((recterm 
                                (lambda (lot) ;; lot means list of term. 
                                  (if (null? lot)
                                      0
                                      (+
                                       (value-of-term (car lot))
                                       (recterm (cdr lot)))))))
                        (recterm t-lst)))))))))


;; value-of-term : term -> ExpVal
;; evaluate the value of terms. 
;; term ::= factor 
(define value-of-term
  (lambda (tm)
    (cases term tm
      (a-term (f1 f-lst) 
              (*
               (value-of-factor f1)
               (if (null? f-lst)
                   1
                   (letrec ((recfact
                             (lambda (lof) ;; lof means list of factors.
                               (if (null? lof)
                                   1
                                   (*
                                    (value-of-factor (car lof))
                                    (recfact (cdr lof)))))))
                     (recfact f-lst))))))))

;; value-of-factor : factor -> ExpVal
;; evaluate the value of factor. 
(define value-of-factor
  (lambda (f)
    (cases factor f
      (var-factor (var) (value-of-var var))
      (expr-factor (expr) (value-of-expr expr)))))

;; value-of-var : var -> ExpVal 
;; evaluate the value of var. 
(define value-of-var
  (lambda (v)
    (cases var v
      (a-var (var) (apply-env var env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile the given language string to assembly code. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    
;; a global list to keep track of the result. 
(define assembly '())

;;; compile : string -> assembly. 
;; compiler of the program to machine code. 
(define compile 
  (lambda (string)
    (let ((ast (scan&parse string)))
      (begin
        (set! assembly '())
        (compile-expr ast)
        (display (string-append "COMPILE: " string "->"))
        (display assembly)
        (newline)
        assembly))))

;; compile-expr : exp -> . 
;;; compiles expression. 
(define compile-expr
  (lambda (exp)
    (cases expr exp
      (an-expr (t1 lot)
               (begin
                 (compile-term t1)
                 (if (not (null? lot))
                     (begin
                       (letrec ((recterm
                                 (lambda (alot)
                                   (if (not (null? alot))
                                       (begin 
                                         (compile-term (car alot))
                                         (recterm (cdr alot))
                                         (set! assembly (append assembly (list 'iadd)))
                                         )))))
                         (recterm lot)))))))))

;; compile-term : term -> . 
;;; compiles term. 
(define compile-term 
  (lambda (tm)
    (cases term tm 
      (a-term (f1 lof)
              (begin
                (compile-factor f1)
                (if (not (null? lof))
                    (begin
                      (letrec ((recfact
                                (lambda (alof)
                                  (if (not (null? alof))                                       
                                      (begin
                                        (compile-factor (car alof))
                                        (recfact (cdr alof))
                                        (set! assembly (append assembly (list 'imul)))
                                        )))))
                        (recfact lof)))))))))

;; compile-factor : factor ->
;; compiles factor. 
(define compile-factor 
  (lambda (f)
    (cases factor f
      (var-factor (var) (compile-var var))
      (expr-factor (exp) (compile-expr exp)))))

;; compile-var : var -> .
;; compiles var. 
(define compile-var 
  (lambda (v)
    (cases var v
      (a-var (var) (cond 
                     ((eqv? var 'i) (set! assembly (append assembly (list 'iload_0))))
                     ((eqv? var 'j) (set! assembly (append assembly (list 'iload_1))))
                     ((eqv? var 'k) (set! assembly (append assembly (list 'iload_2))))
                     (else ()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interpreter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interprets the compiled assembly code and generate the numeric output. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpret : assembly -> .
(define (interpret assem)
  (if (not (null? assem))
      (let ((part1 '()) (part2 assem))
        (letrec ((recitr
                  (lambda (p2)
                    (if  (not (null? p2))
                         (begin
                           (let ((v (car p2)))
                             (cond 
                               ((eqv? v 'iload_0) (set! part1 (append (list 2) part1)))
                               ((eqv? v 'iload_1) (set! part1 (append (list 4) part1)))
                               ((eqv? v 'iload_2) (set! part1 (append (list 8) part1)))
                               ((eqv? v 'iadd) (set! part1 (append (list (+ (car part1) (cadr part1))) (cddr part1))))
                               ((eqv? v 'imul)
                                (set! part1 (append (list (* (car part1) (cadr part1))) (cddr part1))))
                               (else (display "i come h er edirectly"))))
                           (recitr (cdr p2)))))))
          (recitr part2))
        (begin 
          (display (string-append "INTERPRET "))
          (display assem)
          (display " -> ")
          (display (car part1))
          (newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Testing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this module is used to test the AE0 language. 
(display "Testing run ..... \n")
(display "i = 2, j = 4, k = 8\n")
(run "i")
(run "(k + k ) * i")
(run "(i * k) + (j + i)")
(run "i + j * (((k) + j) * i)")
(display "Done!\n")

;;; test for compile 
(display "Testing Compile ...... \n")
(compile "i")
(compile "i+j")
(compile "i*j")
(compile "i+j*k")
(compile "(k+k)*i")
(compile "(i*k)+(j+i)")
(compile "i+j*(((k)+j)*i)")
(compile "i+j+k")
(display "Done!\n")

;;; test for interpret.
(display "Testing interpret ...... \n")
(interpret (compile "(((i+j)))"))
(interpret (compile "i+j"))
(interpret (compile "i+j*(((k)+j)*i)"))
(interpret (compile "((i*i)+j*(k)+j)"))
(display "Done!\n")