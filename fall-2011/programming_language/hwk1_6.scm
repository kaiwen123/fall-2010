#lang eopl
; Exercies 6. EOPL3 Exercise 2.30. 
; 
; This procedure translates one grammer to another grammer. 
; parse-expression : SchemeVal → LcExp
; Definition of the lambda calculus expressions. 

;; data definitions -- obtained from the eopl3.com website.
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

;; parse-expression : Schemeval -> Lcexp
;; page 53, parses lambda calculus expressions into abstract syntax.
;; errors will be reported on invalid inputs. 
(define parse-expression
  (lambda (datum)
    (cond
      ((null? datum) (report-syntax-error datum "null-exp-error"))
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (cond
             ((= (length datum) 1)
              (report-syntax-error datum "incomplete-lc-exp-error"))
             ((= (length datum) 2)
              (report-syntax-error datum "incomplete-lc-exp-error"))
             ((= (length datum) 3)
              (if (and (= (length (cadr datum)) 1) (identifier? (car (cadr datum))))
                  (lambda-exp                 
                   (car (cadr datum))
                   (parse-expression (caddr datum)))
                  (report-syntax-error datum "lc-exp-parameter-number-error")))
             (else 
              (report-syntax-error datum "extra-lc-exp-error")))
           (cond
             ((= (length datum) 1)
              (report-syntax-error datum "missing-operand-error"))
             ((= (length datum) 2)
              (app-exp
               (parse-expression (car datum))
               (parse-expression (cadr datum))))
             (else 
              (report-syntax-error datum "extra-operand-error")))))
      (else (report-syntax-error datum "general-syntax-error")))))

; Error reporting routine, 
; app-exp error e.g.: '(a b c).
; null/nil expression error, e.g.: '()
; incomplete lambda syntax error, e.g.: '(lambda)
; invalid concrete syntax error. 
(define report-syntax-error
  (lambda (datum errmsg)
    (cond 
      ((equal? errmsg "app-exp-error")
       (eopl:error "invalid application expression syntax:" datum))
      ((equal? errmsg "null-exp-error")
       (eopl:error "null expression error:" datum))
      ((equal? errmsg "incomplete-lc-exp-error")
       (eopl:error "incomplete lambda calculus error:" datum))
      ((equal? errmsg "lc-exp-parameter-number-error")
       (eopl:error "lambda calculus parameter number error:" datum))
      ((equal? errmsg "extra-lc-exp-error")
       (eopl:error "extra part(s) lambda calculus error:" datum))
      ((equal? errmsg "extra-operand-error")
       (eopl:error "app expression extra operand error:" datum))
      ((equal? errmsg "missing-operand-error")
       (eopl:error "app expression missing operand error:" datum))
      (else (eopl:error "general syntax error:" datum)))))


; valid expression parses. 
(parse-expression 'y)
(parse-expression '(lambda (x) (x y)))
(parse-expression '(lambda (y) (x y)))
(parse-expression '((lambda (x) x) (x y))) 
(parse-expression '(lambda (y) (lambda (z) (x (y z)))))
(parse-expression '(x (y z)))
(parse-expression '(x (y (z m))))

; invalid expression parses, please uncomment following lines to check 
; the work the parse-expression procedure.
;(parse-expression '(lambda))
;(parse-expression '(lambda (x)))
;(parse-expression '(lambda (x) y z))
(parse-expression '(a b c))
;(parse-expression '(a))
;(parse-expression '())

;(parse-expression '(lambda (x y) x))
;(parse-expression '(lambda () x))
;(parse-expression '(lambda ((x)) x))
