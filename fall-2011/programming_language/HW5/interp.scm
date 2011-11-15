(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require "classes.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 336
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)            
      (cases program pgm
        (a-program (class-decls body) ;; class-decls contains definitions of classes. 
          (initialize-class-env! class-decls)
          (value-of body (init-env))))))
  
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 336 and 337
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) ;(deref (apply-env env var))) ;; comented original.
                 (let ((val (deref (apply-env env var))))
                   (if (number? val)
                       (num-val val)
                       val)))

        (diff-exp (exp1 exp2)
          (let ((val1
		  (expval->num
		    (value-of exp1 env)))
                (val2
		  (expval->num
		    (value-of exp2 env))))
            (num-val
	      (- val1 val2))))
        
        (sum-exp (exp1 exp2)
          (let ((val1
		  (expval->num
		    (value-of exp1 env)))
                (val2
		  (expval->num
		    (value-of exp2 env))))              
            (num-val
	      (+ val1 val2))))

        (zero?-exp (exp1)
	  (let ((val1 (expval->num (value-of exp1 env))))
	    (if (zero? val1)
	      (bool-val #t)
	      (bool-val #f))))

        (if-exp (exp0 exp1 exp2) 
          (if (expval->bool (value-of exp0 env))
            (value-of exp1 env)
            (value-of exp2 env)))

        (let-exp (vars exps body)       
	  (if (instrument-let)
	    (eopl:printf "entering let ~s~%" vars))
          (let ((new-env 
                  (extend-env 
                    vars
                    (map newref (values-of-exps exps env))
                    env)))
	      (if (instrument-let)
		(begin
		  (eopl:printf "entering body of let ~s with env =~%" vars)
		  (pretty-print (env->list new-env))
		  (eopl:printf "store =~%")
		  (pretty-print (store->readable (get-store-as-list)))
		  (eopl:printf "~%")
		  ))
	      (value-of body new-env)))

        (proc-exp (bvars body)
	  (proc-val
	    (procedure bvars body env)))

        (call-exp (rator rands)          
          (let ((proc (expval->proc (value-of rator env)))
                (args (values-of-exps rands env)))
	    (apply-procedure proc args)))

        (letrec-exp (p-names b-varss p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec** p-names b-varss p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (x e)
          (begin
            (setref!
              (apply-env env x)
              (value-of e env))))

        (list-exp (exps)
          (list-val
            (values-of-exps exps env)))

        ;; new cases for CLASSES language

        (new-object-exp (class-name rands)
          (let ((args (values-of-exps rands env))
                (obj (new-object class-name)))
            (apply-method
              (find-method class-name 'initialize)
              obj
              args)
            obj))

        (self-exp ()
          (apply-env env '%self))

        (method-call-exp (obj-exp method-name rands)
          (let* ((args (values-of-exps rands env))
                (obj (value-of obj-exp env))
                (modifier (find-method-modifier (object->class-name obj) method-name)))
            ;;; following are modified so that only public methods can be called. 
            (if (not (eqv? modifier 'public))
                (eopl:error "Non-public method call. ~s" method-name)
                (apply-method
                 (find-method (object->class-name obj) method-name)
                 obj
                 args))))
        
        ;;; named-method-call-exp for execise 9.10. 
        ;;; static dispatch which calls the specific class's specified method,
        ;;; rather than dymanically determine what class the object belong to.
        (named-method-call-exp (class-name obj-exp method-name rands)
          (let ((args (values-of-exps rands env))
                (obj (value-of obj-exp env)))
            (apply-method
              (find-method class-name method-name)
              obj
              args))) 
        
        ;;; field-ref-exp ---> for field references.
        ;;; should return the value of field. 
        ;;; to be done. 
        (field-ref-exp (obj-exp field-name)
          (let ((env1 (get-field-op-env field-name (value-of obj-exp env))))
            (value-of field-name env1)))
            
        ;;; field-set-exp ---> for field sets. 
        ;;; will set the value of the field. 
        (field-set-exp (obj-exp field-name value)
          (let ((env1 (get-field-op-env field-name (value-of obj-exp env))))
          (setref! (apply-env env1 
                              (cases expression field-name
                                (var-exp (var) var) (else 'x)));(expval->printable field-name))
              (value-of value env1))))
      
        (super-call-exp (method-name rands)
          (let ((args (values-of-exps rands env))
                (obj (apply-env env '%self)))
            (apply-method
              (find-method (apply-env env '%super) method-name)
              obj
              args)))
        )))

  ;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((new-env
                  (extend-env
                    vars
                    (map newref args)
                    saved-env)))
            (if (instrument-let)
              (begin
                (eopl:printf
                  "entering body of proc ~s with env =~%"
                  vars)
                (pretty-print (env->list new-env)) 
                (eopl:printf "store =~%")
                (pretty-print (store->readable (get-store-as-list)))
                (eopl:printf "~%")))
            (value-of body new-env)))))) 

  
  ;; apply-method : Method * Obj * Listof(ExpVal) -> ExpVal
  (define apply-method                    
    (lambda (m self args)
      (cases method m
        (a-method (vars body super-name field-names)
          (value-of body
            (extend-env vars (map newref args)
              (extend-env-with-self-and-super
                self super-name
                (extend-env field-names (object->fields self)        
                  ;;; extend env for static fields. 
                  (extend-env 
                   (car (class->static-fields
                         (lookup-class (object->class-name self))))
                   (cadr (class->static-fields
                          (lookup-class (object->class-name self))))
                  (empty-env))))))))))
  
  ;; this is for homework 4-2. field references. 
  ;; it will obtain value of field. 
   (define get-field-op-env
    (lambda (f self)
      (let* ((field-names (object->field-names self))
            (env1 (extend-env field-names (object->fields self) ;;; add all fields to env. 
                                ;;; extend env for static fields. 
                                (extend-env 
                                 (car (class->static-fields
                                       (lookup-class (object->class-name self))))
                                 (cadr (class->static-fields
                                        (lookup-class (object->class-name self))))
                                 (empty-env)))))
        ;;; setref! first will obtain store reference value from env, and then set the newval to the store location.
        env1)))

  (define values-of-exps
    (lambda (exps env)
      (map
        (lambda (exp) (value-of exp env))
        exps)))

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
  )