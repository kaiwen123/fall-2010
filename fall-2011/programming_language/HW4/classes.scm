(module classes (lib "eopl.ss" "eopl")

  (require "store.scm")
  (require "lang.scm")
  ;(require "environments.scm")

  ;; object interface
  (provide object object? new-object object->class-name object->field-names object->fields)

  ;; method interface
  (provide method method? a-method find-method find-method-modifier)
  
  ;; class interface
  (provide lookup-class the-class-env class->static-fields initialize-class-env!)
  
  ;; the scoping framework. 
  (provide the-scope get-scope set-scope get-field-modifier)
  
  ;; error reporting. 
  (provide report-unauthorized-field-access)

;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

  ;; an object consists of a symbol denoting its class, and a list of
  ;; references representing the managed storage for the all the fields. 
  
  (define identifier? symbol?)

  (define-datatype object object? 
    (an-object
      (class-name identifier?)
      (fields (list-of reference?))))

  ;; new-object : ClassName -> Obj
  ;; Page 340
  (define new-object                      
    (lambda (class-name)
      (an-object
        class-name
        (map 
          (lambda (field-name)
            (newref (list 'uninitialized-field field-name)))
          (class->field-names (lookup-class class-name))))))
  
  ;;;;;;;;;;;;;;;;; Scoping mechanism ;;;;;;;;;;;;;;;;;;;;
  
  ;; the global scope structure. 
  ;; default scope is program. in this scope only public fields and 
  ;; methods can be accessed within a class. 
  (define the-scope '(program)) 
  
  ;; get scope 
  (define get-scope 
    (lambda () 
      (car the-scope)))
  
  ;; set scope 
  (define set-scope 
    (lambda (spe) 
      (if (not (eqv? 'superclass (get-scope)))
          (set! the-scope (cons spe '())))))

;;;;;;;;;;;;;;;; methods and method environments ;;;;;;;;;;;;;;;;

  (define-datatype method method?
    (a-method
      (vars (list-of symbol?))
      (body expression?)
      (super-name symbol?)
      (field-names (list-of symbol?))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

  ;; a method environment looks like ((method-name method) ...)

  (define method-environment?
    (list-of 
      (lambda (p)
        (and 
          (pair? p)
          (symbol? (car p)) ;;; method name. used by assq function, should be first one. 
          (symbol? (cadr p)) ;;; modifier. 
          (method? (caddr p)))))) ;; body of method.

  ;; method-env * id -> (maybe method)
  (define assq-method-env
    (lambda (m-env id)
      (cond
        ((assq id m-env) => cadr)
        (else #f))))

  ;; find-method : Sym * Sym -> Method
  ;; Page: 345
  (define find-method
    (lambda (c-name name)
      (let ((m-env (class->method-env (lookup-class c-name))))
        (let ((maybe-pair (assq name m-env)))
          (if (pair? maybe-pair) (caddr maybe-pair)
            (report-method-not-found name))))))
  
  ;; find-method for final method declaration so that '() 
  ;; will be returned rather than an error emitted if no method found. 
  (define find-method-for-final
    (lambda (c-name name)
      (let ((m-env (class->method-env (lookup-class c-name))))
        (let ((maybe-pair (assq name m-env)))
          (if (pair? maybe-pair) (caddr maybe-pair)
              '())))))
  
  ;;; find-method-modifier : c-name x name -> modifier.
  (define find-method-modifier
    (lambda (c-name name)
      (if (not (null? (find-method c-name name)))
          (let ((m-env (class->method-env (lookup-class c-name))))
            (let ((maybe-pair (assq name m-env)))
              (if (not (null? maybe-pair))
                  (cadr maybe-pair)))))))
  
  ;;; find-field-modifier : c-name x name -> modifier. 
;  (define find-field-modifier 
;    (lambda (c-name name)
      
    
  (define report-method-not-found
    (lambda (name)
      (eopl:error 'find-method "unknown method ~s" name)))
  
  ;; unauthorized method call error.
  (define report-unauthorized-call
    (lambda (name)
      (eopl:error "method call operation ~s not permited!" name)))
  
  ;; field access error. 
  (define report-unauthorized-field-access
    (lambda (name)
      (eopl:error "Field access ~s failed!" name)))
  
  ;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
  ;; Page: 345
  (define merge-method-envs
    (lambda (super-m-env new-m-env)
      (append new-m-env super-m-env)))

  ;; method-decls->method-env :
  ;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
  ;; Page: 345
  (define method-decls->method-env
    (lambda (m-decls super-name field-names)
      (map
        (lambda (m-decl)
          (cases method-decl m-decl
            (a-method-decl (method-name vars body)
              ;; find all the super calls and check its super class if 
              ;; the call is proper. 
              (method-modifier-check super-name body)
              (method-final-check super-name method-name)
              (list method-name 'public
                (a-method vars body super-name field-names)))
            
            ;; added with modifiers private, public and final. 
            (a-private-method-decl (method-name vars body)
                  (method-modifier-check super-name body)
                  (method-final-check super-name method-name)
                  (list method-name 'private
                        (a-method vars body super-name field-names)))
            (a-protected-method-decl (method-name vars body)
                  (method-modifier-check super-name body)
                  (method-final-check super-name method-name)
                  (list method-name 'protected
                  (a-method vars body super-name field-names)))
            (a-final-method-decl (method-name vars body)                                 
                  (method-final-check super-name method-name)
                  (list method-name 'final
                        (a-method vars body super-name field-names)))))
        m-decls)))
  
  ;;; method-modifier-check : super-name x m-body -> (pass or error.)
  ;; This method will check with the super method call to make sure
  ;; they are not private.
  (define method-modifier-check
    (lambda (super-name m-body)
      (cases expression m-body
        (super-call-exp (method-name rands)
                        (if (eqv? (find-method-modifier super-name method-name) 'private)
                            (eopl:error "Private method in super class ~s" method-name)))
        (begin-exp (exp1 exps)
                   (letrec ((check-modifier
                             (lambda (exp-lst)
                               (if (not (null? exp-lst))
                                   (begin
                                     (cases expression (car exp-lst)
                                       (super-call-exp (method-name rands)
                                                       (if (eqv? (find-method-modifier super-name method-name) 'private)
                                                           (eopl:error "Private method in super class ~s" method-name)))
                                       (else ()))
                                     (check-modifier (cdr exp-lst)))))))
                     (check-modifier (cons exp1 exps))))
        (else ()))))
  
  ;;; check if a defined method has final method defined in super class? 
  (define method-final-check
    (lambda (s-name m-name)
      (let ((m (find-method-for-final s-name m-name)))
        (if (and (not (null? m)) (eqv? (find-method-modifier s-name m-name) 'final))
            (eopl:error "Error: trying to overload a final function ~s" m-name)))))
  
  ;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

  (define-datatype class class?
    (a-class
      (super-name (maybe symbol?))
      (field-names-with-modifiers (list-of list?)) ;;(field-names (list-of symbol?))
      (method-env method-environment?)
      (static-field-names (list-of list?))))
  
  #|
   Examples of field-names-with-modifiers.
   ((f1 f2 f3)(public private protected))
   A function get-field-modifier can be used to obtain the modifier
   of a field.
   Static fields, because of their special semantics, I put it 
   to the end of the class structure. 

   We need to map the class declarations into the class datatype. 
   Example of parsed class structure is: 
   (a-program
    (list (a-class-decl 'c1 'object 
          '("static" "protected" "private" "protected") 
          '(f11 f12 f13 f14) '()))
     (let-exp '(o) (list (new-object-exp 'c1 '())) (const-exp 11)))
  |#

  ;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

  ;; the-class-env will look like ((class-name class) ...)

  ;; the-class-env : ClassEnv
  ;; Page: 343
  (define the-class-env '())

  ;; add-to-class-env! : ClassName * Class -> Unspecified
  ;; Page: 343
  (define add-to-class-env!
    (lambda (class-name class)
      (set! the-class-env
        (cons
          (list class-name class)
          the-class-env))))

  ;; lookup-class : ClassName -> Class
  (define lookup-class                    
    (lambda (name)
      (let ((maybe-pair (assq name the-class-env)))
        (if maybe-pair (cadr maybe-pair)
          (report-unknown-class name)))))

  (define report-unknown-class
    (lambda (name)
      (eopl:error 'lookup-class "Unknown class ~s" name)))
      
  ;; constructing classes

  ;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
  ;; Page: 344
  (define initialize-class-env!
    (lambda (c-decls)
      (set! the-class-env 
        (list
          (list 'object (a-class #f '(()()) '() '(()()))))) ;; added '() for s-fields for hw5.
      (for-each initialize-class-decl! c-decls)))

  ;; initialize-class-decl! : ClassDecl -> Unspecified
  (define initialize-class-decl!
    (lambda (c-decl)
      (cases class-decl c-decl        
        (a-class-decl (c-name s-name f-modifiers f-names m-decls) ;; added f-modifiers for hwk4-5. 
          ;; Because of the difference in semantic between static fields and other fields, we need to consider 
          ;; them differently, so seperate them first !!! 
          ;; filtered example ((non-static-fields)(non-static-modifiers)(static-fields))                      
          (let* ((filtered (seperate-static-fields f-modifiers f-names)) 
                (f-names
                 (append-field-names
                  (car filtered)
                  (class->field-names (lookup-class s-name))))
                (f-modifiers
                 (append-field-modifiers
                  (cadr filtered)
                  (class->field-modifiers (lookup-class s-name))))
                ;; append the store location to the static fields so that it can be easily applied
                ;; to envs during the evaluation process. --> hw5. 
                (sf-fields
                 (append-static-fields
                  (cons (caddr filtered)
                        (cons (map
                               (lambda (sf-name)
                                 (newref 0))  ;; num-val problem, can access constructor. 
                               (caddr filtered)) '())) ;;; static fields are initialized to 0. 
                  (class->static-fields (lookup-class s-name)))))
            (add-to-class-env!
              c-name
              (a-class s-name (cons f-names (cons f-modifiers '()))
                (merge-method-envs
                  (class->method-env (lookup-class s-name)) ; get env of super class.
                  (method-decls->method-env ;; get env of new class. 
                   ;; added sf-names static fields to the end of strcture so that it will not affect the 
                   ;; interpretation of methods. hw5.
                    m-decls s-name f-names)) sf-fields))))))) 

  ;; exercise:  rewrite this so there's only one set! to the-class-env.

  ;; append-field-names :  Listof(FieldName) * Listof(FieldName) 
  ;;                       -> Listof(FieldName)
  ;; Page: 344
  ;; like append, except that any super-field that is shadowed by a
  ;; new-field is replaced by a gensym
  (define append-field-names
    (lambda (new-fields super-fields)
      (append new-fields super-fields)))
  
  ;;; I am trying to use a new method to hide the variables.
#|      (cond
        ((null? super-fields) new-fields)
        (else
         (cons 
           (if (memq (car super-fields) new-fields)
             (fresh-identifier (car super-fields))
             (car super-fields))
           (append-field-names
             (cdr super-fields) new-fields))))))
 |#
  ;;; append field modifiers -- simply add them together. 
  (define append-field-modifiers
    (lambda (super-modifiers new-modifiers)
      (append new-modifiers super-modifiers)))
  
  ;;; append-static-fields 
  ;;; (append-static-fields '((a b) (1 2)) '((c d) (3 4)))
  ;;; will generate '((a b c d) (1 2 3 4))
  (define append-static-fields
    (lambda (new-static-fields super-static-fields)
      (cons 
       (append (car new-static-fields) (car super-static-fields))
       (cons 
        (append (cadr new-static-fields) (cadr super-static-fields))
        '()))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

  (define class->super-name
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names method-env sf-names);;added sf-names for hw5.
          super-name))))

  (define class->field-names
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names-with-modifiers method-env sf-names);; changed field-names to field-names-with-modifiers.
          (car field-names-with-modifiers)))))
  
  ;;; added to get modifiers of class fields.
  (define class->field-modifiers
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names-with-modifiers method-env sf-names);; changed field-names to field-names-with-modifiers.
          (cadr field-names-with-modifiers)))))
  
  ;;; get field modifier. private, public or protected.
  (define get-field-modifier 
    (lambda (c-struct field-name)
      (cases class c-struct
        (a-class (super-name field-names-with-modifiers method-env sf-names)
           (let ((f-names (car field-names-with-modifiers))
                 (f-modifiers (cadr field-names-with-modifiers)))
             ;(display f-names) (newline) (display f-modifiers) (newline) (display field-name) (newline)
             (letrec ((get-modifier
                       (lambda (fnames fmodifiers)
                         (if (null? fnames) 
                             (report-field-not-found field-name)
                             (if (eqv? field-name (car fnames))
                                 (car fmodifiers)
                                 (get-modifier (cdr fnames) (cdr fmodifiers)))))))
               (get-modifier f-names f-modifiers)))))))
  
  ;; field not found. 
  (define report-field-not-found
    (lambda (field-name) 
      (eopl:error "Field ~s not found." field-name)))
  
  ;;; sperate the static fields out to seperate list. 
  (define seperate-static-fields
    (lambda (f-modifiers f-names)
      (let ((modifiers '()) (fields '()) (s-fields '()))
        (letrec ((seperate
                  (lambda (f-ms f-ns) ; list of modifiers and names. 
                    (if (not (null? f-ms))
                        (let ((fst-m (string->symbol (car f-ms))) (fst-n (car f-ns)))
                          (if (eqv? fst-m 'static) 
                              (set! s-fields (append s-fields (cons fst-n '())))
                              (begin 
                                (set! modifiers (append modifiers (cons fst-m '())))
                                (set! fields (append fields (cons fst-n '())))))
                          (seperate (cdr f-ms) (cdr f-ns)))))))
          (begin 
            (seperate f-modifiers f-names)
            (cons fields (cons modifiers (cons s-fields '()))))))))
  
  ;; function to get static fields. 
  (define class->static-fields
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names method-env s-fields);; added s-fields for hw5.
          s-fields))))

  (define class->method-env
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names method-env s-fields);; added s-fields for hw5.
          method-env))))


  (define object->class-name
    (lambda (obj)
      (cases object obj
        (an-object (class-name fields)
          class-name))))
  
  ;;; added to obtain class structure of an object. 
  (define object->field-names
    (lambda (obj)
      (class->field-names 
       (lookup-class (object->class-name obj)))))

  (define object->fields
    (lambda (obj)
      (cases object obj
        (an-object (class-decl fields)
          fields))))

  (define fresh-identifier
    (let ((sn 0))
      (lambda (identifier)  
        (set! sn (+ sn 1))
        (string->symbol
          (string-append
            (symbol->string identifier)
            "%"             ; this can't appear in an input identifier
            (number->string sn))))))

  (define maybe
    (lambda (pred)
      (lambda (v)
        (or (not v) (pred v)))))

  )