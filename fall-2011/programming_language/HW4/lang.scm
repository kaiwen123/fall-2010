(module lang (lib "eopl.ss" "eopl")

  ;; grammar for the CLASSES language.  Based on IMPLICIT-REFS, plus
  ;; multiple-argument procedures, multiple-declaration letrecs, and
  ;; multiple-declaration lets. 
  
  (require "drscheme-init.scm")
  
  (provide (all-defined))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
        (letter (arbno (or letter digit "_" "-" "?")))
        symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      (modifier ((or "static" "public" "protected" "private" "final")) string) ;; added for homework4-5.
      ))
  
  (define the-grammar
    '((program ((arbno class-decl) expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)

      (expression
        ("+" "(" expression "," expression ")")
        sum-exp)
      
      (expression
        ("zero?" "(" expression ")")
        zero?-exp)

      (expression
        ("if" expression "then" expression "else" expression)
        if-exp)

      (expression (identifier) var-exp)

      (expression
        ("let" (arbno identifier "=" expression) "in" expression)
        let-exp)   

      (expression
        ("proc" "(" (separated-list identifier ",") ")" expression)
        proc-exp)

      (expression
        ("(" expression (arbno expression) ")")
        call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" (separated-list identifier ",") ")"
            "=" expression)
          "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("set" identifier "=" expression)
        assign-exp)

      (expression
        ("list" "(" (separated-list expression ",") ")" )
        list-exp)

      ;; new productions for oop
      ;; this is for homework 5 of programming languages. 
      ;; class-decl with static fields. 
      (class-decl                         
        ("class" identifier 
          "extends" identifier    
          (arbno modifier "field" identifier) ;; added static fields. hw5
          (arbno method-decl)
          )
        a-class-decl)
      
     
      ;;;;;;;;;;;;;;;;;;;;;;;Execise 9.11 Start from here;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; Modifying method to accept public, private and protected modifiers. 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; 
      ;; default method declaration. -- should be public.
      (method-decl
        ("method" identifier 
          "("  (separated-list identifier ",") ")" ; method formals
          expression
          )
        a-method-decl)
      
      ;; public method declarations.
      ;; should have the same behavior as the default case except for the 
      ;; "public" keyword. 
      (method-decl
        ("public method" identifier 
          "("  (separated-list identifier ",") ")" ; method formals
          expression 
          )
        a-method-decl)
      
      ;; private method declaration. 
      ;; only objects of *THIS* class can access its information. 
      (method-decl
        ("private method" identifier 
          "("  (separated-list identifier ",") ")" ; method formals
          expression 
          )
        a-private-method-decl)
      
      ;; protected method declaration. 
      ;; only objects of *THIS* and decendents of *THIS* class can call 
      ;; the method. 
      (method-decl
        ("protected method" identifier 
          "("  (separated-list identifier ",") ")" ; method formals
          expression 
          )
        a-protected-method-decl)
      
      ;; final method declaration.
      ;; final method can not be overriden. 
      (method-decl 
       ("final method" identifier 
          "(" (separated-list identifier ",") ")" 
          expression 
          )
       a-final-method-decl)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;End of Execise 9.10.;;;;;;;;;;;;;;;;;;;;

      (expression 
        ("new" identifier "(" (separated-list expression ",") ")")
        new-object-exp)

      ;; this is special-cased to prevent it from mutation
      (expression
        ("self")
        self-exp)

      (expression
        ("send" expression identifier
          "("  (separated-list expression ",") ")")
        method-call-exp)
      
      ;; named method call --> static dispatch.
      (expression 
       ("named-send" identifier expression identifier
                     "(" (separated-list expression ",") ")")
       named-method-call-exp)
      
      ;; field references. for homework 4-2.
      ;; the first expression should be evaluated to an object; 
      ;; and the second one should be evaluated to the field id in the 
      ;; objecct. 
      (expression
       (
       "field-ref" "(" expression "," expression ")"
             )
       field-ref-exp)
      
      ;; field set. for homework 4-2. 
      (expression 
       (
        "field-set" "(" expression "," expression "," expression ")"
                    )
       field-set-exp)

      ;;super call expression.
      (expression                                
        ("super" identifier    "("  (separated-list expression ",") ")")
        super-call-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
