#lang eopl

;; this is mid-term of 2010 fall, programming languages.
;; 1. swapper. 
(define (swapper a b lst)
  (if (null? lst)
      '()
      (let ((fst (car lst))
            (rst (swapper a b (cdr lst))))
        (if (pair? fst)
            (cons (swapper a b fst) rst)
            (cond 
              ((eqv? a fst) (cons b rst))
              ((eqv? b fst) (cons a rst))
              (else (cons fst rst)))))))

(define (swapper1 a b lst)
  (if (null? lst)
      '()
      (if (pair? (car lst))
          (cons (swapper1 a b (car lst)) (swapper1 a b (cdr lst)))
          (cond 
            ((eqv? a (car lst)) (cons b (swapper1 a b (cdr lst))))
            ((eqv? b (car lst)) (cons a (swapper1 a b (cdr lst))))
            (else 
             (cons (car lst) (swapper1 a b (cdr lst))))))))

;; test. 
(swapper 'a 'b '(a b c d a a b b))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))
(swapper1 'x 'y '((x) y (z (x))))

;; 2. define-datatype. 
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (Var symbol?)
   (Val value?)
   (Env environment?)))

(define (value? x) #t) ;; this is tricky. 

;; has-binding? : env x s -> boolean
;; adding has-binding?
(define (has-binding? env s)
  (cases environment env
    (empty-env () #f)
    (extend-env (var val env1)
                (if (eqv? s var)
                    #t
                    (has-binding? env1 s)))))

;;test for has-binding? 
(has-binding? (extend-env 'a 'b (empty-env)) 'a)


;; 4. algebraic specification of ADT. 
#|
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors. 
create()
insertOne(x, B)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Observers. 
isEmpty(B)
count(x, B)
equal(B1, B2)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-constructors. 
removeOne(x, B)
removeAll(x, B)
difference(B1 B2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algebraic specification. 
isEmpty(create()) = true
count('a, create()) = 0
equal(create(), create()) = true
removeOne(x, create()) = create()
removeAll(x, create()) = create()
difference(create(), create()) = create()

;; suppose B1 and B2 are non-empty bags. 
isEmpty(insertOne(x, B1)) = false
count(x, insert(s, B1) = if x = s then count(x, B1) + 1 else count(x, B1)
equal(insert(s1, B1), insert(s2, B2)) = if s1 = s2 equal(B1, B2) else false
removeOne(x, insert(s, B1)) = if x = s then B1 else insert(s, remove(x, B1))
removeAll(x, insert(s, B1)) = if x = s removeAll(x, B1) else insert(s, remove(x, B1))
removeAll(x, removeAll(y, B1)) = if x=y then removeAll(B1) else removeAll(y, removeAll(x, B1))
difference(B1, B1) = create() 
difference(A, B) = difference(B, A)
difference(A, insertOne(x, B)) = if count(x, A) > count(x, B) then removeOne(x, difference(A, B)) else insertOne(x, difference(A, B)) fi

equal(A, B) = isEmpty(difference(A, B))

|#