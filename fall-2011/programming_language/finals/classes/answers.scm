#lang eopl

(require srfi/41) 

;; Author: Shumin Guo (U00617724)
;; Date 11/17/2011. 
;; Note: Final exam for programming languages.

;; Problem 1. 
#|
wp( {while i > 1 do i := i / 2 od}, i==1 ) 
P0 = (i <= 1) and (i == 1) = (i == 1)
P1 = (i > 1) and wp({i := i / 2}, i == 1)
   = (i > 1) and (i / 2 == 1) 
   = (i > 1) and (i == 2)
   = (i == 2)
P2 = (i > 1) and wp({i := i / 2}, i == 2)
   = (i > 1) and (i / 2 == 2)
   = (i == 4)
Pk = 2^k
|#

;; Problem 2. 

#|
Problem 2.i. 
The rule states how the implicite refs works for the assign 
expression. In this rule, environment p is not used to store the 
actual value of the variable, rather, it stores the location
of the variable in the store. So, this rule sets the store value
of var to val1. And in our scheme code, it is corresponding
to the following code: 

              (setref! (apply-env p var) 27)

where setref! is used to change the store value, and apply-env
is used to get value of var in the given env p, which is the 
store location. 

Problem 2.ii.
(value-of exp p S0) = (val1, S1)
(value-of (let-exp (var1 var2 ...) (val1 val2 ...) body) p S0)
= (value-of body [p(var1) = val1, p(var2) = val2, ...]S1)
In the let expression, it will first create new stores for each
local variable var1 with value val1, and then store the 
(var, store-location) pairs into the environment p. Next, the body 
will be evaluated with the newly extended env with local variabls. 
And in scheme it is corresponding to the following code. 

(value-of body 
  (extend-env vars 
    (map newref (values-of-exps exps env)) env))
|#

#|
;;; Problem 3 --> please see this problem answer in the seperate file.
|#

#|
;;;Problem 4.i. 
Notational comments: In order to differentiate the same non-terminal 
on the right hand side and the left hand side, I used some simple notations
like in rule "B -> 0Br", Br is used to denote the right hand side B. 
The following rules are numbered 1, 2, 3 and 4, respectively. 

B -> 0    | B.nozs := 0; B.length := 1;
B -> 1    | B.nozs := 0; B.length := 1;
B -> 0Br  | B.nozs := Br.nozs; B.length := 1 + Br.length; 
B -> 1Br  | B.nozs := if Br.length = 1 then 
                        if Br = 0 then 1 else 0
                      else 
                        if Br.lmb = 0 then 1 + Brr.nozs (lmb means left most bit and Brr means (cdr Br)); 

;;; Problem 4.ii.
1) nozs(0101) = 1
(0101).nozs = (101).nozs (rule 3) = 1 + (1).nozs (rule 4) = 1 + 0 (rule 2) = 1; 

(10101100).nozs = 1 + (101100).nozs (rule 4) = 2 + (1100).nozs (rule 4) = 2 + (100).nozs (rule 3)
                = 3 + (0).nozs (rule 4) = 3 + 0 (rule 1) = 3; 

(001000010).nozs = (01000010).nozs (rule 3) = (1000010).nozs (rule 3) = 1 + (00010).nozs (rule 4) 
                = 1 + (10).nozs (apply rule 4 three times) = 1 + 1 (rule 4) = 2; 

(01010101).nozs = (1010101).nozs (rule 3) = 1 + (10101).nozs (rule 4) = 2 + (101).nozs (rule 4)
                = 3 + (1).nozs (rule 4) = 3 + 0 (rule 2) = 3; 

(10001000).nozs = 1 + (001000).nozs (rule 4) = 1 + (1000).nozs (apply rule 3 twice) = 2 + (00).nozs (rule 4)
                = 2 + (0).nozs (rule 3) = 2 + 0 (rule 1) = 2;

|#

;;; problem 5.
;;; 
;;; mul : m x n -> stream
;;; function mul will generate an infinite stream starting from n. 
;;; e.g.: (mul 11 0) will yield '(0 11 22 33 44 55 66 77 88 99 ... )
(define (mul m n)
  (stream-cons (* m n)
               (mul m (+ n 1))))

;;; multiples : m -> stream
;;; This is a wrapper function of the above one, which sets parameter n 
;;; of mul into 0.
;;; e.g.: (multiples 11) will yield '(0 11 22 33 44 55 66 77 88 99).
(define (multiples m)
  (mul m 0))

;;; a function to get the first n elements from the infinite stream. 
;;; first-n : n x s -> list
;;; get the first n elements of the stream as a list.
(define first-n
  (lambda (n s)
    (let ((res '()))
      (if (= 1 n)
          (list (stream-car s))
          (append res (cons (stream-car s) (first-n (- n 1) (stream-cdr s))))))))

;;; test. 
(display "Test of the multiples problem .... ")
(display (first-n 10 (multiples 21)))