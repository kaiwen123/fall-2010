#lang eopl

;; This is the program of 2010 fall programming language final exam. 
;; 1. 
;; 1.a. currying function to return product of two numbers. 
(define (prod n1)
  (lambda (n2) 
    (* n1 n2)))

;; 1.b. 
#|
let makemult = proc (maker)
  proc (x)
    if zero?(x)
      then 0
      else -(((maker maker) -(x,1)), -4)
in let times4 = proc (x) ((makemult makemult) x)
  in (times4 3)
|#

;; The evaluation of this let & proc program can be explained as follows. 
#|
Actually, this is a recursive function. And it will calculate the value in the following step. 

let's abbreviate makemult as mul. 

((mul mul) 3)
= -(((mul mul) 2), -4)

= -(-(((mul mul) 1), -4), -4)

= -(-(-(((mul mul) 0), -4), -4), -4)

= -(-(-(0, -4), -4), -4)

= 12

let makemult = proc (maker)
  proc (x y)
    if zero?(x)
      then 1
      else -(((maker maker) -(x,1)), -(y,0))
in let timesy = proc (x y) ((makemult makemult) x y)
in (timesy 10 4)

;; let program example. 
let p = [i=1, v=5, x=10].
(value-of <<-(-(x,3), -(v,i))>> p)
= (num-val (- (value-of <<-(x,3)>> p)
(value-of <<-(v,i)>> p)

= (num-val (- (expval->num (num-val (-10 3))
(expval->num (num-val (- 5 1)))))

= (num-val (- 7 4)) = (num-val 3)

(value-of <<if zero?(- (x,11)) then -(y,2) else -(y,4)>> p)
= (if (value-of <<zero?(-(x,11))>> p)
(value-of <<-(y,2)>> p)
(value-of <<-(y,4)>> p))

= (if (value-of <<zero?(22)>> p) (value-of <<-(y,2)>> p) (value-of <<-(y,4)>> p))

= (if #f (value-of <<-(y,2)>> p) (value-of <<-(y,4)>> p))

= (value-of <<-(y,4)>> p)

= (num-val (- (expval->val y) (expval->val 4)) p)

= (num-val (- 22 4))

= (num-val 18)


(value-of <<let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8),y)>> p)

= (value-of <<let y = let x = -(x,1) in -(x,y) in -(-(x,8),y)>> [y = 2, x = 7]p0)

= (value-of <<-(-(x,8),y)>> [y = (value-of <<let x = -(x,1) in -(x,y)>> p1)]p1)

= (value-of <<-(-(x,8),y)>> [y = (value-of <<-(x,y)>> [x=6]p1)] p1)

= (value-of <<-(-(x,8),y)>> [y=4]p1)

= (num-val (- (- 7 8) 4))

= (num-val -5)

(value-of <<let x = 200 in let f = proc (z) -(z,x) in let x=100 in let g=proc (z) -(z,x) in -((f 1), (g 1))>> p)

= (value-of <<let f = proc (z) -(z,x) in let x=100 in let g=proc (z) -(z,x) in -((f 1), (g 1))>> [x=200]p)

= (value-of <<let g=proc (z) -(z,x) in -((f 1), (g 1))>> [x=100][f=(value-of <<f = proc (z) -(z,200)>> p) [x=200]p)

let p1 = [x=100][f=(value-of <<f = proc (z) -(z,200)>> p) [x=200]p

= (value-of <<-((f 1), (g 1))>> [g=(value-of <<proc (z) -(z,x)>> p1)]p1)

= (value-of <<-((f 1), (g 1))>> [g=(value-of <<proc (z) -(z,100)>> p1)] p1)

p2 = [g=(value-of <<proc (z) -(z,100)>> p1)]p1
= (num-val (- (value-of <<(f 1)>> p2) (value-of <<(g 1)>> p2)))

= (num-val (- (value-of <<-(z,200)>> [z = 1]p2) (value-of <<-(z,100)>> [z = 1]p2)

= (num-val (- -199 -99))

= (num-val -100)



|#