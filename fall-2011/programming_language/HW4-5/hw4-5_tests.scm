#lang racket

;;; bogus method overriding. 
(value-of-program (scan&parse "
class oddeven extends object 
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))

class bogus-oddeven extends oddeven
  method even(n)if zero?(n) then 0 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 1 else send self even (-(n,1))
let o1 = new bogus-oddeven() in send o1 odd(18)"))

;;; simple class test. 
(value-of-program (scan&parse "
class c1 extends object
 method initialize () 1
 method m2() 12
let o1 = new c1()
in send o1 m2()"))

;; run time modifier check. 
(value-of-program (scan&parse "
class c1 extends object
 method initialize () 1
 private method m2() 12
let o1 = new c1()
in send o1 m2()"))

;; compile time modifier check. 
(value-of-program (scan&parse "
class c1 extends object
 method initialize () 1
 private method m2() 12
class c2 extends c1
 method initialize () 2
 public method m2() super m2() 
let o1 = new c2()
in send o1 m2()"))