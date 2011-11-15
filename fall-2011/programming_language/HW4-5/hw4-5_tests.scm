#lang eopl

(require "top.scm")

;;; bogus method overriding. 
(run "
class oddeven extends object 
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))

class bogus-oddeven extends oddeven
  method even(n)if zero?(n) then 0 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 1 else send self even (-(n,1))
let o1 = new bogus-oddeven() in send o1 odd(18)")

;;; simple class test. 
(run "
class c1 extends object
 method initialize () 1
 method m2() 12
let o1 = new c1()
in send o1 m2()")

;; run time modifier check. 
(run "
class c1 extends object
 method initialize () 1
 method m2() 12
let o1 = new c1()
in send o1 m2()")

;; compile time modifier check. 
;(run "
;class c1 extends object
; method initialize () 1
; private method m2() 12
;class c2 extends c1
; method initialize () 2
; public method m2() super m2() 
;let o1 = new c2()
;in send o1 m2()")

;;; test static fields. 
(run "
class c1 extends object
static s
field x 
 method initialize () 1
 method m2() 12
let o1 = new c1()
in send o1 m2()")

;;; operations on static fields.
(run "
class c1 extends object 
  field y 
  method gety()y 33 ")

;test from the homework description. 
; there are errors in the original example. 
; one: there is no add1 declared in the classes language, i changed to + operation. 
; the syntax of begin ... end has error, there should be no ";" at the end of 
; the last expression. 
(run "
class c1 extends object
  static next_serial_number
  field my_serial_number
  method get_serial_number() my_serial_number
  method initialize()
     begin
      set my_serial_number = next_serial_number;
      set next_serial_number = +(next_serial_number,1)
     end
  let o1 = new c1()
      o2 = new c1()
  in list (send o1 get_serial_number(),
           send o2 get_serial_number())")