#lang eopl
;;
;; Author : Shumin Guo (U00617724)
;; Date : 11/15/2011. 
;; Notes: Test cases for Homework 4 of programming languages. 
;;

(require "top.scm")

;;; testing named send. 
;;; should return (21 0), f11 is 0 because no object of c1 is created, so initialize is not executed.
(display "Testing for named-send .....")
(display (run "
class c1 extends object
  static field f11
  static field f12 
  method initialize() begin set f11 = 11; set f12 = 12 end
  method getf() f11
class c2 extends c1 
  protected field f21
  private field f22
  method initialize() begin set f21 = 21; set f22 = 22 end
  method getf() f21 
let o = new c2() 
in list (send o getf(), named-send c1 o getf())
"))
(newline)

;; field reference and field set. 
(display "Testing for field reference ......")
(run "
class c1 extends object
 public field f1
 method initialize () set f1 = 111
 method m2() 12
let o1 = new c1()
in field-ref (o1, f1)")

(display "\nAnother test for field reference and field set......")
(run "
class c1 extends object
 public field f1
 method initialize () set f1 = 111
 method m2() f1
let o1 = new c1()
in begin field-set (o1, f1, 111); field-ref (o1, f1); 
field-set (o1, f1, 999); field-ref (o1, f1) end ")

;;; This test case will test the modifiers for methods and fields. 
;;; method modifiers include "public final private protected"
;;; field modifiers include "static public private protected"
;;; This one should not work, because getf1() private, objects outside 
;;; of the class can not access it. 
(display "\nTesting for method modifiers (private).......\n")
#|(display (run "
class c1 extends object
  static field f11
  method initialize() set f11 = 11
  private method getf1() f11 
  method getf2() f11
let o = new c1() 
in send o getf1()
"))|#
(newline)

;;; while this one should be able to work, because getf2() is 
;;; public method and it can call private method getf1() within 
;;; the same class. 
(display "\nTesting for method modifiers (private).......\n")
(display (run "
class c1 extends object
  static field f11
  method initialize() set f11 = 11
  private method getf1() f11 
  method getf2() f11
let o = new c1() 
in send o getf2()
"))
(newline)

;;; test for protected methods.
;;; in this test case, it should that decendant methods in classes
;;; can call protected methods in super class.
(display "\nTesting for method modifiers (protected).......\n")
(display (run "
class c1 extends object
  static field f11
  static field f12 
  method initialize() begin set f11 = 11; set f12 = 12 end
  protected method getf11() f11
class c2 extends c1 
  protected field f21
  private field f22
  method getf11() super getf11() 
let o = new c2() 
in send o getf11()
"))
(newline) 

;;; while this one should not be able to work, because protected method getf11()
;;; in class c2 should only be called by methods within the same class or decendent 
;;; class methods. 
#|(display (run "
class c1 extends object
  static field f11
  static field f12 
  method initialize() begin set f11 = 11; set f12 = 12 end
  method getf11() f11
class c2 extends c1 
  protected field f21
  private field f22
  protected method getf11() f11 
let o = new c2() 
in send o getf11()
"))|#

;;; Here is the test for the final method, which prohibit the overriding
;;; of super class methods. 
;;; in this test case, method even in class oddeven is declared as final, 
;;; so, semantically it can not be overriden. And in its child class bogus-oddeven
;;; we are trying to override this method even(), it will encounter an error.
(display "Testing for final method overriding.....\n")
#|(display (run "
class oddeven extends object 
  method initialize()1
  final method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))

class bogus-oddeven extends oddeven
  method even(n)if zero?(n) then 0 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 1 else send self even (-(n,1))
let o1 = new bogus-oddeven() in 
list (send o1 odd(18), send o1 odd(13), send o1 odd(20), send o1 odd(15))"))|#

(newline)

;;; Testing for field modifiers. 
(display "Testing for private fields (should have an error) .....")
#|(display (run "
class c1 extends object
 private field f1
 method initialize () set f1 = +(1,5555)
class c2 extends c1 
 method m2() let o1 = new c2()
in field-ref (o1, f1)
let o2 = new c2()
in send o2 m2()"))|#

(newline)

;;; testing for static fields. 
(display "Testing for static fields (see homework 5.) .....")
(newline)

;;; testing for protected fields. 
(display "Testing for protected fields (should not work, because f1 of c1 is protected field......")
#|(display (run "
class c1 extends object
 protected field f1
 method initialize () set f1 = +(1,5555)
 method get() f1
class c2 extends c1 
 method m2() super get()
let o2 = new c2()
in field-ref (o2, f1)"))|#
(newline)

;;; test for protected again.
(display "Testing for protected fields (should work......")
(display (run "
class c1 extends object
 protected field f1
 method initialize () set f1 = +(1,1234)
 method get() f1
class c2 extends c1 
 method m2() super get()
let o2 = new c2()
in send o2 m2()"))
(newline)