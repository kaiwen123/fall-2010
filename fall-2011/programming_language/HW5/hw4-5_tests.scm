#lang eopl

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

(display "\nTesting for field reference and field set......")
(run "
class c1 extends object
 public field f1
 method initialize () set f1 = 111
 method m2() f1
let o1 = new c1()
in begin field-set (o1, f1, 111); field-ref (o1, f1); 
field-set (o1, f1, 999); field-ref (o1, f1) end ")

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
(display "\nTesting for static fields......\n")
;test from the homework description. 
; there are errors in the original example. 
; one: there is no add1 declared in the classes language, i changed to + operation. 
; the syntax of begin ... end has error, there should be no ";" at the end of 
; the last expression. 
(run "
class c1 extends object
  static field next_serial_number
  public field my_serial_number
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

;;; This test case will test the modifiers for methods and fields. 
;;; method modifiers include "public final private protected"
;;; field modifiers include "static public private protected"
(display "\nTesting for method modifiers.......\n")
(display (run "
class c1 extends object
  static field f11
  static field f12 
  method initialize() begin set f11 = 11; set f12 = 12 end
  method getf11() f11
class c2 extends c1 
  protected field f21
  private field f22
  method getf11() f11 
let o = new c2() 
in send o getf11()
"))

(display (run "
class c1 extends object
  static field f11
  static field f12 
  method initialize() begin set f11 = 11; set f12 = 12 end
  method getf11() f11
class c2 extends c1 
  protected field f21
  private field f22
  method getf11() f11 
let o = new c2() 
in send o getf11()
"))