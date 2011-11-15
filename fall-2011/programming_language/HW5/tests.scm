#lang eopl
;;
;; Author : Shumin Guo (U00617724)
;; Date : 11/15/2011. 
;; Notes: Homework 5 of programming languages. 
;;
(require "top.scm")

(display "This is the test file for homework 5, some cases will combine work from homework 4. \n\n")
;; field reference and field set. 
(display "Testing for default value of static field (should return 0) ......")
(display (run "
class c1 extends object
 static field f1
 public field f2
 method initialize () set f2 = 2
 method m2() 12
let o1 = new c1()
in field-ref (o1, f1)"))
(newline)

;;; should return (1000), f11 is 0 because no object of c1 is created, so initialize is not executed.
(display "\nTesting static field using field set/ref .....")
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
  method getf() f11 
let o = new c2() 
in begin 
field-set (o,f11,1000); 
list (field-ref(o,f11), send o getf())
end
"))
(newline)

(display "\nTesting for static field addition (without explicite initialization)......")
(display (run "
class c1 extends object
 static field f1
 method initialize () set f1 = +(f1,5555)
 method m2() f1
let o1 = new c1()
in send o1 m2()"))
(newline)

; test from the homework description. 
; there are errors in the original example. 
; one: there is no add1 declared in the classes language, i changed to + operation. 
; the syntax of begin ... end has error, there should be no ";" at the end of 
; the last expression. 
(display "\nTesting for static fields with case from problem description (slightly modified according to my grammar) ......")
(display (run "
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
           send o2 get_serial_number())"))
(newline)

;;; This test case will test the modifiers for methods and fields. 
;;; method modifiers include "public final private protected"
;;; field modifiers include "static public private protected"
(display "\nTesting with static fields expanding over class heirarchy.......")
(display (run "
class c1 extends object
  static field f11
  static field f12 
  protected field f13
  method initialize() set f13 = 13
  method add1() begin set f11 = +(f11,1); set f12 = +(f12, 1) end
  method sumstatic() begin set f11 = +(f11,f12); set f12 = +(f12,f11); +(f11, f12) end
class c2 extends c1 
  private field f21
  method initialize() begin super add1(); set f21 = super sumstatic()  end
  method getf21() f21
let o = new c2() 
in send o getf21()
"))
