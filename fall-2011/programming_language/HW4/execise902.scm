#lang eopl 

(require "top.scm")

;;; bogus method overriding. 
;;; should return value (1 0 1 0)
(display "Testing for bogus method overriding.....\n")
(display (run "
class oddeven extends object 
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))

class bogus-oddeven extends oddeven
  method even(n)if zero?(n) then 0 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 1 else send self even (-(n,1))
let o1 = new bogus-oddeven() in 
list (send o1 odd(18), send o1 odd(13), send o1 odd(20), send o1 odd(15))"))

(newline)