#lang eopl


(display (run "
let maketimes = proc(p)
proc (x)
    proc (y)
       if zero? (y)
       then 0 
       else -((((p p) -(y,1)) x),-(0,x))
in let times = proc(x) proc(y) (((maketimes maketimes) x) y)
in let makefact = proc(q) proc (x) if zero? (x) then 1 else ((times x) ((q q)  -(x ,1)))
in let fact = proc (x) ((makefact makefact) x)
in (fact 1)
"))