#lang eopl
;;
;; Author : Shumin Guo (U00617724)
;; Date : 11/08/2011. 
;; Notes: Homework 3 of programming languages. 
;;
(require "lang.scm")

;;; this module is used to test the AE0 language. 
(display "Testing run ..... \n")
(display "i = 2, j = 4, k = 8\n")
(run "i")
(run "(k + k ) * i")
(run "(i * k) + (j + i)")
(run "i + j * (((k) + j) * i)")
(display "Done!\n")

;;; test for compile 
(display "Testing Compile ...... \n")
(compile "i")
(compile "i+j")
(compile "i*j")
(compile "i+j*k")
(compile "(k+k)*i")
(compile "(i*k)+(j+i)")
(compile "i+j*(((k)+j)*i)")
(compile "i+j+k")
(display "Done!\n")

;;; test for interpret.
(display "Testing interpret ...... \n")
(interpret (compile "(((i+j)))"))
(interpret (compile "i+j"))
(interpret (compile "i+j*(((k)+j)*i)"))
(interpret (compile "((i*i)+j*(k)+j)"))
(display "Done!\n")