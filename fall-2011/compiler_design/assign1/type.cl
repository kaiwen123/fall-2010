(*
 *  CS780 Fall 2011
 *
 *  Programming Assignment 1 ::  Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
-- This is the first homework, the goal is to use stack data structure to 
-- test the arithmatic calculation results. 
-- Note that some of the code in this file was borrowed from the list.cl 
-- example script, so thanks to the original author of the examples. 
-- @author Shumin Guo && Hailong Shi. 
-- @revision 1.1 09/21/2011 
-- @contact guo.18@wright.edu

-- This is the empty stack class. 
-- It is the building base class for the stack. 
-- This class provides procedures to test the state of the Stack object. 
class Stack {
   -- Define operations on empty lists.

   isNil() : Bool { true };

   -- first element in the object. 
   -- if the stack is empty, the program will abort, as you are trying to access
   -- an element that does not exist.
   head()  : String { { abort(); " Stack Empty! "; } };

   -- stack of elements other than
   tail()  : Stack { { abort(); self; } };

   cons(str : String) : Stack {
      (new Cons).init(str, self)
   };

};


-- child class of the stack class, I am trying to use the recursive method to 
-- build the stack, a push corresponds to re-assigning the head element and 
-- the tail element of the stack. 
class Cons inherits Stack {

   car : String;		-- The element in this list cell

   cdr : Stack;			-- The rest of the stack.

   isNil() : Bool { false };

   head()  : String { car };

   tail()  : Stack { cdr };

   init(str : String, rest : Stack) : Stack {
      {
	 car <- str;
	 cdr <- rest;
	 self;
      }
   };
};


-- main class of the assignment. 
-- this class is reponsible for interaction with users, it accepts inputs, 
-- and update the stack state, user can check the status the stack using the 
-- "p" command. 
class Main inherits IO {

   stack : Stack;

   -- Print all elements of the stack. Calls itself recursively with
   -- the tail of the stack, until the end of the stack is reached.

   print(l : Stack) : Object {
      if l.isNil() then out_string("\n")
                   else {
			   out_string(l.head());
			   out_string(" ");
			   print(l.tail());
		        }
      fi
   };

   main() : Object {
	let done : Bool <- true,
	    opcode : String, 
	    prompt : String <- ">>",
	    intstr : String <- "int",
	    realstr : String <- "real",
	    stack : Stack <- new Stack in 
	{ while done loop 
      	  {	    
	    out_string(prompt); 
      	    opcode <- in_string(); 

	    if opcode = "p" then print(stack) else "" fi;
	    if opcode = "i" then stack <- stack.cons(intstr) else "" fi;
	    if opcode = "j" then stack <- stack.cons(intstr) else "" fi;
	    if opcode = "a" then stack <- stack.cons(realstr) else "" fi;
	    if opcode = "b" then stack <- stack.cons(realstr) else "" fi;
	    if opcode = "*" then
	       {
	         let operand1 : String, 
		     operand2 : String in 
		     {
			operand1 <- stack.head(); 
			stack <- stack.tail(); 
			operand2 <- stack.head(); 
			stack <- stack.tail(); 
			if operand1 = intstr then 
			   {
				stack <- stack.cons(operand2);
			   }
			else stack <- stack.cons(realstr) fi; 
		     };
	       }
	    else 0 fi;

	    if opcode = "+" then
	       {
	         let operand1 : String, 
		     operand2 : String in 
		     {
			operand1 <- stack.head(); 
			stack <- stack.tail(); 
			operand2 <- stack.head();
			stack <- stack.tail(); 
			if operand1 = intstr then 
			   {
				stack <- stack.cons(operand2);
			   }
			else stack <- stack.cons(realstr) fi; 
		     };
	       }
	    else 0 fi;

	    if opcode = "!" then { 
	       out_string(stack.head());
	       out_string("\n");
	    } else 0 fi;

	    if opcode = "q" then done <- false  else 0 fi;
	}
	pool; 
      }
  };
};
