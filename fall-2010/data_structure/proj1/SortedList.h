// Sorted List Abstract Class
// M. Raymer, 12/31/04
// CS 400/600 -- Data Structures and Program Design
// ----------------------------------------------------------------
// This file defines the Sorted List Abstract Class.
// The class includes pure virtual functions, so it cannot be
// instantiated.  Instead, you must derive a class that includes
// the implementation for each of the functions below.
//
// The list defined here can only hold integer values.
//
// Collaborators:  None.
// References:     None.
// ----------------------------------------------------------------

#ifndef _SortedListClass_
#define _SortedListClass_

template <typename T> 
class SortedList {
public:

  // -------------------------------------------------------------------
  // Pure virtual functions -- you must implement each of the following
  // functions in your implementation:
  // -------------------------------------------------------------------

  // Clear the list.  Free any dynamic storage.
  virtual void clear() = 0;          
                                     
  // Insert an integer into the list.  Return true if successful, false
  // if failure.
  virtual bool insert(T newvalue) = 0;
                
  // Get AND DELETE the first element of the list, placing it into the
  // return variable "value".  If the list is empty, return false, otherwise
  // return true.
  virtual bool getfirst(T& returnvalue) = 0;

  // Print out the entire list to cout.  Print an appropriate message
  // if the list is empty.  Note:  the "const" keyword indicates that
  // this function cannot change the contents of the list.
  virtual void print() const = 0;

  // Check to see if "value" is in the list.  If it is found in the list,
  // return true, otherwise return false.  Like print(), this function is
  // declared with the "const" keyword, and so cannot change the contents
  // of the list.
  virtual bool find(T searchvalue) const = 0;

  // Return the number of items in the list
  virtual int size() const = 0;
};

#endif
