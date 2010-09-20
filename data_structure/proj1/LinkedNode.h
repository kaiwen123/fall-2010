// LinkedNode.h
// M. Raymer, 12/31/04
// CS 400/600
// Collaborators: None
// References: None
// Author: Shumin Guo (U00617724)
// Email : guo.18@wright.edu
//
// This is the definition for a linked list node that holds a single
// integer value.  This is not a very object-oriented implementation,
// since the two data values are public (they should be private!) but
// this implementation makes the rest of the code simpler and easier
// to read, so we will do it this way for now.

#ifndef _LinkedNodeClass_
#define _LinkedNodeClass_

#include <iostream>
using namespace std;

template <typename T> 
class LinkedNode {
public:

  T value;			// The data value
  LinkedNode *next;		// Pointer to the next node

  // Simple inline constructor:  initialize values
  LinkedNode(T newval = (T)0, LinkedNode* newptr = NULL){
    value = newval; 
    next = newptr;
}

  // Inline print function:  print the node's value
  void print() {cout << value << ", ";}

};

#endif
