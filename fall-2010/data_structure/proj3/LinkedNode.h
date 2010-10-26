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

template <class T> 
class LinkedNode {
public:

  T value;			// The data value
  LinkedNode *next;		// Pointer to the next node
  LinkedNode *prev;		// Pointer to the previous node
  
  // Simple inline constructor:  initialize values
  // In order to avoid calling the default constructor,
  // move the assignment to the initilization list. 
 LinkedNode(const T& newval):value(newval),next(NULL),prev(NULL){}
  ~LinkedNode(){}

  T& getValue() {return value;}
  
  // Inline print function:  print the node's value
  void print() {cout << value << endl;}

  /**
   * @brief Overloading operators.
   */
  bool operator>(LinkedNode<T> &node) {
    return value > node.getValue();
  }

  bool operator<=(LinkedNode<T> &node) {
    return value <= node.getValue();
  }

  bool operator==(LinkedNode<T> &node) {
    return value == node.getValue(); 
  }
  
  bool operator<(LinkedNode<T> &node) {
    return value < node.getValue();
  }
};

// overloading the << operator for LinkedNode class. 
template <class T>
ostream& operator<<(ostream& out, LinkedNode<T>& node) {
  out << node.value; 
  return out;
}

#endif
