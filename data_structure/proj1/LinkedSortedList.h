// -*- C++ -*-
/**
 * @file LinkedSortedList.h
 * @brief Definition of a LinkedSortedList class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _LinkedSortedListClass_
#define _LinkedSortedListClass_
#include "LinkedNode.h"
#include "SortedList.h"

using namespace std; 

template <typename T>
class LinkedSortedList : public SortedList<T> {
 private:
  int _size; 
  LinkedNode<T>* _list_head;
  LinkedNode<T>* _it; 		/* The List iterator */

 public:
  /* a ctors */
  LinkedSortedList<T>();
  ~LinkedSortedList<T>();

  // Virtual functions from SortedList class
  // Virtual functions inherited from the parent class should be implemented 
  // in the Child class. 
  /**
   * @brief Delete each node in the sorted list; 
   * @param none; 
   * @return none; 
  */
  virtual void clear(); 

  /**
   * @brief Insert a value of type T into the sorted list; 
   * @param newvalue The value to be inserted with type T; 
   * @return true on success and false on failure; 
   */
  virtual bool insert(T newvalue); 

  /**
   * @brief Get the first value and then delete this node; 
   * @param returnvalue A variable to store the value in the first node. 
   * @return true on success and false on failure; 
  */
  virtual bool getfirst(T& returnvalue); 

  /**
   * @brief print the value of each node in the list. 
  */
  virtual void print() const;

  /**
   * @brief find a value from the list.
   * @param searchvalue The value to be searched. 
   * @return true on success and false on failure; 
  */
  virtual bool find(T searchvalue) const;

  /**
   * @brief return the size of the list;
   * @return size of the list. 
  */
  virtual int size() const; 

  // Function specific to this class. 
  // Operations to pointer it.
  /**
   * @brief rewind the pointer to the start of the list.
  */
  void reset(); 

  /**
   * @brief Insert node from the head of list. 
   * @param[node] The node to be inserted. 
   * @return true on success and false on failure; 
  */
  bool insertHead(LinkedNode<T>* node); 

  /**
   * @brief Insert node from the tail of list. 
   * @param[node] The node to be inserted. 
   * @return true on success and false on failure; 
  */
  bool insertTail(LinkedNode<T>* node); 

  /**
   * @brief test if the list is empty. 
   * @return true if empty and false if not; 
  */
  bool isEmpty() const; 
};
#endif	/* ifdef */
