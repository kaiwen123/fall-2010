// -*- C++ -*-
/**
 * @file LinkedSortedList.cpp 
 * @brief Implementation of a LinkedSortedList class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "LinkedSortedList.h"

// Constructor; 
template <typename T> 
LinkedSortedList<T>::LinkedSortedList() {
  _size = 0;
  _list_head = NULL; 
  _it = NULL;
}

// Destructor;
template <typename T> 
LinkedSortedList<T>::~LinkedSortedList<T>() {
  clear(); 
}

// Clear the list; 
template <typename T> 
void LinkedSortedList<T>::clear() {
  for(_it = _list_head; _it != NULL;) {
    LinkedNode<T> * tmp = _it; 
    _it = _it->next;
    delete tmp; 
  }
  _list_head = NULL; 
  _it = _list_head; 
  _size = 0; 
}

// Insert a value into the list; 
template <typename T> 
bool LinkedSortedList<T>::insert(T newvalue) {
  LinkedNode<T> * tmp = new LinkedNode<T>((T)newvalue); 
  if(!tmp) return false;
  reset();
  // Inserting from head; 
  if((isEmpty()) || (_it->value >= newvalue)) {
    insertHead(tmp);
    _size++;
    return true; 
  } 
  while(_it->next != NULL) {
    if(_it->next->value >= newvalue) {
      tmp->next = _it->next; 
      _it->next = tmp;
      _size++; 
      return true;  
    } // if
    _it = _it->next;
  }
  insertTail(tmp);
  _size++; 
  return true; 
} //insert

// Get first value and then delete this node; 
template <typename T> 
bool LinkedSortedList<T>::getfirst(T& returnvalue) {
  if(!isEmpty()) {
    returnvalue = _list_head->value;
    LinkedNode<T>* tmp = _list_head;
    _list_head = _list_head->next;
    _it = _list_head; 
    _size--; 
    delete tmp; 
    return true; 
  } else {
    returnvalue = (T)0;
    return false; 
  }
}

// Print out the value of each node; 
template <typename T> 
void LinkedSortedList<T>::print() const {
  LinkedNode<T>* _it = _list_head;
  while(_it) {
    _it->print(); 
    _it = _it->next; 
  }
}

// Search a value in the list
template <typename T> 
bool LinkedSortedList<T>::find(T searchvalue) const {
  LinkedNode<T>* _it = _list_head;
  while(!isEmpty()) {
    if(_it->value == searchvalue)
      return true; 
    _it = _it->next; 
  } //while
  return false;
}

// Return the size of the list; 
template <typename T> 
int LinkedSortedList<T>::size() const {
  return _size; 
}
// Reset the _it pointer to the start of list. 
template <typename T> 
void LinkedSortedList<T>::reset() {
  _it = _list_head; 
}
// Insert node on the head;
template <typename T> 
bool LinkedSortedList<T>::insertHead(LinkedNode<T>* node) {
  node->next = _list_head; 
  _list_head = node; 
  return true; 
}
// Insert node from the tail; 
template <typename T> 
bool LinkedSortedList<T>::insertTail(LinkedNode<T>* node) {
  _it->next = node;
  return true; 
}

// Test if the list is empty? 
template <typename T> 
bool LinkedSortedList<T>::isEmpty() const {
  return (_size == 0); 
}

// For template class to export symbols, I initialized the template
// class with int and double datatypes. 
template class LinkedSortedList<int>;
template class LinkedSortedList<double>; 
