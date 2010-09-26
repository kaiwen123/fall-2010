// -*- C++ -*-
/**
 * @file LinkedSortedList.cpp 
 * @brief Implementation of a LinkedSortedList class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "LinkedSortedList.h"
#include "Employee.h"

// Constructor; 
template <class T> 
LinkedSortedList<T>::LinkedSortedList() {
  _size = 0;
  _list_head = NULL; 
  _it = NULL;
}

// Destructor;
template <class T> 
LinkedSortedList<T>::~LinkedSortedList<T>() {
  clear(); 
}

// Clear the list; 
template <class T> 
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
template <class T> 
bool LinkedSortedList<T>::insert(T& newvalue) {
  LinkedNode<T> * tmp = new LinkedNode<T>(newvalue); 
  if(!tmp) {cerr << "Error Creating new node. " << endl; return false;}
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
  cout << "Inserted new node : " << endl; 
  cout << tmp << endl; 
  return true; 
} //insert

// Get first value and then delete this node; 
template <class T> 
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
    //returnvalue = (T);
    return false; 
  }
}

// Print out the value of each node; 
template <class T> 
void LinkedSortedList<T>::print() const {
  LinkedNode<T>* _it = _list_head;
  while(_it) {
    _it->print(); 
    _it = _it->next; 
  }
}

// save data to file.
template <class T>
void LinkedSortedList<T>::saveToFile(string fname) {
  ofstream saveFile(fname.c_str());
  LinkedNode<T>* _it = _list_head;
  saveFile << "<Records>" << endl;
  while(_it) {
    saveFile >> _it->value; 
    saveFile << "--" << endl;
    _it = _it->next; 
  }
  saveFile << "<END>";
  saveFile.close(); 
}

// Search a value in the list
template <class T> 
bool LinkedSortedList<T>::find(string searchvalue) const {
  cout << "Searching ...." << endl; 
  LinkedNode<T>* _it = _list_head;
  int s_count = 1, r_count = 0;	// search count and result count.
  while((!isEmpty()) && (_it)) {
    if(searchvalue.compare(_it->value.getLastName())==0) {
      // print out the found item. 
      r_count++; 
      cout << _it->value << endl; 
    }
    s_count++; 
    _it = _it->next; 
  } //while
  cout << s_count << " Employee(s) searched. " 
       << "Found: " << r_count << " record(s)." 
       << endl << endl; 
  return true;
}

// Return the size of the list; 
template <class T> 
int LinkedSortedList<T>::size() const {
  return _size; 
}
// Reset the _it pointer to the start of list. 
template <class T> 
void LinkedSortedList<T>::reset() {
  _it = _list_head; 
}
// Insert node on the head;
template <class T> 
bool LinkedSortedList<T>::insertHead(LinkedNode<T>* node) {
  node->next = _list_head; 
  _list_head = node; 
  return true; 
}
// Insert node from the tail; 
template <class T> 
bool LinkedSortedList<T>::insertTail(LinkedNode<T>* node) {
  _it->next = node;
  return true; 
}

// Test if the list is empty? 
template <class T> 
bool LinkedSortedList<T>::isEmpty() const {
  return (_size == 0); 
}

// For template class to export symbols, I initialized the template
// class with int and double datatypes. 
// template class LinkedSortedList<int>;
// template class LinkedSortedList<double>; 
template class LinkedSortedList<Employee>; 
