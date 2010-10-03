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
    _size++; return true; 
  } 
  while(_it->next != NULL) {
    if(_it->next->value >= newvalue) {
      tmp->next = _it->next; 
      _it->next = tmp;
      _size++; return true;  
    } // if
    _it = _it->next;
  }
  insertTail(tmp);
  _size++; return true; 
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
    delete tmp; return true; 
  } else { return false;}
}

// Overloading the operator << 
template <class T> 
ostream& operator<<(ostream& out, LinkedSortedList<T>& list) {
  LinkedNode<T>* _it = list.getListHead();
  out << "<Records>" << endl;
  while(_it) {
    out << *_it; 
    out << "--" << endl;
    _it = _it->next; 
  }
  out << "<END>";
  return out; 
}

// Print out the value of each node; 
template <class T> 
void LinkedSortedList<T>::print(){
  cout << *this;
}

// save data to file.
template <class T>
bool LinkedSortedList<T>::saveToFile(string fname) {
  ofstream saveFile(fname.c_str());
  if(!saveFile){cerr<<""<<endl;return false;}
  saveFile << *this; 
  saveFile.close(); 
  return true;
}

// Search a value in the list
template <class T> 
bool LinkedSortedList<T>::find(string lname) const {
  cout << "Searching ...." << endl; 
  if((lname.at(0)<='z') && (lname.at(0)>='a')) 
    lname.at(0) = lname.at(0) - ('a' - 'A');
  LinkedNode<T>* _it = _list_head;
  int r_count = 0;	// search count and result count.
  while((!isEmpty()) && (_it)) {
    if(lname.compare(_it->value.getLastName())==0) {
      // print out the found item. 
      r_count++; 
      cout << _it->value << endl; 
    }
    _it = _it->next; 
  } //while
  cout << size() << " Employee(s) searched. " 
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
// class with Employee. 
template class LinkedSortedList<Employee>; 
