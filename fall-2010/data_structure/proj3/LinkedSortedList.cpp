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
LinkedSortedList<T>::LinkedSortedList():size_(0),list_head(NULL),iter(NULL) {
}

// Destructor;
template <class T> 
LinkedSortedList<T>::~LinkedSortedList<T>() {
  clear(); 
}

// Clear the list; 
template <class T> 
void LinkedSortedList<T>::clear() {
  for(iter = getListHead(); iter != NULL; iter = iter->next) {
    LinkedNode<T> * tmp = iter; 
    if(tmp) delete tmp; 
  }
  setListHead(NULL);
  iter = getListHead();
  size_ = 0; 
}

// Make the list empty. 
template <class T> 
bool LinkedSortedList<T>::emptyList() {
  clear();
  return true; 
}

// Insert a value into the list; 
template <class T> 
bool LinkedSortedList<T>::insert(T& newvalue) {
  LinkedNode<T> *node = new (nothrow) LinkedNode<T>(newvalue); 
  if(!node) {
    cerr << "Error Creating new node. " << endl;
    return false;
  }
  insert(node);
} //insert

// Insert a node into the list. 
template <class T> 
bool LinkedSortedList<T>::insert(LinkedNode<T> *node) {
  iter = getListHead();
  if(!iter) {			// First node becomes head. 
    setListHead(node); 

#ifdef DEBUG_LIST_INSERT
    cout << "Insert First node. " 
	 << node->getValue().getLastName() << endl; 
#endif

    size_++; return true;
  }
  for(iter = getListHead(); iter; iter = iter->next) {
    if(*node <= *iter) {	// node is smaller, insert in front. 

#ifdef DEBUG_LIST_INSERT
      cout << "Insert node: " << node->getValue().getLastName() 
	   << " \tbefore: " << iter->getValue().getLastName() << endl; 
#endif
      node->next = iter; 
      node->prev = iter->prev;
      if(iter == getListHead()) { // insert to Head.
	setListHead(node);

#ifdef DEBUG_LIST_INSERT
	cout << "Set head to: " << node->getValue().getLastName() <<
	endl;
#endif

      }	else {			// not head insert.
	iter->prev->next = node; // Head does not have this.
      }
      iter->prev = node; 
      size_++; return true; 

    } else if((*node > *iter) && (!(iter->next))) {//tail

#ifdef DEBUG_LIST_INSERT
      cout << "Insert tail: " << node->getValue().getLastName()
	   << " \tafter: " << iter->getValue().getLastName() << endl; 
#endif

      iter->next = node;
      node->prev = iter;
      node->next = NULL;
      size_++; return true; 
    }
  } // while
}

// delete node from list. 
template <class T> 
bool LinkedSortedList<T>::deleteNode(LinkedNode<T> *node) {
  if(!node) return false;	// node is NULL;
  // First, we need to release the node before deleting.
  if(getListHead() == node) {	// Head node.
    setListHead(node->next);
  } else if(node->next == NULL) {
    node->prev->next = NULL;
  } else {
    node->prev->next = node->next;
    node->next->prev = node->prev;
  }
  // Now, delete node.
#ifdef DEBUG_LIST_DELETE 
  cout << "Deleted record for : " 
       << node->getValue().getLastName() 
       << "\t" << node->getValue().getEid() << endl;
#endif
  delete node; 
  size_--;
  return true;
}

// Get first value and then delete this node; 
template <class T> 
bool LinkedSortedList<T>::getfirst(T& returnvalue) {
  if(!isEmpty()) {
    returnvalue = list_head->value;
    LinkedNode<T>* tmp = list_head;
    list_head = list_head->next;
    list_head->next = NULL;
    list_head->prev = NULL;
    iter = list_head; 
    size_--; 
    delete tmp; return true; 
  } else { return false;}
}

// Overloading the operator << 
template <class T> 
ostream& operator<<(ostream& out, LinkedSortedList<T>& list) {
  LinkedNode<T>* iter = list.getListHead();
  out << "<Records>" << endl;
  while(iter) {
    out << *iter; 
    out << "--" << endl;
    iter = iter->next; 
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
  if(!saveFile){
    cerr<<"Openning save file error..."<<endl;
    return false;
  }
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
  LinkedNode<T>* iter = list_head;
  int r_count = 0;	// search count and result count.
  while((!isEmpty()) && (iter)) {
    if(lname.compare(iter->getValue().getLastName())==0) {
      // print out the found item. 
      r_count++; 
      cout << iter->getValue() << endl; 
    }
    iter = iter->next; 
  } //while
  cout << size() << " Employee(s) searched. " 
       << "Found: " << r_count << " record(s)." 
       << endl << endl; 
  return true;
}

// Return the size of the list; 
template <class T> 
int LinkedSortedList<T>::size() const {
  return size_; 
}

// Test if the list is empty? 
template <class T> 
bool LinkedSortedList<T>::isEmpty() const {
  return (size_ == 0); 
}

// For template class to export symbols, I initialized the template
// class with Employee. 
template class LinkedSortedList<Employee>;
