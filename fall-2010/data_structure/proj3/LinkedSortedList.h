// -*- C -*-
/**
 * @file LinkedSortedList.h
 * @brief Definition of a LinkedSortedList class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _LinkedSortedListClass_
#define _LinkedSortedListClass_
#include <fstream>
#include "LinkedNode.h"
#include "SortedList.h"
#include "Employee.h"

using namespace std; 

template <class T> class LinkedSortedList;
template <class T>
ostream& operator<<(ostream& out, LinkedSortedList<T>& list);

template <class T>
class LinkedSortedList : public SortedList<T> {
 private:
  int size_; 
  LinkedNode<T>* list_head;
  LinkedNode<T>* iter; 		/* The List iterator */
  
 public:
  /* a ctors */
  LinkedSortedList<T>();
  virtual ~LinkedSortedList<T>();

  LinkedNode<T>* getListHead() {return list_head;}
  void setListHead(LinkedNode<T>* n) {list_head = n;}
  /**
   * @brief Delete each node in the sorted list.
   * @param none. 
   * @return none. 
  */
  virtual void clear(); 

  /**
   * @brief Delete each node in the sorted list.
   * @param node Node to be deleted. 
   * @return true on success and false on failure.
  */
  bool deleteNode(LinkedNode<T> *node); 

  /**
   * @brief Insert a value of type T into the sorted list; 
   * @param newvalue The value to be inserted with type T.
   * @return true on success and false on failure. 
   */
  virtual bool insert(T& newvalue); 
  bool insert(LinkedNode<T> *node);

  /**
   * @brief Get the first value and then delete this node; 
   * @param returnvalue A variable to store the value in the first node. 
   * @return true on success and false on failure. 
  */
  virtual bool getfirst(T& returnvalue); 

  /**
   * @brief print the value of each node in the list. 
  */
  virtual void print();

  /**
   * @brief find a value from the list.
   * @param lname Last name to be searched. 
   * @return true on success and false on failure. 
  */
  virtual bool find(string lname) const;

  /**
   * @brief return the size of the list.
   * @return size of the list. 
  */
  virtual int size() const; 

  /**
   * @brief test if the list is empty. 
   * @return true if empty and false if not.
   */
  bool isEmpty() const; 

  /**
   * @brief Make the linked list empty. 
   * @return true if empty and false if not.
   */
  bool emptyList(); 

  /**
   * @brief Save All the employee data into a named file. 
   * @param The name of the file to save to. 
   * @return true on success and false on failure. 
   */
  bool saveToFile(string fname);

  /**
   * @brief Overloading the operator << for output. 
   * Here the <<<> is a little tricky, please refer to related
   * materials about the standard of defining template friend
   * functions. 
   * @param out output stream. 
   * @param list LinkedSortedList object. 
   * @return output stream object. 
   */
  friend ostream& operator<<<>(ostream& out, LinkedSortedList<T>& list);
};

/**
 * @brief An alternative method for overloading the operator << for
 * output. Move the declaration of friend functions outside of the
 * class definition. 
 * @param out output stream. 
 * @param list LinkedSortedList object. 
 * @return output stream object. 
 */
// template <class T>
// ostream& operator<<(ostream& out, LinkedSortedList<T>& list);
// Then in the cpp source file. Implement this function as:  
// template <class T> 
// ostream& operator<<(ostream& out, LinkedSortedList<T>& list) {
// Implementation detail comes here. 
// }

#endif	/* ifdef */
