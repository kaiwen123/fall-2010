// -*- C -*-
/**
 * @file SortedVector.h
 * @brief Definition of a SortedVector class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _SortedVectorClass_
#define _SortedVectorClass_
#include <fstream>
#include "Employee.h"
#include <vector>

using namespace std; 

class SortedVector {
 private:
  vector<Employee> _sortedArray; 	/* Sorted vector list. */

 public:
  /* a ctors */
  SortedVector();
  ~SortedVector(){_sortedArray.clear();}

  int size() const {return _sortedArray.size();}
  vector<Employee>& getArray() {return _sortedArray;}

  /**
   * @brief Insert and element into the list. 
   *        The list should be ordered after sort. 
   * @param t Element to be inserted. 
   * @return true on success and false on failure.
  */
  bool insert(Employee& e);

  /**
   * @brief find according to the last name of Employee. 
   * @param lname last name of employee
   * @return true on success and false on failure. 
   */
  bool find(string lname); 

  /**
   * @brief Overloading the << operator for output. 
   *        Note that this is not a member function but
   *        just a friend function of this class. 
   * @param out output stream. 
   * @param v and object of this class. 
   * @return ostream object. 
   */
  friend ostream& operator<<(ostream& out, SortedVector& sv);

  /**
   * @brief Save all records to file. 
   * @param fname name of file to be saved to. 
   * @return true on success and false on failure. 
   */
  bool saveToFile(string fname);

  /**
   * @brief Print all records to stdout. 
   * @param none. 
   * @return none. 
   */
  void print();
};
#endif	/* ifdef */
