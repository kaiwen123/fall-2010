// -*- C++ -*-
/**
 * @file LinkedSortedList.cpp 
 * @brief Implementation of a LinkedSortedList class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "SortedVector.h"
#include "Employee.h"

// Constructor; 
template <class T> 
SortedVector<T>::SortedVector(){

}

// insert element into vector. 
template <class T> 
bool SortedVector<T>::insert(T& t) {

}

// Find element(s) with last name. 
template <class T> 
bool SortedVector<T>::find(string lname) {

}

// overloading << operator for convenience of output.
template <class T>
ostream& operator<<(ostream& out, SortedVector<T>& v) {

}

// For template class to export symbols, I initialized the template
// class with Employee. 
template class SortedVector<Employee>; 
