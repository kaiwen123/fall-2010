// -*- C++ -*-
/**
 * @file SortedVector.cpp 
 * @brief Implementation of a SortedVector class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "SortedVector.h"
#include "Employee.h"

// Constructor; 
SortedVector::SortedVector(){}

// insert element into vector. 
bool SortedVector::insert(Employee& e) {
  //if(!size()){_sortedArray.push_back(t); return true;}
  vector<Employee>::iterator _it = _sortedArray.begin();
  while(_it != _sortedArray.end()) {
    if(*_it >= e) {
      //void insert(iterator loc, size_type num, const TYPE& val);
      _sortedArray.insert(_it, 1, e);
      return true;
    }
    _it++;
  }
  _sortedArray.push_back(e); 
  return true;
}

// Find element(s) with last name. 
bool SortedVector::find(string lname) {
  int total = 0, cmp;
  int start = 0, end = size() - 1; // location of binary search. 
  int mid = (start + end) / 2;   // middle point.
  // upper first letter. 
  if((lname.at(0)<='z') && (lname.at(0)>='a')) 
    lname.at(0) = lname.at(0) - ('a' - 'A');
  while(1) {
    cmp = lname.compare(_sortedArray.at(mid).getLastName());
    if(cmp > 0) { // result in second half.
      start = mid; 
      mid = (start + end) / 2;
    } else if(cmp < 0) { // result in first half. 
      end = mid; 
      mid = (start + end) / 2;
    } else { // result is around here!!.
      cout << _sortedArray.at(mid) << endl; total++; // current record. 
      for(int i = 1; i <= mid - start; i++) { // search up.
	cmp = lname.compare(_sortedArray.at(mid-i).getLastName());
	if(cmp == 0){
	  cout << _sortedArray.at(mid-i) << endl; total++;
	} else break; 
      }
      for(int i = 1; i <= end - mid; i++) { // search down. 
	cmp = lname.compare(_sortedArray.at(mid+i).getLastName());
	if(cmp == 0){
	  cout << _sortedArray.at(mid+i) << endl; total++;
	} else break;
      }
      break; 			// stop searching. 
    }
  }
  cout << size() << " Employee(s) searched. " 
       << "Found: " << total << " record(s)." 
       << endl << endl;
  return total == 0;
}

// overloading << operator for convenience of output.
ostream& operator<<(ostream& out, SortedVector& v) {
  out << "<Records>" << endl;
  vector<Employee>::iterator _it = v.getArray().begin();
  while(_it != v.getArray().end()) {
    out << *_it << "--" << endl;
    _it++;
  }
  out << "<END>";
  return out; 
}

// print to stdout. 
void SortedVector::print() {
  cout << *this; 
}

// Save records to file. 
bool SortedVector::saveToFile(string fname) {
  ofstream saveFile(fname.c_str());
  saveFile << *this; 
  saveFile.close();
  return true; 
}
