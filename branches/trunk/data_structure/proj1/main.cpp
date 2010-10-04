// $Id$
/**
 * @file main.cpp 
 * @brief This is a test program for this project. 
 * @see README. 
 * @warning
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "LinkedSortedList.h"
#include <iostream>

using namespace std;

// ---------------------------------------------------------------------
// main() -- Load some values into the list and print it.  Then clear
//           the list and reload it.  Finally, print the values in the
//           list using getfirst, which should remove all the values
//           from the list.
// ---------------------------------------------------------------------
int main() {

  // Create int and double lists; 
  LinkedSortedList<int> intlist;
  LinkedSortedList<double> doublelist; 
  // Inserting integer values; 
  int aInt[10] = {3, -2, 5, 8, 1, 3, 9, 10, -3, 16};
  int bInt[10] = {30, -12, 500, 18, 1000, -12, 900, 120, -53, 11};
  double aDouble[10] = {-0.209878, 0.122, 9.2, 3.4, 5.5, 2.3,\
			-1.0, 23.0, 12.8, 2.1}; 
  double bDouble[10] = {0.2, 10.22, 90.12, 13.41, -5.59999, \
			52.3, -1.01, -23.0, 120.8, 202.1}; 

  // Testing int list; 
  cout << "Testing Integer list ... " << endl; 

  /*Insert and print.*/
  for(int i = 0; i < 10; i++)
    intlist.insert(aInt[i]); 
  intlist.print();
  cout << endl; 

  /*Test the find() method. */
  for(int i = 0; i < 10; i++)
    cout << intlist.find(aInt[i]) << ", ";
  cout << endl; 
  /*Test the getfirst() method.*/   
  int intvalue; 
  while(intlist.getfirst(intvalue))
    cout << intvalue << ", ";
  cout << endl; 
  
  /* Clear the list and test again. */
  intlist.clear(); 
  for(int i = 0; i < 10; i++)
    intlist.insert(bInt[i]); 
  intlist.print(); 
  cout << endl; 

  // =========================================
  cout << endl << "Testing double list ... " << endl; 
  //Testing double list.
  /* Testing the insert method. */
  for(int i = 0; i < 10; i++)
    doublelist.insert(aDouble[i]); 
  doublelist.print();
  cout << endl; 

  /* Testing the find() method.*/ 
  for(int i = 0; i < 10; i++)
    cout << doublelist.find(aDouble[i]) << ", ";
  cout << endl; 

  /*Testing the getfirst() method. */
  double doublevalue; 
  while(doublelist.getfirst(doublevalue))
    cout << doublevalue << ", ";
  cout << endl; 
  
  /*Clear and test the double list again; */
  doublelist.clear(); 
  for(int i = 0; i < 10; i++)
    doublelist.insert(bDouble[i]); 
  doublelist.print(); 
  cout << endl; 
 
  return 0;
}
