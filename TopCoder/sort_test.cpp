#include "sortalg.hpp"
#include <iostream>
#include <stdlib.h>

using namespace std; 

bool printv(vector<int>::iterator itb, vector<int>::iterator ite);

int main() {
  const int INTSIZE = 10; 

  vector<int> input; 
  srand ( time(0) );
  for(int i = 0; i < INTSIZE; i++) {
    input.push_back(rand() % 1000);
  }

  // print the original int vector. 
  cout << "-----------Selection Sort------------" << endl; 
  cout << "Before sort: " << endl; 
  printv(input.begin(), input.end()); 
  select_sort(input); 
  // print the sort result. 
  cout << endl << "After sort: " << endl; 
  printv(input.begin(), input.end()); 

  // insertion sort. 
  input.clear(); 
  srand ( time(0) );
  for(int i = 0; i < INTSIZE; i++) {
    input.push_back(rand() % 1000);
  }
  cout << "\n-----------Insertion Sort------------" << endl; 
  cout << "Before sort: " << endl; 
  printv(input.begin(), input.end()); 
  insert_sort(input); 
  // print the sort result. 
  cout << endl << "After sort: " << endl; 
  printv(input.begin(), input.end()); 

  return 0; 
}

bool printv(vector<int>::iterator itb, vector<int>::iterator ite) {
  while(itb != ite) {
    cout << *itb << ","; 
    itb++; 
  }
  cout << endl; 
  return true;
}
