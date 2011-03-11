#include "sortalg.hpp"
#include <iostream>

using namespace std; 

bool insert_sort(vector<int> &input) {
  vector<int> sorted; 
  int smallest = 0; 
  int len = input.size(); 
  int unsorted = len; 

  for(int i = 0; i < len; i++) {
    smallest = 0; 
    for(int j = 0; j < unsorted; j++) {
      if(input[smallest] > input[j]) smallest = j; 
    }
    unsorted--;
    int smallnum = input[smallest];

    // insert the smallest one in the end. 
    for(int k = smallest; k < len; k++) input[k] = input[k+1];
    input[len-1] = smallnum; 
  }
  return true; 
}
