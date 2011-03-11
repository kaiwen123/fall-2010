// Step one: select the smallest one from unsorted list. 
// Step two: swap the ith value with the smallest value.
#include "sortalg.hpp"

using namespace std; 

bool select_sort(vector<int> &input) {
  int smallest = 0; 		// index of the smallest item. 
  int len = input.size(); 
  for(int i = 0; i < len-1; i++) {
    for(int j = i+1; j < len; j++) {
      if(input[j] < input[smallest]) smallest = j; 
    }
    // swap the ith value with the smallest one. 
    if(i != smallest) {
      int tmp; 
      tmp = input[i]; 
      input[i] = input[smallest]; 
      input[smallest] = tmp; 
    }
  }
  return true; 
}
