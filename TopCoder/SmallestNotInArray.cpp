// Problem description: 
// Given a list of integers, find the smallest positive integer. 
// The complexity of the algorithm is O(n).

#include <iostream>
#include <vector>

using namespace std; 

int main() {
  int a[] = {100, -1, 1, 2, 10};
  int size = 5; 
  int tmp; 
  for(int i = 0; i < size; i++) {
    if((a[i] >= 0) && (a[i] < size) && (a[i] != i)) {
      tmp = a[i]; 
      a[i] = a[tmp]; 
      a[tmp] = tmp;
      i--;			// don't move forward.!!
    }
  }

  int result = -1; 
  for(int i = 0; i < size; i++) {
    cout << i << " " << a[i] << endl; 
    if((i>0) && (a[i] != i)){
      result = i; break; 
    }
  }
  if(result == -1) result = size; 
  cout << result << endl; 
  return 0;
}
