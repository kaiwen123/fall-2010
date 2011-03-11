/**
 * Implementation of the merge join operation. 
 * Given two integer vectors, join the ones with the same value. 
 * Two steps are used in this algorithm. 
 * 1, sort the vectors. 
 * 2, join vectors by value, and print the result. 
 */
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std; 

bool sortmergejoin(vector<int> &s, vector<int> &t);

int main() {
  int a[] = {5,3,5,5,6,8,2,2,2,9}; 
  int b[] = {1,1,2,6,3,2,10,2,5,5,8,6};
  vector<int> A;
  vector<int> B;
  for(int i = 0; i < 10; i++) A.push_back(a[i]); 
  for(int i = 0; i < 12; i++) B.push_back(b[i]);

  sortmergejoin(A, B); 
  return 0; 
}

bool sortmergejoin(vector<int> &s, vector<int> &t) {
  int n = s.size(); 
  int m = t.size(); 
  bool result = false; 
  sort(s.begin(), s.end()); 
  sort(t.begin(), t.end()); 
  int i = 0, j = 0; 
  while((i < n) && (j < m)) {
    if(s[i] < t[j]) {
      i++; 
    } else if(s[i] > t[j]) {
      j++; 
    } else {
      int k = 0; 
      while((j + k < m) && (s[i] == t[j+k])) {
	cout << s[i] << " s-t " << t[j+k] << endl; 
	k++; result = true; 
      }
      k= 1; 
      while((i+k<n) && (s[i+k]==t[j])) {
	cout << s[i+k] << " s-t " << t[j] << endl; 
	k++; result = true; 
      }
      i++; j++; 
    }
  }
  return result; 
}
