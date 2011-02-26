#include <iostream>
using namespace std; 

int main() {
  int a, b; 
  int *ptr;
  int **ptrPtr; 
  ptr = &a; 
  ptrPtr = &ptr; 
  
  cin >> a >> b; 
  cout << *a << endl; 
  cout << *&ptr << "------" << ptr << endl; 
  cout << &*ptr << "------" << ptr << endl; 
  cout << "Ptr is: " << ptr << " + " << *ptr << endl; 
  *ptrPtr = &b;
  cout << "ptrPtr is: " << ptrPtr << " + " << **ptrPtr << endl; 

  return 0; 
}
