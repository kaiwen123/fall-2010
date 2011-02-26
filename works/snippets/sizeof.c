#include <iostream> 
void fun(int b[10]) {
  std::cout << sizeof b << std::endl;
}

int main() {
  int i; 
  int *p = NULL; 
  int a[100];
  std::cout << sizeof i << std::endl; 
  std::cout << sizeof(*p) << " " << sizeof p << std::endl; 
  std::cout << sizeof(a) << " " << sizeof a 
    << " " << sizeof a[100] << " " << sizeof(&a) 
    << " " << sizeof(&a[10]) << std::endl; 
  fun(a);
  return 0; 
}
