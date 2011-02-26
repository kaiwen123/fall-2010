#include <string> 
#include <iostream>
using namespace std; 

class A {
private : 
  string name;
public:
  A(string a):name(a){} 
  A(){}
  friend ostream& operator<<(ostream& out, const A& a) {
    out << a.name; 
    return out; 
  }
  string hello() {
    return "hello ... "; 
  }
}; 

struct S1{
    short a;
};

struct S2{
    int a;
    double b;
    short c;
};

struct S3{
    char a;
    short b;
    char c;
    int d;
};

struct S4{
    char a;
    short s1;
    char *pchar;
    double d;
    long l;
    float f;
};
union U{
    char *p;
    short s;
    long l;
};

int main(void)
{
  double x; 
  A a("string"); 
  A* b = 0;
  cout << b->hello() << endl; 

  string input("reverse_this_string");
  string::iterator it_first = input.begin();
  string::iterator it_last = input.end()-1;
  
  while(it_first < it_last)
    swap(*it_first++, *it_last--);  
  cout << input << endl; 
  cout << a << endl;
  cout << sizeof(S1) << endl; 
  cout << sizeof(S2) << endl; 
  cout << sizeof(S3) << endl; 
  cout << sizeof(S4) << endl; 
  cout << sizeof(U) << endl; 
  cout << sizeof(x) << endl; 
  return 0; 
}
