#include <iostream> 
using namespace std; 
//1. what is the output of the following program?
class A{
public:
    struct HEADER{
        int a_;
        double b_;
        unsigned int c_;
        unsigned char d_;
        static const int SIZE = 100;
    }header;
private:
    double k;
};

int main()
{
    A a;
    cout << "sizeof A: " << sizeof(a) << endl;
    cout << "sizeof structure: " << sizeof(a.header) << endl;
}
