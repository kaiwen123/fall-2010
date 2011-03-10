#include <functional>
#include <algorithm>
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <fstream>
#include <numeric>
#include <iomanip>
#include <cstdlib>
#include <cstring>
#include <utility>
#include <cctype>
#include <vector>
#include <string>
#include <bitset>
#include <cmath>
#include <queue>
#include <stack>
#include <ctime>
#include <list>
#include <map>
#include <set>
 
using namespace std;
 
#define PB push_back
#define MP make_pair
 
typedef vector<int> VI;
typedef vector<string> VS;
typedef vector<double> VD;
typedef pair<int,int> PII;
typedef long long LL;
typedef unsigned long long ULL;
 
class MinDifference {
public:
int closestElements(int A0, int X, int Y, int M, int n) {
  int min = 10000; 
  int diff;
  vector<int> listA; 
  listA.push_back(A0); 
  for(int i = 1; i < n; i++) {
    int tmp = (listA[i-1] * X + Y) % M;
    listA.push_back(tmp);
  }
  sort(listA.begin(), listA.end()); 
  int len = listA.size(); 
  for(int i = 1; i < len; i++) {
    diff = abs(listA[i] - listA[i - 1]);
    if(diff < min) min = diff; 
  }
  return min;
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arg0 = 3; int Arg1 = 7; int Arg2 = 1; int Arg3 = 101; int Arg4 = 5; int Arg5 = 6; verify_case(0, Arg5, closestElements(Arg0, Arg1, Arg2, Arg3, Arg4)); }
	void test_case_1() { int Arg0 = 3; int Arg1 = 9; int Arg2 = 8; int Arg3 = 32; int Arg4 = 8; int Arg5 = 0; verify_case(1, Arg5, closestElements(Arg0, Arg1, Arg2, Arg3, Arg4)); }
	void test_case_2() { int Arg0 = 67; int Arg1 = 13; int Arg2 = 17; int Arg3 = 4003; int Arg4 = 23; int Arg5 = 14; verify_case(2, Arg5, closestElements(Arg0, Arg1, Arg2, Arg3, Arg4)); }
	void test_case_3() { int Arg0 = 1; int Arg1 = 1221; int Arg2 = 3553; int Arg3 = 9889; int Arg4 = 11; int Arg5 = 275; verify_case(3, Arg5, closestElements(Arg0, Arg1, Arg2, Arg3, Arg4)); }
	void test_case_4() { int Arg0 = 1; int Arg1 = 1; int Arg2 = 1; int Arg3 = 2; int Arg4 = 10000; int Arg5 = 0; verify_case(4, Arg5, closestElements(Arg0, Arg1, Arg2, Arg3, Arg4)); }
	void test_case_5() { int Arg0 = 1567; int Arg1 = 5003; int Arg2 = 9661; int Arg3 = 8929; int Arg4 = 43; int Arg5 = 14; verify_case(5, Arg5, closestElements(Arg0, Arg1, Arg2, Arg3, Arg4)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    MinDifference ___test;
    ___test.run_test(-1);
}
// END CUT HERE
