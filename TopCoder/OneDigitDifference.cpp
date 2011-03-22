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
 
class OneDigitDifference {
public:
int getSmallest(int N) {
  // The smallest int will be the one to change the highest digit of N
  // to 0. 
  if(0 == N) return 1; 
  long int N1 = (long)N; 
  long int base = 10; 
  while(0 != N1 / base) {
    base *= 10;
  }
  base /= 10; 
  cout << N1 << " " << base << " "  << N1 % base << endl; 
  return N1 % base; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arg0 = 9; int Arg1 = 0; verify_case(0, Arg1, getSmallest(Arg0)); }
	void test_case_1() { int Arg0 = 0; int Arg1 = 1; verify_case(1, Arg1, getSmallest(Arg0)); }
	void test_case_2() { int Arg0 = 900000123; int Arg1 = 123; verify_case(2, Arg1, getSmallest(Arg0)); }
	void test_case_3() { int Arg0 = 30000; int Arg1 = 0; verify_case(3, Arg1, getSmallest(Arg0)); }
	void test_case_4() { int Arg0 = 47; int Arg1 = 7; verify_case(4, Arg1, getSmallest(Arg0)); }
	void test_case_5() { int Arg0 = 2000000000; int Arg1 = 0; verify_case(5, Arg1, getSmallest(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    OneDigitDifference ___test;
    ___test.run_test(-1);
}
// END CUT HERE
