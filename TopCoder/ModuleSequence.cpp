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
 
class ModuleSequence {
public:
long long countElements(long long K, long long N, long long A, long long B, long long lower, long long upper) {
    
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const long long &Expected, const long long &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { long long Arg0 = 2LL; long long Arg1 = 7LL; long long Arg2 = 1LL; long long Arg3 = 5LL; long long Arg4 = 2LL; long long Arg5 = 5LL; long long Arg6 = 3LL; verify_case(0, Arg6, countElements(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5)); }
	void test_case_1() { long long Arg0 = 9LL; long long Arg1 = 1LL; long long Arg2 = 0LL; long long Arg3 = 7LL; long long Arg4 = 0LL; long long Arg5 = 0LL; long long Arg6 = 8LL; verify_case(1, Arg6, countElements(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5)); }
	void test_case_2() { long long Arg0 = 20LL; long long Arg1 = 12LL; long long Arg2 = 21LL; long long Arg3 = 30LL; long long Arg4 = 1LL; long long Arg5 = 11LL; long long Arg6 = 6LL; verify_case(2, Arg6, countElements(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5)); }
	void test_case_3() { long long Arg0 = 30LL; long long Arg1 = 89LL; long long Arg2 = 112LL; long long Arg3 = 200LL; long long Arg4 = 80LL; long long Arg5 = 88LL; long long Arg6 = 9LL; verify_case(3, Arg6, countElements(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5)); }
	void test_case_4() { long long Arg0 = 890LL; long long Arg1 = 1000LL; long long Arg2 = 1000LL; long long Arg3 = 10000LL; long long Arg4 = 456LL; long long Arg5 = 980LL; long long Arg6 = 4770LL; verify_case(4, Arg6, countElements(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    ModuleSequence ___test;
    ___test.run_test(-1);
}
// END CUT HERE
