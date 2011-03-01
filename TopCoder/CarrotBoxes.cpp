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
 
class CarrotBoxes {
public:
double theProbability(vector <string> information) {
    
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const double &Expected, const double &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arr0[] = {"YYYYY",
 "NYNNN",
 "NNYNN",
 "NNNYN",
 "NNNNY"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); double Arg1 = 0.8; verify_case(0, Arg1, theProbability(Arg0)); }
	void test_case_1() { string Arr0[] = {"YNNNN",
 "NYNNN",
 "NNYNN",
 "NNNYN",
 "NNNNY"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); double Arg1 = 0.2; verify_case(1, Arg1, theProbability(Arg0)); }
	void test_case_2() { string Arr0[] = {"Y"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); double Arg1 = 1.0; verify_case(2, Arg1, theProbability(Arg0)); }
	void test_case_3() { string Arr0[] = {"YNNNN",
 "YYNNN",
 "YNYNN",
 "NNNYY",
 "NNNYY"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); double Arg1 = 0.6; verify_case(3, Arg1, theProbability(Arg0)); }
	void test_case_4() { string Arr0[] = {"YYYNNNYN",
 "NYNNNNYN",
 "NNYNNNNN",
 "NYNYNNNN",
 "YNNNYNNY",
 "NNYNNYNN",
 "NNNNYNYN",
 "NNYNNNNY"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); double Arg1 = 0.875; verify_case(4, Arg1, theProbability(Arg0)); }
	void test_case_5() { string Arr0[] = {"YNNNNNNNNYNNNNNNNNNN",
 "NYNNNNNNNNNNNNNNNNNN",
 "NNYNNNNNNNYNNNNNYNNN",
 "NNNYNYNNNNNNNNYNNNNN",
 "NNNNYNNNNNNNNNYNNNNY",
 "NNNNNYNNNNNNNNNNNNNY",
 "NNNNYNYNYNNNNNNNNNNN",
 "NNNNNNNYNNNYYNNNNNNN",
 "NNNNNNNNYNNNNNNNNNNN",
 "YNNNNNNNNYNNNNNYNNNN",
 "NNNNNNNNNNYNNNNNNNNN",
 "NYNNNNNNNNNYNNNNNNNN",
 "NNNNNNNYNNNNYNNNNNNN",
 "NNNNNNNNNNNNNYNNNYNN",
 "NNNNNNNNNNNYNNYNNNYN",
 "NYNNNNNNNNNNNNNYNNNN",
 "NNYNNNNNNNNNNNNNYNNN",
 "NNNNNNNNNNNNNYNYNYNN",
 "NNNNNNNNYNYNNNNNNNYY",
 "NNNYNNNNNNNNNNNNNNNY"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); double Arg1 = 0.75; verify_case(5, Arg1, theProbability(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    CarrotBoxes ___test;
    ___test.run_test(-1);
}
// END CUT HERE
