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
 
class Lottery {
public:
vector <string> sortByOdds(vector <string> rules) {
    
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const vector <string> &Expected, const vector <string> &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: " << print_array(Expected) << endl; cerr << "\tReceived: " << print_array(Received) << endl; } }
	void test_case_0() { string Arr0[] = {"PICK ANY TWO: 10 2 F F"
,"PICK TWO IN ORDER: 10 2 T F"
,"PICK TWO DIFFERENT: 10 2 F T"
,"PICK TWO LIMITED: 10 2 T T"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arr1[] = { "PICK TWO LIMITED",  "PICK TWO IN ORDER",  "PICK TWO DIFFERENT",  "PICK ANY TWO" }; vector <string> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); verify_case(0, Arg1, sortByOdds(Arg0)); }
	void test_case_1() { string Arr0[] = {"INDIGO: 93 8 T F",
 "ORANGE: 29 8 F T",
 "VIOLET: 76 6 F F",
 "BLUE: 100 8 T T",
 "RED: 99 8 T T",
 "GREEN: 78 6 F T",
 "YELLOW: 75 6 F F"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arr1[] = { "RED",  "ORANGE",  "YELLOW",  "GREEN",  "BLUE",  "INDIGO",  "VIOLET" }; vector <string> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); verify_case(1, Arg1, sortByOdds(Arg0)); }
	void test_case_2() { string Arr0[] = {}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arr1[] = { }; vector <string> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); verify_case(2, Arg1, sortByOdds(Arg0)); }

// END CUT HERE

}; 
 
// BEGIN CUT HERE
int main() {
    Lottery ___test;
    ___test.run_test(-1);
}
// END CUT HERE
