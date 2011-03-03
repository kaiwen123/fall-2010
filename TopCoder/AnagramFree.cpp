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
 
class AnagramFree {
public:
int getMaximumSubset(vector <string> S) {
  int len = S.size(); 
  for(int i = 0; i < len; i++) {
    sort(S[i].begin(), S[i].end());
  }

  int maxanagram = len; 
  for(int i = 0; i < len - 1; i++) {
    for(int j = i+1; j < len; j++) {
      if(S[i] == S[j]) {
	maxanagram--; 
	break;
      }
    }
  }
  return maxanagram; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arr0[] = {"abcd","abdc","dabc","bacd"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(0, Arg1, getMaximumSubset(Arg0)); }
	void test_case_1() { string Arr0[] = {"abcd","abac","aabc","bacd"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 2; verify_case(1, Arg1, getMaximumSubset(Arg0)); }
	void test_case_2() { string Arr0[] = {"aa","aaaaa","aaa","a","bbaaaa","aaababaa"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 6; verify_case(2, Arg1, getMaximumSubset(Arg0)); }
	void test_case_3() { string Arr0[] = {"creation","sentence","reaction","sneak","star","rats","snake"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 4; verify_case(3, Arg1, getMaximumSubset(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    AnagramFree ___test;
    ___test.run_test(-1);
}
// END CUT HERE
