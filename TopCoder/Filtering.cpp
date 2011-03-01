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
 
class Filtering {
public:
vector <int> designFilter(vector <int> sizes, string outcome) {
  vector<int> result; 		// result to be returned. 
  map<int, char> sets; 		// 
  int len = sizes.size(); 
  for(int i = 0; i < len; i++) {
    sets.insert(pair<int, char>(sizes[i], outcome[i]));
  }

  // now, test if there exist a range.
  int step = 0; 
  int start, end; 
  map<int, char>::iterator iter = sets.begin(); 
  while(iter != sets.end()) {
    if('A' == iter->second) {
      switch(step) {
      case 0: step = 1; start = iter->first; break; 
      case 1: end = iter->first; break; 
      case 2: result.clear(); return result; 
      default: result.clear(); return result; 
      }
    } else if ('R' == iter->second) {
      if(1 == step) step = 2;
    }
    iter++;
  }
  result.push_back(start); 
  result.push_back(end);
  return result; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const vector <int> &Expected, const vector <int> &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: " << print_array(Expected) << endl; cerr << "\tReceived: " << print_array(Received) << endl; } }
	void test_case_0() { int Arr0[] = {3, 4, 5, 6, 7}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arg1 = "AAAAA"; int Arr2[] = {3, 7 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(0, Arg2, designFilter(Arg0, Arg1)); }
	void test_case_1() { int Arr0[] = {3, 4, 5, 6, 7}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arg1 = "AARAA"; int Arr2[] = { }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(1, Arg2, designFilter(Arg0, Arg1)); }
	void test_case_2() { int Arr0[] = {3, 4, 5, 6, 7}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arg1 = "RAAAA"; int Arr2[] = {4, 7 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(2, Arg2, designFilter(Arg0, Arg1)); }
	void test_case_3() { int Arr0[] = {68,57,7,41,76,53,43,77,84,52,34,48,27,75,36}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arg1 = "RARRRARRRARARRR"; int Arr2[] = {48, 57 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(3, Arg2, designFilter(Arg0, Arg1)); }
	void test_case_4() { int Arr0[] = {26,81,9,14,43,77,55,57,12,34,29,79,40,25,50}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); string Arg1 = "ARAAARRARARARAA"; int Arr2[] = { }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(4, Arg2, designFilter(Arg0, Arg1)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    Filtering ___test;
    ___test.run_test(-1);
}
// END CUT HERE
