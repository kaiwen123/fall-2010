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
 
class TimeTravellingCellar {
public:
int determineProfit(vector <int> profit, vector <int> decay) {
  int proft = 0;
  int maxgain = 0, minlost = 10000, 
    semaxgain = 0, seminlost = 10000; 
  int maxgainidx = 0, minlostidx = 0,
    semaxgainidx = 0, seminlostidx = 0; 
  int len = profit.size(); 
  for(int i = 0; i < len; i++) {
    if(maxgain < profit[i]) {
      maxgain = profit[i];
      maxgainidx = i;
    } 
    if(minlost > decay[i]) {
      minlost = decay[i]; 
      minlostidx = i; 
    }
  }
 
  // gain and lost have the same index; 
  if(maxgainidx == minlostidx) {
    for(int i = 0; i < len; i++) {
      if((i != maxgainidx) && (semaxgain < profit[i])) {
	semaxgainidx = i; 
	semaxgain = profit[i];
      }
      if((i != minlostidx) && (seminlost > decay[i])) {
	seminlostidx = i; 
	seminlost = decay[i];
      }
    }
    int gain1 = semaxgain - minlost;
    int gain2 = maxgain - seminlost;
    proft = gain1 >= gain2 ? gain1 : gain2; 
  } else {
    proft = maxgain - minlost; 
  }

  return proft; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arr0[] = {1,2,3}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {3,1,2}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 2; verify_case(0, Arg2, determineProfit(Arg0, Arg1)); }
	void test_case_1() { int Arr0[] = {3,2}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {1,2}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 1; verify_case(1, Arg2, determineProfit(Arg0, Arg1)); }
	void test_case_2() { int Arr0[] = {3,3,3}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {1,1,1}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 2; verify_case(2, Arg2, determineProfit(Arg0, Arg1)); }
	void test_case_3() { int Arr0[] = {1000,500,250,125}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {64,32,16,8}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 992; verify_case(3, Arg2, determineProfit(Arg0, Arg1)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    TimeTravellingCellar ___test;
    ___test.run_test(-1);
}
// END CUT HERE
