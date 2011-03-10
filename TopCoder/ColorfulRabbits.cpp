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
 
class ColorfulRabbits {
public:
int getMinimum(vector <int> replies) {
  int totalnum = 0; 
  int len = replies.size(); 
  sort(replies.begin(), replies.end()); 
  totalnum = replies[0] + 1;
  int replycount = 1; 
  for(int i = 1; i < len; i++) {
    int cureply = replies[i]; 
    int preply = replies[i-1]; 
    cout << cureply << " - " << totalnum << endl; 
    if(cureply == preply) {
      replycount++; 
      if(replycount > cureply + 1) {
	replycount = 1;
	totalnum += cureply + 1;
      }
    } else {
      totalnum += cureply + 1; 
      replycount = 1; 
    }
  }
  return totalnum;
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arr0[] = { 1, 1, 2, 2 }
; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 5; verify_case(0, Arg1, getMinimum(Arg0)); }
	void test_case_1() { int Arr0[] = { 0 }
; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(1, Arg1, getMinimum(Arg0)); }
	void test_case_2() { int Arr0[] = {6,2,0,2,2,2,6,0,2,2,2,2,6,2,2,6,2,3,3,3,0,0,6,6,0}
; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 28; verify_case(2, Arg1, getMinimum(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    ColorfulRabbits ___test;
    ___test.run_test(-1);
}
// END CUT HERE
