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
 
class PalindromeGame {
public:
int getMaximum(vector <string> front, vector <int> back) {
  int len = front.size(); 
  int total = 0; 

  // clean the vectors. 
  for(int i = 0; i < len - 1; i++) {
    int fnt = i, bak = -1; 
    for(int j = i + 1; j < len; j++) {
      if((front[i] == front[j]) && (back[j] < back[fnt])) {
	fnt = j; 
      }
      if(isPanlin(front[i], front[j]) && (back[j] > back[bak])) {
	bak = j; 
      }
    }

    // found panlindrome.
    if(bak != -1) {
      total += back[fnt] + back[bak]; 
      front.erase(front.begin() + fnt); 
      back.erase(back.begin() + fnt); 
      front.erase(front.begin() + bak); 
      back.erase(back.begin() + bak); 
      len = front.size();
    }
  }
  return total; 
}

  bool isPanlin(string &a, string &b) {
    string::iterator ita = a.begin(); 
    string::reverse_iterator itb = b.rbegin(); 
    while(a != a.end()) {
      if((*ita) != (*itb)) return false; 
    }
    ita++; itb++; 
    return true; 
  } 

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arr0[] = { "topcoder", "redcoder", "redocpot" }; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = { 7, 5, 3 }; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 10; verify_case(0, Arg2, getMaximum(Arg0, Arg1)); }
	void test_case_1() { string Arr0[] = { "rabbit" }; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = { 1000000 }; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 0; verify_case(1, Arg2, getMaximum(Arg0, Arg1)); }
	void test_case_2() { string Arr0[] = { "abc", "abc", "def", "cba", "fed" }; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = { 24, 7, 63, 222, 190 }; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 499; verify_case(2, Arg2, getMaximum(Arg0, Arg1)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    PalindromeGame ___test;
    ___test.run_test(-1);
}
// END CUT HERE
