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
 
class NinePuzzle {
public:
int getMinimumCost(string init, string goal) {
  map<char, int> initcount; 
  map<char, int> goalcount; 
  if(init.size() != goal.size()) {
    return -1; 
  }
  initcount.insert(pair<char, int>('R', 0));
  initcount.insert(pair<char, int>('G', 0));
  initcount.insert(pair<char, int>('Y', 0));
  initcount.insert(pair<char, int>('B', 0));
  initcount.insert(pair<char, int>('*', 0));

  goalcount.insert(pair<char, int>('R', 0));
  goalcount.insert(pair<char, int>('G', 0));
  goalcount.insert(pair<char, int>('Y', 0));
  goalcount.insert(pair<char, int>('B', 0));
  goalcount.insert(pair<char, int>('*', 0));

  int len = init.size(); 
  for(int i = 0; i < len; i++) {
    initcount[init[i]] = initcount[init[i]]+1;
    goalcount[goal[i]] = goalcount[goal[i]]+1; 
  }

  // now test which one to repaint. 
  int a = goalcount['R'] - initcount['R']; 
  int b = goalcount['B'] - initcount['B']; 
  int c = goalcount['Y'] - initcount['Y']; 
  int d = goalcount['G'] - initcount['G']; 
  //if( a == b == c == d == 0) return 0; 
  int total = 0; 
  if(a > 0) total += a; 
  if(b > 0) total += b; 
  if(c > 0) total += c; 
  if(d > 0) total += d; 
  return total; 
}
 
// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arg0 = "BG*YRGRRYR" ; string Arg1 = "BGGY*YRRRR" ; int Arg2 = 0; verify_case(0, Arg2, getMinimumCost(Arg0, Arg1)); }
	void test_case_1() { string Arg0 = "GBBB*BGBBG" ; string Arg1 = "RYYY*YRYYR"; int Arg2 = 9; verify_case(1, Arg2, getMinimumCost(Arg0, Arg1)); }
	void test_case_2() { string Arg0 = "RRBR*BRBBB" ; string Arg1 = "BBRB*RBRRR" ; int Arg2 = 1; verify_case(2, Arg2, getMinimumCost(Arg0, Arg1)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    NinePuzzle ___test;
    ___test.run_test(-1);
}
// END CUT HERE
