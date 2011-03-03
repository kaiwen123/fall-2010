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
 
class ColoredStrokes {
public:
int getLeast(vector <string> picture) {
  int H = picture.size(); 
  int W = picture[0].size(); 
  int repaint = 0;

  // test number of horizental lines. 
  for(int i = 0; i < H; i++) {	// rows. 
    for(int j = 0; j < W; j++) { // cols. 
      if(('R' == picture[i][j]) || ('G' == picture[i][j])){ // horizontal lines. 
	if(0 == j) {
	  repaint++; 
	} else if(('R' != picture[i][j-1]) && ('G' != picture[i][j-1])) {
	  repaint++; 
	}
      } 

      if(('B' == picture[i][j]) || ('G' == picture[i][j])){ // vertical lines. 
	if(0 == i) {
	  repaint++; 
	} else if(('B' != picture[i-1][j]) && ('G' != picture[i-1][j])) {
	  repaint++; 
	}
      } 
    }
  }

  return repaint; 
}

     // BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arr0[] = {"...",
 "..."}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 0; verify_case(0, Arg1, getLeast(Arg0)); }
	void test_case_1() { string Arr0[] = {"..B.",
 "..B."}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(1, Arg1, getLeast(Arg0)); }
	void test_case_2() { string Arr0[] = {".BB."}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 2; verify_case(2, Arg1, getLeast(Arg0)); }
	void test_case_3() { string Arr0[] = {"...B..",
 ".BRGRR",
 ".B.B.."}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 3; verify_case(3, Arg1, getLeast(Arg0)); }
	void test_case_4() { string Arr0[] = {"...B..",
 ".BRBRR",
 ".B.B.."}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 4; verify_case(4, Arg1, getLeast(Arg0)); }
	void test_case_5() { string Arr0[] = {"GR",
 "BG"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 4; verify_case(5, Arg1, getLeast(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    ColoredStrokes ___test;
    ___test.run_test(-1);
}
// END CUT HERE
