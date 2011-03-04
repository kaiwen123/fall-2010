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
 
class Painting {
public:
int largestBrush(vector <string> picture) {
  int size = 50; 		// size to return; 
  int rows = picture.size(), cols = picture[0].size(); 
  for(int i = 0; i < rows; i++) {
    int sizerow = 0;
    for(int j = 0; j < cols; j++) {
      // test smallest row width. 
      if('B' == picture[i][j]) {
	if(0 == j) {
	  sizerow = 1; 	  
	} else {
	  if('B' == picture[i][j-1]) {
	    sizerow++; 
	  } else {
	    if((sizerow < size) && (sizerow > 0)) size = sizerow; 
	    sizerow = 1; 
	  }
	}
      } else {
	if((sizerow < size) && (sizerow > 0)) size = sizerow; 
      }
    }
    //cout << sizerow << endl; 
    if((sizerow < size) && (sizerow > 0)) size = sizerow; 
  }

  // test col width. 
  for(int i = 0; i < cols; i++) {
    int sizecol = 0; 
    for(int j = 0; j < rows; j++) { 
      //cout << picture[j][i] << " " ;     
      if('B' == picture[j][i]) {
	if(0 == j) {
	  sizecol = 1; 	  
	} else {
	  if('B' == picture[j-1][i]) {
	    sizecol++; 
	  } else {
	    if((sizecol < size) && (sizecol > 0)) size = sizecol; 
	    sizecol = 1; 
	  }
	}
      } else {
	if((sizecol < size) && (sizecol > 0)) size = sizecol; 
      }
    }
    //cout << sizecol << endl; 
    if((sizecol < size) && (sizecol > 0)) size = sizecol; 
  }
 
  return size; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arr0[] = {"BBBB",
 "BBBB",
 "BBBB",
 "BBBB"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 4; verify_case(0, Arg1, largestBrush(Arg0)); }
	void test_case_1() { string Arr0[] = {"BBBB",
 "BWWB",
 "BWWB",
 "BBBB"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(1, Arg1, largestBrush(Arg0)); }
	void test_case_2() { string Arr0[] = {"WBBBBB",
 "BBBBBB",
 "BBBBBB",
 "BBBBBB"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 3; verify_case(2, Arg1, largestBrush(Arg0)); }
	void test_case_3() { string Arr0[] = {"BBBB",
 "BBBB",
 "WBBB",
 "BBBB",
 "BBBB",
 "BBBB"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 2; verify_case(3, Arg1, largestBrush(Arg0)); }
	void test_case_4() { string Arr0[] = {"WBBBBBWWWWWWWWW",
 "WBBBBBBWWWWWWWW",
 "WBBBBBBBBBBBWWW",
 "WBBBBBBBBBBBWWW",
 "BBBBBBBBBBBBBBB",
 "BBBBBBBBBBBBBBB",
 "BBBBBBBBBBBBBBB",
 "BBBBBBBBWWBBBBB",
 "BBBBBBBBWBBBBBB",
 "WBBBBBBBWBBBBBW",
 "BBBBBBBWWBBBBBW",
 "BBBBBBBWWBBBBBW",
 "BBBBBBWWWBBBBBW",
 "BBBBBWWWWWWWWWW",
 "BBBBBWWWWWWWWWW"}
; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 5; verify_case(4, Arg1, largestBrush(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    Painting ___test;
    ___test.run_test(-1);
}
// END CUT HERE
