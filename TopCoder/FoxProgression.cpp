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
 
class FoxProgression {
public:
int theCount(vector <int> seq) {
  int com_add = 0; 
  double com_mul = 1.0; 
  int len = seq.size();
  bool arith = true, geom = true; 
  int result = 0;
  int append1 = 0, append2 = 0; 
  if((1 == len) || (0 == len)) return -1; 

  com_add = seq[1] - seq[0];
  if(seq[0] != 0) {
    com_mul = (double)seq[1] / (double)seq[0];
  } else {
    com_mul = 1.0;
  }
  if(2 == len) {
    result++; append1 = 2 * seq[1] - seq[0];
    double diff = com_mul - (double)(int)com_mul;
    if((diff > -1e-10) && (diff < 1e-10)) {
      append2 = (int)(seq[1] * com_mul);
      if(append1 != append2) result++; 
    }
    return result; 
  }
  for(int i = 2; i < len; i++) {
    int commonadd = seq[i] - seq[i-1];
    if(commonadd != com_add) arith = false;

    double commonmul = 1.0; 
    if(seq[i-1] != 0) {
      commonmul = (double)seq[i] / (double)seq[i-1];
    } else {
      commonmul = 1.0;
    }
    // test if the commonmul is integer. 
    double diff = commonmul - (double)(int)commonmul;
    
    if((diff > -1e-10) && (diff < 1e-10)) {
      // test if com_mul == commonmul; 
      double diff = commonmul - com_mul;
      if(!((diff > -1e-10) && (diff < 1e-10))) geom = false;  
    } else {
      geom = false;
    }
  }

  // test if two appended integers are the same. 
  // arithmetic sequence. 
  if(arith) { 
    result++;
    append1 = seq[len-1] + com_add;
  }

  // geometric sequence.  
  if(geom) {
    append2 = seq[len-1] * (int)com_mul;
    if(arith) {
      if(append1 != append2) return ++result; 
    } else {
      return ++result; 
    }
  }
  return result; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); if ((Case == -1) || (Case == 6)) test_case_6(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arr0[] = {1, 2, 4, 8}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(0, Arg1, theCount(Arg0)); }
	void test_case_1() { int Arr0[] = {5, 3, 1}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(1, Arg1, theCount(Arg0)); }
	void test_case_2() { int Arr0[] = {1, 1}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(2, Arg1, theCount(Arg0)); }
	void test_case_3() { int Arr0[] = {8, 4}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(3, Arg1, theCount(Arg0)); }
	void test_case_4() { int Arr0[] = {1}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = -1; verify_case(4, Arg1, theCount(Arg0)); }
	void test_case_5() { int Arr0[] = {4, 8}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 2; verify_case(5, Arg1, theCount(Arg0)); }
	void test_case_6() { int Arr0[] = {1, 3, 4}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 0; verify_case(6, Arg1, theCount(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    FoxProgression ___test;
    ___test.run_test(-1);
}
// END CUT HERE
