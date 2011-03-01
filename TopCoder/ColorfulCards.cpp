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
 
class ColorfulCards {
public:
  vector <int> theCards(int N, string colors) {
    vector<int> val_guess; 

    string colormap; 
    for(int i = 1; i <= N; i++) {
      if(isPrime(i)) {		 
	colormap += 'R'; 
      } else {			
	colormap += 'B'; 
      }
    }

    int curval = 0;			 
    int input_len = colors.size(); 
    for(int j = 0; j < input_len; j++) { 
      for(int i = curval; i < N; i++) {	 
	curval = i;
	if(colors[j] == colormap[i]) {
	  val_guess.push_back(curval+1);
	  curval++;
	  break;  
	}
      }	// for colormap. 
    }	// for colors. 
    
    // Now inversely test the match.
    curval = N-1; 
    for(int j = input_len - 1; j >= 0; j--) { // match string char to char; 
      for(int i = curval; i >= 0; i--) {	 // test the match over the map.
	curval = i;
	if(colors[j] == colormap[i]) {
	  if(val_guess[j] != curval + 1) {
	    val_guess[j] = -1;
	  }
	  curval--;
	  break;  
	}
      }	// for colormap. 
    }	// for colors.
    return val_guess; 
  }
  
  // A program to test if a number is prime number. 
  bool isPrime(int val) {
    if(1 == val) return false; 
    for(int i = 2; i <= sqrt(val); i++) {
      if(0 == val % i) {
	return false;
      }
    }
    return true; 
  }

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const vector <int> &Expected, const vector <int> &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: " << print_array(Expected) << endl; cerr << "\tReceived: " << print_array(Received) << endl; } }
	void test_case_0() { int Arg0 = 5; string Arg1 = "RRR"; int Arr2[] = {2, 3, 5 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(0, Arg2, theCards(Arg0, Arg1)); }
	void test_case_1() { int Arg0 = 7; string Arg1 = "BBB"; int Arr2[] = {1, 4, 6 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(1, Arg2, theCards(Arg0, Arg1)); }
	void test_case_2() { int Arg0 = 6; string Arg1 = "RBR"; int Arr2[] = {-1, 4, 5 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(2, Arg2, theCards(Arg0, Arg1)); }
	void test_case_3() { int Arg0 = 58; string Arg1 = "RBRRBRBBRBRRBBRRBBBRRBBBRR"; int Arr2[] = {-1, -1, -1, -1, -1, -1, -1, -1, 17, 18, 19, 23, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 47, 53 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(3, Arg2, theCards(Arg0, Arg1)); }
	void test_case_4() { int Arg0 = 495; string Arg1 = "RBRRBRBBRBRRBBRRBBBRRBBBRR"; int Arr2[] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }; vector <int> Arg2(Arr2, Arr2 + (sizeof(Arr2) / sizeof(Arr2[0]))); verify_case(4, Arg2, theCards(Arg0, Arg1)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    ColorfulCards ___test;
    ___test.run_test(-1);
}
// END CUT HERE
